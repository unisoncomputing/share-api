module Share.Web.Share.Diffs.Impl
  ( computeAndStoreCausalDiff,
    diffTerms,
    diffTypes,
  )
where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Share.Codebase qualified as Codebase
import Share.NamespaceDiffs (DefinitionDiffKind (..), GNamespaceTreeDiff, NamespaceDiffError (..))
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHash, BranchHashId, CausalId)
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NamesPerspective.Ops qualified as NLOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective (..))
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Aeson (PreEncoded (PreEncoded))
import Share.Utils.Lens (asListOfDeduped)
import Share.Web.Authorization (AuthZReceipt)
import Share.Web.Errors
import U.Codebase.Reference qualified as V2Reference
import Unison.Codebase.SqliteCodebase.Conversions (referent1to2)
import Unison.ConstructorReference (ConstructorReference)
import Unison.Merge (TwoOrThreeWay (..), TwoWay (..))
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Referent qualified as Referent
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.NameSearch.Postgres qualified as PGNameSearch
import Unison.Server.Share.Definitions qualified as Definitions
import Unison.Server.Types
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)

-- | Diff two causals and store the diff in the database.
computeAndStoreCausalDiff ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, CausalId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, CausalId) ->
  Maybe CausalId ->
  PG.Transaction e (PreEncoded NamespaceDiffs.NamespaceDiffResult)
computeAndStoreCausalDiff authZReceipt old@(oldCodebase, _, oldCausalId) new@(newCodebase, _, newCausalId) lca = PG.transactionSpan "computeAndStoreCausalDiff" mempty do
  result <-
    PG.catchTransaction (tryComputeCausalDiff authZReceipt old new lca) <&> \case
      Right diff -> NamespaceDiffs.NamespaceDiffResult'Ok diff
      Left err -> NamespaceDiffs.NamespaceDiffResult'Err err
  let encoded = Aeson.encode result
  ContributionQ.savePrecomputedNamespaceDiff
    (oldCodebase, oldCausalId)
    (newCodebase, newCausalId)
    (TL.toStrict $ TL.decodeUtf8 encoded)
  pure (PreEncoded encoded)

tryComputeCausalDiff ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, CausalId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, CausalId) ->
  Maybe CausalId ->
  PG.Transaction
    NamespaceDiffError
    ( NamespaceDiffs.NamespaceAndLibdepsDiff
        (TermTag, ShortHash)
        (TypeTag, ShortHash)
        TermDefinition
        TypeDefinition
        TermDefinitionDiff
        TypeDefinitionDiff
        BranchHash
    )
tryComputeCausalDiff !authZReceipt (oldCodebase, oldRuntime, oldCausalId) (newCodebase, newRuntime, newCausalId) maybeLcaCausalId = PG.transactionSpan "tryComputeCausalDiff" mempty do
  -- Ensure name lookups for the things we're diffing.
  let getBranch :: CausalId -> PG.Transaction NamespaceDiffError (BranchHashId, NameLookupReceipt, NamesPerspective (PG.Transaction NamespaceDiffError))
      getBranch causalId = do
        branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np@NamesPerspective {nameLookupReceipt} <- NLOps.namesPerspectiveForRoot branchHashId
        pure (branchHashId, nameLookupReceipt, np)
  (oldBranchHashId, oldBranchNLReceipt, oldPerspective) <- PG.transactionSpan "getOldBranch" mempty $ getBranch oldCausalId
  (newBranchHashId, newBranchNLReceipt, newPerspective) <- PG.transactionSpan "getNewBranch" mempty $ getBranch newCausalId
  (maybeLcaBranchHashId, maybeLcaBranchNLReceipt, maybeLcaPerspective) <-
    case maybeLcaCausalId of
      Just lcaCausalId -> do
        (lcaBranchHashId, lcaBranchNLReceipt, lcaPerspective) <- PG.transactionSpan "getNewBranch" mempty $ getBranch lcaCausalId
        pure (Just lcaBranchHashId, Just lcaBranchNLReceipt, Just lcaPerspective)
      Nothing -> pure (Nothing, Nothing, Nothing)
  -- Do the initial 3-way namespace diff
  diff0 <-
    NamespaceDiffs.computeThreeWayNamespaceDiff
      TwoWay {alice = oldCodebase, bob = newCodebase}
      TwoOrThreeWay {alice = oldBranchHashId, bob = newBranchHashId, lca = maybeLcaBranchHashId}
      TwoOrThreeWay {alice = oldBranchNLReceipt, bob = newBranchNLReceipt, lca = maybeLcaBranchNLReceipt}
  -- Resolve the term referents to tag + hash
  diff1 <- PG.transactionSpan "hydrate-diff1" mempty $ do
    diff0
      & asListOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferents_)
        %%~ \refs -> do
          termTags <- Codebase.termTagsByReferentsOf traversed (referent1to2 <$> refs)
          pure $ zip termTags (refs <&> Referent.toShortHash)
  -- Resolve the type references to tag + hash
  diff2 <-
    PG.transactionSpan "hydrate-diff2" mempty $
      diff1
        & asListOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferences_)
          %%~ \refs -> do
            typeTags <- Codebase.typeTagsByReferencesOf traversed refs
            pure $ zip typeTags (refs <&> V2Reference.toShortHash)
  -- Resolve libdeps branch hash ids to branch hashes
  diff3 <-
    PG.transactionSpan "hydrate-diff3" mempty $
      HashQ.expectNamespaceHashesByNamespaceHashIdsOf
        (NamespaceDiffs.namespaceAndLibdepsDiffLibdeps_ . traversed . traversed)
        diff2
  -- Resolve the actual term/type definitions. Use the LCA as the "old" (because that's what we're rendering the
  -- diff relative to, unless there isn't an LCA (unlikely), in which case we fall back on the other branch (we
  -- won't have anything classified as an "update" in this case so it doesn't really matter).
  diff4 <-
    PG.transactionSpan "hydrate-diff4" mempty $
      diff3
        & NamespaceDiffs.namespaceAndLibdepsDiffDefns_
          %%~ computeUpdatedDefinitionDiffs
            authZReceipt
            (oldCodebase, oldRuntime, fromMaybe oldPerspective maybeLcaPerspective)
            (newCodebase, newRuntime, newPerspective)
  pure diff4

computeUpdatedDefinitionDiffs ::
  forall a b from to.
  (Ord a, Ord b) =>
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, NamesPerspective (PG.Transaction NamespaceDiffError)) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, NamesPerspective (PG.Transaction NamespaceDiffError)) ->
  GNamespaceTreeDiff NameSegment a b Name Name Name Name ->
  PG.Transaction
    NamespaceDiffError
    (NamespaceDiffs.NamespaceTreeDiff a b TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs !authZReceipt (fromCodebase, fromRuntime, fromPerspective) (toCodebase, toRuntime, toPerspective) diff0 = PG.transactionSpan "computeUpdatedDefinitionDiffs" mempty $ do
  diff1 <- PG.transactionSpan "termDiffs" mempty $ do
    NamespaceDiffs.witherNamespaceTreeDiffTermDiffs
      (\name -> diffTerms authZReceipt (fromCodebase, fromRuntime, fromPerspective, name) (toCodebase, toRuntime, toPerspective, name))
      diff0
  diff2 <- PG.transactionSpan "termDiffKinds" mempty $ do
    NamespaceDiffs.witherNamespaceTreeTermDiffKinds
      -- TODO: Batchify this properly
      (fmap throwAwayConstructorDiffs . renderDiffKind getTermDefinition)
      diff1
  diff3 <- PG.transactionSpan "typeDiffs" mempty $ do
    NamespaceDiffs.namespaceTreeDiffTypeDiffs_
      (\name -> diffTypes authZReceipt (fromCodebase, fromRuntime, fromPerspective, name) (toCodebase, toRuntime, toPerspective, name))
      diff2
  diff4 <- PG.transactionSpan "typeDiffKinds" mempty $ do
    NamespaceDiffs.namespaceTreeTypeDiffKinds_
      (renderDiffKind getTypeDefinition)
      diff3
  pure (NamespaceDiffs.compressNameTree diff4)
  where
    notFound name t = MissingEntityError $ EntityMissing (ErrorID "definition-not-found") (t <> ": Definition not found: " <> Name.toText name)

    renderDiffKind ::
      forall diff r x.
      (forall s. (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) -> PG.Transaction NamespaceDiffError (Maybe x)) ->
      DefinitionDiffKind r Name diff ->
      PG.Transaction NamespaceDiffError (DefinitionDiffKind r x diff)
    renderDiffKind getter = \case
      Added r name -> Added r <$> (getter (toCodebase, toRuntime, toPerspective, name) `whenNothingM` throwError (notFound name "Added"))
      NewAlias r existingNames name -> NewAlias r existingNames <$> (getter (toCodebase, toRuntime, toPerspective, name) `whenNothingM` throwError (notFound name "NewAlias"))
      Removed r name -> Removed r <$> (getter (fromCodebase, fromRuntime, fromPerspective, name) `whenNothingM` throwError (notFound name "Removed"))
      Updated oldRef newRef diff -> pure $ Updated oldRef newRef diff
      Propagated oldRef newRef diff -> pure $ Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names <$> (getter (fromCodebase, fromRuntime, fromPerspective, name) `whenNothingM` throwError (notFound name "RenamedTo"))
      RenamedFrom r names name -> RenamedFrom r names <$> (getter (toCodebase, toRuntime, toPerspective, name) `whenNothingM` throwError (notFound name "RenamedFrom"))

    throwAwayConstructorDiffs ::
      DefinitionDiffKind a (Either ConstructorReference TermDefinition) diff -> Maybe (DefinitionDiffKind a TermDefinition diff)
    throwAwayConstructorDiffs = \case
      Added ref (Right term) -> Just (Added ref term)
      NewAlias ref names (Right term) -> Just (NewAlias ref names term)
      Removed ref (Right term) -> Just (Removed ref term)
      Updated old new diff -> Just (Updated old new diff)
      Propagated old new diff -> Just (Propagated old new diff)
      RenamedTo ref names (Right term) -> Just (RenamedTo ref names term)
      RenamedFrom ref names (Right term) -> Just (RenamedFrom ref names term)
      --
      Added _ (Left _) -> Nothing
      NewAlias _ _ (Left _) -> Nothing
      Removed _ (Left _) -> Nothing
      RenamedFrom _ _ (Left _) -> Nothing
      RenamedTo _ _ (Left _) -> Nothing

diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError (Maybe TermDefinitionDiff)
diffTerms !_authZReceipt old@(_, _, _, oldName) new@(_, _, _, newName) = do
  oldTerm <- getTermDefinition old `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
  newTerm <- getTermDefinition new `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
  case (oldTerm, newTerm) of
    (Right oldTerm, Right newTerm) -> do
      let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
      pure (Just TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject})
    -- For later: decide how to render a constructor-to-term or constructor-to-constructor diff
    -- Just dropping them from the diff for now
    _ -> pure Nothing

-- | Get the term definition for a given name. Note: This assumes all names are within the
-- same names perspective.
--
-- TODO: batchify this
getTermDefinition :: (PG.QueryM m) => (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, NamesPerspective m, Name) -> m (Maybe (Either ConstructorReference TermDefinition))
getTermDefinition (codebase, rt, namesPerspective, name) = do
  let ppedBuilder deps = PPED.biasTo [name] <$> PPEPostgres.ppedForReferences namesPerspective deps
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Definitions.termDefinitionByName codebase ppedBuilder nameSearch renderWidth rt name
  where
    renderWidth :: Width
    renderWidth = 80

getTermDefinitionsOf :: Codebase.CodebaseEnv -> Codebase.CodebaseRuntime s IO -> BranchHashId -> Traversal s t Name (Maybe (Either ConstructorReference TermDefinition)) -> s -> m t
getTermDefinitionsOf codebase rt bhId trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      let perspective = mempty
      -- TODO: batchify this
      (namesPerspective, Identity relocatedName) <- for names \name -> NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      Definitions.termDefinitionByNamesOf codebase ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError TypeDefinitionDiff
diffTypes !_authZReceipt old@(_, _, _, oldTypeName) new@(_, _, _, newTypeName) = do
  oldType <-
    getTypeDefinition old
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  newType <-
    getTypeDefinition new
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition oldType) (typeDefinition newType)
  pure $ TypeDefinitionDiff {left = oldType, right = newType, diff = typeDiffDisplayObject}

-- | Get the type definition for a given name. Note: This assumes all names are within the
-- same names perspective.
--
-- TODO: batchify this
getTypeDefinition :: (PG.QueryM m) => (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, NamesPerspective m, Name) -> m (Maybe TypeDefinition)
getTypeDefinition (codebase, rt, namesPerspective, name) = do
  let ppedBuilder deps = (PPED.biasTo [name]) <$> (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Definitions.typeDefinitionByName codebase ppedBuilder nameSearch renderWidth rt name
  where
    renderWidth :: Width
    renderWidth = 80
