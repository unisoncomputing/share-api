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
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Aeson (PreEncoded (PreEncoded))
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
computeAndStoreCausalDiff authZReceipt old@(oldCodebase, _, oldCausalId) new@(newCodebase, _, newCausalId) lca = do
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
tryComputeCausalDiff !authZReceipt (oldCodebase, oldRuntime, oldCausalId) (newCodebase, newRuntime, newCausalId) maybeLcaCausalId = do
  -- Ensure name lookups for the things we're diffing.
  let getBranch :: CausalId -> PG.Transaction NamespaceDiffError (BranchHashId, NameLookupReceipt)
      getBranch causalId = do
        branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        nameLookupReceipt <- NLOps.ensureNameLookupForBranchId branchHashId
        pure (branchHashId, nameLookupReceipt)
  (oldBranchHashId, oldBranchNLReceipt) <- getBranch oldCausalId
  (newBranchHashId, newBranchNLReceipt) <- getBranch newCausalId
  (maybeLcaBranchHashId, maybeLcaBranchNLReceipt) <-
    case maybeLcaCausalId of
      Just lcaCausalId -> do
        (lcaBranchHashId, lcaBranchNLReceipt) <- getBranch lcaCausalId
        pure (Just lcaBranchHashId, Just lcaBranchNLReceipt)
      Nothing -> pure (Nothing, Nothing)
  -- Do the initial 3-way namespace diff
  diff0 <-
    NamespaceDiffs.computeThreeWayNamespaceDiff
      TwoWay {alice = oldCodebase, bob = newCodebase}
      TwoOrThreeWay {alice = oldBranchHashId, bob = newBranchHashId, lca = maybeLcaBranchHashId}
      TwoOrThreeWay {alice = oldBranchNLReceipt, bob = newBranchNLReceipt, lca = maybeLcaBranchNLReceipt}
  -- Resolve the term referents to tag + hash
  diff1 <-
    diff0
      & unsafePartsOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferents_)
        %%~ \refs -> do
          termTags <- Codebase.termTagsByReferentsOf (\f -> traverse (f . referent1to2)) refs
          pure $ zip termTags (refs <&> Referent.toShortHash)
  -- Resolve the type references to tag + hash
  diff2 <-
    diff1
      & unsafePartsOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferences_)
        %%~ \refs -> do
          typeTags <- Codebase.typeTagsByReferencesOf traversed refs
          pure $ zip typeTags (refs <&> V2Reference.toShortHash)
  -- Resolve libdeps branch hash ids to branch hashes
  diff3 <-
    HashQ.expectNamespaceHashesByNamespaceHashIdsOf
      (NamespaceDiffs.namespaceAndLibdepsDiffLibdeps_ . traversed . traversed)
      diff2
  -- Resolve the actual term/type definitions. Use the LCA as the "old" (because that's what we're rendering the
  -- diff relative to, unless there isn't an LCA (unlikely), in which case we fall back on the other branch (we
  -- won't have anything classified as an "update" in this case so it doesn't really matter).
  diff4 <-
    diff3
      & NamespaceDiffs.namespaceAndLibdepsDiffDefns_
        %%~ computeUpdatedDefinitionDiffs
          authZReceipt
          (oldCodebase, oldRuntime, fromMaybe oldBranchHashId maybeLcaBranchHashId)
          (newCodebase, newRuntime, newBranchHashId)
  pure diff4

computeUpdatedDefinitionDiffs ::
  forall a b from to.
  (Ord a, Ord b) =>
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, BranchHashId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, BranchHashId) ->
  GNamespaceTreeDiff NameSegment a b Name Name Name Name ->
  PG.Transaction
    NamespaceDiffError
    (NamespaceDiffs.NamespaceTreeDiff a b TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs !authZReceipt (fromCodebase, fromRuntime, fromBHId) (toCodebase, toRuntime, toBHId) diff0 = do
  diff1 <-
    NamespaceDiffs.witherNamespaceTreeDiffTermDiffs
      (\name -> diffTerms authZReceipt (fromCodebase, fromRuntime, fromBHId, name) (toCodebase, toRuntime, toBHId, name))
      diff0
  diff2 <-
    NamespaceDiffs.witherNamespaceTreeTermDiffKinds
      (fmap throwAwayConstructorDiffs . renderDiffKind getTermDefinition)
      diff1
  diff3 <-
    NamespaceDiffs.namespaceTreeDiffTypeDiffs_
      (\name -> diffTypes authZReceipt (fromCodebase, fromRuntime, fromBHId, name) (toCodebase, toRuntime, toBHId, name))
      diff2
  diff4 <-
    NamespaceDiffs.namespaceTreeTypeDiffKinds_
      (renderDiffKind getTypeDefinition)
      diff3
  pure (NamespaceDiffs.compressNameTree diff4)
  where
    notFound name t = MissingEntityError $ EntityMissing (ErrorID "definition-not-found") (t <> ": Definition not found: " <> Name.toText name)

    renderDiffKind ::
      forall diff r x.
      (forall s. (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, BranchHashId, Name) -> PG.Transaction NamespaceDiffError (Maybe x)) ->
      DefinitionDiffKind r Name diff ->
      PG.Transaction NamespaceDiffError (DefinitionDiffKind r x diff)
    renderDiffKind getter = \case
      Added r name -> Added r <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "Added"))
      NewAlias r existingNames name -> NewAlias r existingNames <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "NewAlias"))
      Removed r name -> Removed r <$> (getter (fromCodebase, fromRuntime, fromBHId, name) `whenNothingM` throwError (notFound name "Removed"))
      Updated oldRef newRef diff -> pure $ Updated oldRef newRef diff
      Propagated oldRef newRef diff -> pure $ Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names <$> (getter (fromCodebase, fromRuntime, fromBHId, name) `whenNothingM` throwError (notFound name "RenamedTo"))
      RenamedFrom r names name -> RenamedFrom r names <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "RenamedFrom"))

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
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, BranchHashId, Name) ->
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

getTermDefinition :: (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, BranchHashId, Name) -> PG.Transaction e (Maybe (Either ConstructorReference TermDefinition))
getTermDefinition (codebase, rt, bhId, name) = do
  let perspective = mempty
  (namesPerspective, Identity relocatedName) <- NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = PPED.biasTo [name] <$> PPEPostgres.ppedForReferences namesPerspective deps
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Codebase.codebaseMToTransaction codebase do
    Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, BranchHashId, Name) ->
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

getTypeDefinition :: (Codebase.CodebaseEnv, Codebase.CodebaseRuntime s IO, BranchHashId, Name) -> PG.Transaction e (Maybe TypeDefinition)
getTypeDefinition (codebase, rt, bhId, name) = do
  let perspective = mempty
  (namesPerspective, Identity relocatedName) <- NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Codebase.codebaseMToTransaction codebase do
    Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80
