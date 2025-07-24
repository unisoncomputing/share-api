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
import Unison.Referent qualified as Referent
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
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
tryComputeCausalDiff !_authZReceipt (oldCodebase, oldRuntime, oldCausalId) (newCodebase, newRuntime, newCausalId) maybeLcaCausalId = PG.transactionSpan "tryComputeCausalDiff" mempty do
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
            (oldCodebase, oldRuntime, fromMaybe oldPerspective maybeLcaPerspective)
            (newCodebase, newRuntime, newPerspective)
  pure diff4

computeUpdatedDefinitionDiffs ::
  forall a b from to.
  (Ord a, Ord b) =>
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, NamesPerspective (PG.Transaction NamespaceDiffError)) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, NamesPerspective (PG.Transaction NamespaceDiffError)) ->
  GNamespaceTreeDiff NameSegment a b Name Name Name Name ->
  PG.Transaction
    NamespaceDiffError
    (NamespaceDiffs.NamespaceTreeDiff a b TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs (fromCodebase, fromRuntime, fromPerspective) (toCodebase, toRuntime, toPerspective) diff0 = PG.transactionSpan "computeUpdatedDefinitionDiffs" mempty $ do
  diff1 <- PG.transactionSpan "termDiffs" mempty $ do
    diff0
      & NamespaceDiffs.namespaceTreeDiffTermDiffs_ %~ (\name -> (name, name))
      & expectTermDefinitionsOf fromCodebase fromRuntime fromPerspective (NamespaceDiffs.namespaceTreeDiffTermDiffs_ . _1)
      >>= expectTermDefinitionsOf toCodebase toRuntime toPerspective (NamespaceDiffs.namespaceTreeDiffTermDiffs_ . _2)
      >>= NamespaceDiffs.witherNamespaceTreeDiffTermDiffs (pure . diffTermsPure)

  diff2 <- PG.transactionSpan "termDiffKinds" mempty $ do
    diff1
      & NamespaceDiffs.namespaceTreeTermDiffKinds_ %~ partitionDiffKind
      & expectTermDefinitionsOf fromCodebase fromRuntime fromPerspective (NamespaceDiffs.namespaceTreeDiffRenderedTerms_ . _Left)
      >>= expectTermDefinitionsOf toCodebase toRuntime toPerspective (NamespaceDiffs.namespaceTreeDiffRenderedTerms_ . _Right)
      <&> NamespaceDiffs.namespaceTreeDiffRenderedTerms_ %~ either id id
      >>= NamespaceDiffs.witherNamespaceTreeTermDiffKinds (pure . throwAwayConstructorDiffs)

  diff3 <- PG.transactionSpan "typeDiffs" mempty $ do
    diff2
      & NamespaceDiffs.namespaceTreeDiffTypeDiffs_ %~ (\name -> (name, name))
      & expectTypeDefinitionsOf fromCodebase fromRuntime fromPerspective (NamespaceDiffs.namespaceTreeDiffTypeDiffs_ . _1)
      >>= expectTypeDefinitionsOf toCodebase toRuntime toPerspective (NamespaceDiffs.namespaceTreeDiffTypeDiffs_ . _2)
      <&> NamespaceDiffs.namespaceTreeDiffTypeDiffs_ %~ diffTypesPure
  diff4 <- PG.transactionSpan "typeDiffKinds" mempty $ do
    diff3
      & NamespaceDiffs.namespaceTreeTypeDiffKinds_ %~ partitionDiffKind
      & expectTypeDefinitionsOf fromCodebase fromRuntime fromPerspective (NamespaceDiffs.namespaceTreeDiffRenderedTypes_ . _Left)
      >>= expectTypeDefinitionsOf toCodebase fromRuntime toPerspective (NamespaceDiffs.namespaceTreeDiffRenderedTypes_ . _Right)
      <&> NamespaceDiffs.namespaceTreeDiffRenderedTypes_ %~ either id id
  pure (NamespaceDiffs.compressNameTree diff4)
  where
    -- Splits the diff kind into the parts from the 'from' and 'to' sections of a diff.
    --
    -- Left = From, Right = To
    partitionDiffKind :: DefinitionDiffKind r rendered diff -> DefinitionDiffKind r (Either rendered rendered) diff
    partitionDiffKind = \case
      Added r name -> Added r (Right name)
      NewAlias r existingNames name -> NewAlias r existingNames (Right name)
      Removed r name -> Removed r (Left name)
      Updated oldRef newRef diff -> Updated oldRef newRef diff
      Propagated oldRef newRef diff -> Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names (Left name)
      RenamedFrom r names name -> RenamedFrom r names (Right name)

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

-- | Note: Only use this if you're diffing a single definition, otherwise batch operations are
-- more efficient.
diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError (Maybe TermDefinitionDiff)
diffTerms !_authZReceipt (oldCodebase, oldRt, oldNp, oldName) (newCodebase, newRt, newNp, newName) = do
  oldTerm <- getTermDefinitionsOf oldCodebase oldRt oldNp id oldName `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
  newTerm <- getTermDefinitionsOf newCodebase newRt newNp id newName `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
  pure $ diffTermsPure (oldTerm, newTerm)

diffTermsPure ::
  (Either a2 TermDefinition, Either a3 TermDefinition) -> (Maybe TermDefinitionDiff)
diffTermsPure = \case
  (Right oldTerm, Right newTerm) ->
    let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
     in (Just TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject})
  -- For later: decide how to render a constructor-to-term or constructor-to-constructor diff
  -- Just dropping them from the diff for now
  _ -> Nothing

-- | Get definitions for a batch of terms within a codebase and perspective.
-- NOTE: The names should already be properly scoped to the names perspective.
getTermDefinitionsOf ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective m ->
  Traversal s t Name (Maybe (Either ConstructorReference TermDefinition)) ->
  s ->
  m t
getTermDefinitionsOf codebase rt namesPerspective trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      Definitions.termDefinitionByNamesOf codebase ppedBuilder namesPerspective renderWidth rt includeDocs traversed names
  where
    includeDocs = False
    ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
    renderWidth :: Width
    renderWidth = 80

expectTermDefinitionsOf ::
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective (PG.Transaction NamespaceDiffError) ->
  Traversal s t Name (Either ConstructorReference TermDefinition) ->
  s ->
  PG.Transaction NamespaceDiffError t
expectTermDefinitionsOf codebase rt np trav s =
  s
    & asListOf trav %%~ \names -> do
      results <- getTermDefinitionsOf codebase rt np traversed names
      for (zip names results) \case
        (name, Nothing) -> throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("Term not found: " <> Name.toText name <> ", in names perspective: " <> tShow np))
        (_, Just termDef) -> pure termDef

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError TypeDefinitionDiff
diffTypes !_authZReceipt (oldCodebase, oldRt, oldNp, oldTypeName) (newCodebase, newRt, newNp, newTypeName) = do
  oldType <-
    getTypeDefinitionsOf oldCodebase oldRt oldNp id oldTypeName
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  newType <-
    getTypeDefinitionsOf newCodebase newRt newNp id newTypeName
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  pure $ diffTypesPure (oldType, newType)

diffTypesPure ::
  (TypeDefinition, TypeDefinition) -> TypeDefinitionDiff
diffTypesPure (oldType, newType) = do
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition oldType) (typeDefinition newType)
   in TypeDefinitionDiff {left = oldType, right = newType, diff = typeDiffDisplayObject}

-- | Get definitions for a batch of types within a codebase and perspective.
-- NOTE: The names should already be properly scoped to the names perspective.
getTypeDefinitionsOf ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective m ->
  Traversal s t Name (Maybe TypeDefinition) ->
  s ->
  m t
getTypeDefinitionsOf codebase rt namesPerspective trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      Definitions.typeDefinitionsByNamesOf codebase ppedBuilder namesPerspective renderWidth rt includeDocs traversed names
  where
    includeDocs = False
    ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
    renderWidth :: Width
    renderWidth = 80

expectTypeDefinitionsOf ::
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective (PG.Transaction NamespaceDiffError) ->
  Traversal s t Name TypeDefinition ->
  s ->
  PG.Transaction NamespaceDiffError t
expectTypeDefinitionsOf codebase rt np trav s =
  s
    & asListOf trav %%~ \names -> do
      results <- getTypeDefinitionsOf codebase rt np traversed names
      for (zip names results) \case
        (name, Nothing) -> throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("Type not found: " <> Name.toText name <> ", in names perspective: " <> tShow np))
        (_, Just typeDef) -> pure typeDef
