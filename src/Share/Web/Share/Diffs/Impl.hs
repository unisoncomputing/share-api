module Share.Web.Share.Diffs.Impl
  ( diffNamespaces,
    diffCausals,
    diffTerms,
    diffTypes,
  )
where

import Control.Lens
import Control.Monad.Except
import Share.Codebase qualified as Codebase
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.Web.App
import Share.Web.Authorization (AuthZReceipt)
import Share.Web.Errors (EntityMissing (..), ErrorID (..), respondError)
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as V2Referent
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.NameSearch.Postgres qualified as PGNameSearch
import Unison.Server.Share.Definitions qualified as Definitions
import Unison.Server.Types (TermDefinition (..), TermDefinitionDiff (..), TermTag, TypeDefinition (..), TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)
import UnliftIO qualified

diffNamespaces ::
  AuthZReceipt ->
  (BranchHashId, NameLookupReceipt) ->
  (BranchHashId, NameLookupReceipt) ->
  WebApp (NamespaceDiffs.NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) Name Name)
diffNamespaces !_authZReceipt oldNamespacePair newNamespacePair = do
  PG.runTransactionOrRespondError $ do
    diff <- NamespaceDiffs.diffTreeNamespaces oldNamespacePair newNamespacePair `whenLeftM` throwError
    withTermTags <-
      ( diff
          & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferents_
            %%~ ( \refs -> do
                    termTags <- Codebase.termTagsByReferentsOf traversed refs
                    pure $ zip termTags (refs <&> V2Referent.toShortHash)
                )
        )
    withTermTags
      & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferences_
        %%~ ( \refs -> do
                typeTags <- Codebase.typeTagsByReferencesOf traversed refs
                pure $ zip typeTags (refs <&> V2Reference.toShortHash)
            )

-- | Find the common ancestor between two causals, then diff
diffCausals ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, CausalId) ->
  (Codebase.CodebaseEnv, CausalId) ->
  WebApp (Either Text (NamespaceDiffs.NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinitionDiff TypeDefinitionDiff))
diffCausals !authZReceipt (oldCodebase, oldCausalId) (newCodebase, newCausalId) = do
  -- Ensure name lookups for each thing we're diffing.
  -- We do this in two separate transactions to ensure we can still make progress even if we need to build name lookups.
  let getOldBranch = PG.runTransaction $ do
        oldBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id oldCausalId
        oldBranchNLReceipt <- NLOps.ensureNameLookupForBranchId oldBranchHashId
        pure (oldBranchHashId, oldBranchNLReceipt)

  let getNewBranch = PG.runTransaction $ do
        newBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id newCausalId
        newNLReceipt <- NLOps.ensureNameLookupForBranchId newBranchHashId
        pure (newBranchHashId, newNLReceipt)
  ((oldBranchHashId, oldBranchNLReceipt), (newBranchHashId, newNLReceipt)) <- getOldBranch `UnliftIO.concurrently` getNewBranch
  (PG.runTransaction $ ContributionQ.getPrecomputedNamespaceDiff (oldCodebase, oldBranchHashId) (newCodebase, newBranchHashId))
    >>= \case
      Just diff -> pure $ Left diff
      Nothing -> do
        diffWithTags <- PG.runTransactionOrRespondError $ do
          diff <- NamespaceDiffs.diffTreeNamespaces (oldBranchHashId, oldBranchNLReceipt) (newBranchHashId, newNLReceipt) `whenLeftM` throwError
          withTermTags <-
            ( diff
                & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferents_
                  %%~ ( \refs -> do
                          termTags <- Codebase.termTagsByReferentsOf traversed refs
                          pure $ zip termTags (refs <&> V2Referent.toShortHash)
                      )
              )
          withTermTags
            & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferences_
              %%~ ( \refs -> do
                      typeTags <- Codebase.typeTagsByReferencesOf traversed refs
                      pure $ zip typeTags (refs <&> V2Reference.toShortHash)
                  )
        Right <$> computeUpdatedDefinitionDiffs authZReceipt (oldCodebase, oldBranchHashId) (newCodebase, newBranchHashId) diffWithTags

computeUpdatedDefinitionDiffs ::
  (Ord a, Ord b) =>
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId) ->
  (Codebase.CodebaseEnv, BranchHashId) ->
  (NamespaceDiffs.NamespaceTreeDiff a b Name Name) ->
  WebApp (NamespaceDiffs.NamespaceTreeDiff a b TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs !authZReceipt (fromCodebase, fromBHId) (toCodebase, toBHId) diff = do
  withTermDiffs <-
    diff
      & NamespaceDiffs.namespaceTreeDiffTermDiffs_ %%~ \name ->
        diffTerms authZReceipt (fromCodebase, fromBHId, name) (toCodebase, toBHId, name)
  withTermDiffs
    & NamespaceDiffs.namespaceTreeDiffTypeDiffs_ %%~ \name ->
      diffTypes authZReceipt (fromCodebase, fromBHId, name) (toCodebase, toBHId, name)

diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  WebApp TermDefinitionDiff
diffTerms !_authZReceipt old@(_, _, oldName) new@(_, _, newName) = do
  let getOldTerm = getTermDefinition old `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
  let getNewTerm = getTermDefinition new `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
  (oldTerm, newTerm) <- getOldTerm `UnliftIO.concurrently` getNewTerm
  let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
  pure $ TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject}
  where
    renderWidth :: Width
    renderWidth = 80
    getTermDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> WebApp (Maybe TermDefinition)
    getTermDefinition (codebase, bhId, name) = do
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransactionMode PG.ReadCommitted PG.Read $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransactionMode PG.ReadCommitted codebase do
        Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  WebApp TypeDefinitionDiff
diffTypes !_authZReceipt old@(_, _, oldTypeName) new@(_, _, newTypeName) = do
  let getOldType =
        getTypeDefinition old
          `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  let getNewType =
        getTypeDefinition new
          `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  (sourceType, newType) <- getOldType `UnliftIO.concurrently` getNewType
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition sourceType) (typeDefinition newType)
  pure $ TypeDefinitionDiff {left = sourceType, right = newType, diff = typeDiffDisplayObject}
  where
    renderWidth :: Width
    renderWidth = 80
    getTypeDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> WebApp (Maybe TypeDefinition)
    getTypeDefinition (codebase, bhId, name) = do
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransactionMode PG.ReadCommitted PG.Read $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransactionMode PG.ReadCommitted codebase do
        Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
