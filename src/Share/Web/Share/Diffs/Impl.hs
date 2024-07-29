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
import Unison.Server.Types (DisplayObjectDiff, TermDefinition (..), TermTag, TypeDefinition (..), TypeTag)
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)

diffNamespaces ::
  AuthZReceipt ->
  (BranchHashId, NameLookupReceipt) ->
  (BranchHashId, NameLookupReceipt) ->
  WebApp (NamespaceDiffs.NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash))
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
  CausalId ->
  CausalId ->
  WebApp (NamespaceDiffs.NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash))
diffCausals !_authZReceipt oldCausalId newCausalId = do
  -- Ensure name lookups for each thing we're diffing.
  -- We do this in two separate transactions to ensure we can still make progress even if we need to build name lookups.
  (oldBranchHashId, oldBranchNLReceipt) <- PG.runTransaction $ do
    oldBranchHashId <- CausalQ.expectNamespaceIdForCausal oldCausalId
    oldBranchNLReceipt <- NLOps.ensureNameLookupForBranchId oldBranchHashId
    pure (oldBranchHashId, oldBranchNLReceipt)

  (newBranchHashId, newNLReceipt) <- PG.runTransaction $ do
    newBranchHashId <- CausalQ.expectNamespaceIdForCausal newCausalId
    newNLReceipt <- NLOps.ensureNameLookupForBranchId newBranchHashId
    pure (newBranchHashId, newNLReceipt)

  PG.runTransactionOrRespondError $ do
    diff <- NamespaceDiffs.diffTreeNamespaces (oldBranchHashId, oldBranchNLReceipt) (newBranchHashId, newNLReceipt) `whenLeftM` throwError
    withTermTags <-
      ( diff
          & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferents_
            %%~ ( \refs -> do
                    termTags <- Codebase.termTagsByReferentsOf traversed refs
                    pure $ zip termTags (refs <&> V2Referent.toShortHash)
                )
        )
    diffWithTags <-
      withTermTags
        & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferences_
          %%~ ( \refs -> do
                  typeTags <- Codebase.typeTagsByReferencesOf traversed refs
                  pure $ zip typeTags (refs <&> V2Reference.toShortHash)
              )
    pure diffWithTags

diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  WebApp (TermDefinition, TermDefinition, DisplayObjectDiff)
diffTerms !_authZReceipt old@(_, _, oldName) new@(_, _, newName) =
  do
    oldTerm@(TermDefinition {termDefinition = oldDisplayObj}) <- getTermDefinition old `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
    newTerm@(TermDefinition {termDefinition = newDisplayObj}) <- getTermDefinition new `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
    let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects oldDisplayObj newDisplayObj
    pure $ (oldTerm, newTerm, termDiffDisplayObject)
  where
    renderWidth :: Width
    renderWidth = 80
    getTermDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> WebApp (Maybe TermDefinition)
    getTermDefinition (codebase, bhId, name) = do
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransaction $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransaction codebase do
        Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  WebApp (TypeDefinition, TypeDefinition, DisplayObjectDiff)
diffTypes !_authZReceipt old@(_, _, oldTypeName) new@(_, _, newTypeName) =
  do
    sourceType@(TypeDefinition {typeDefinition = sourceDisplayObj}) <- getTypeDefinition old `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
    newType@(TypeDefinition {typeDefinition = newDisplayObj}) <- getTypeDefinition new `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
    let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects sourceDisplayObj newDisplayObj
    pure $ (sourceType, newType, typeDiffDisplayObject)
  where
    renderWidth :: Width
    renderWidth = 80
    getTypeDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> WebApp (Maybe TypeDefinition)
    getTypeDefinition (codebase, bhId, name) = do
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransaction $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransaction codebase do
        Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
