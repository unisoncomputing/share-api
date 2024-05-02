module Enlil.Web.Share.Diffs.Impl
  ( diffNamespaces,
    diffCausals,
  )
where

import Control.Lens
import Control.Monad.Except
import Enlil.Codebase qualified as Codebase
import Enlil.NamespaceDiffs qualified as NamespaceDiffs
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Causal.Queries qualified as CausalQ
import Enlil.Postgres.IDs (BranchHashId, CausalId)
import Enlil.Postgres.NameLookups.Ops qualified as NLOps
import Enlil.Postgres.NameLookups.Types (NameLookupReceipt)
import Enlil.Prelude
import Enlil.Web.App
import Enlil.Web.Authorization (AuthZReceipt)
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as V2Referent
import Unison.Server.Types (TermTag, TypeTag)
import Unison.ShortHash (ShortHash)

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
