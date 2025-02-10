module Share.BackgroundJobs.EntityDepthMigration.Worker (worker) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.EntityDepthMigration.Queries qualified as Q
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import UnliftIO.Concurrent qualified as UnliftIO

pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "migration:entity_depth" $ forever do
    -- Do the components first, they're the bottom of the dependency tree.
    computeComponentDepths authZReceipt
    -- Then do the patches, they depend on components.
    computePatchDepths authZReceipt
    -- Then do the namespaces and causals together
    computeNamespaceAndCausalDepths authZReceipt
    liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000

-- | Components must be handled separately since they're sandboxed to specific users.
-- NOTE: this process doesn't insert the row into serialized_components, you'll need to do that manually after the automated migration is finished.
computeComponentDepths :: AuthZ.AuthZReceipt -> Background ()
computeComponentDepths !_authZReceipt = do
  PG.runTransaction Q.updateComponentDepths >>= \case
    0 -> do
      Logging.logInfoText $ "Done processing component depth"
      pure ()
    -- Recurse until there's nothing left to do.
    n -> do
      Logging.logInfoText $ "Computed Depth for " <> tShow n <> " components"
      computeComponentDepths _authZReceipt

computePatchDepths :: AuthZ.AuthZReceipt -> Background ()
computePatchDepths !_authZReceipt = do
  PG.runTransaction Q.updatePatchDepths >>= \case
    0 -> do
      Logging.logInfoText $ "Done processing patch depth"
      pure ()
    -- Recurse until there's nothing left to do.
    n -> do
      Logging.logInfoText $ "Computed Depth for " <> tShow n <> " patches"
      computePatchDepths _authZReceipt

computeNamespaceAndCausalDepths :: AuthZ.AuthZReceipt -> Background ()
computeNamespaceAndCausalDepths !authZReceipt = do
  PG.runTransaction Q.updateNamespaceDepths >>= \case
    namespaceN -> do
      PG.runTransaction Q.updateCausalDepths >>= \case
        causalN -> do
          case (namespaceN, causalN) of
            (0, 0) -> do
              Logging.logInfoText $ "Done processing namespace and causal depth"
              pure ()
            (namespaceN, causalN) -> do
              Logging.logInfoText $ "Computed Depth for " <> tShow namespaceN <> " namespaces and " <> tShow causalN <> " causals"
              computeNamespaceAndCausalDepths authZReceipt
