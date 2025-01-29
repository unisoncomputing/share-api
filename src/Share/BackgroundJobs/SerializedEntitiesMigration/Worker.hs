module Share.BackgroundJobs.SerializedEntitiesMigration.Worker (worker) where

import Data.ByteString.Lazy qualified as BL
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.SerializedEntitiesMigration.Queries qualified as Q
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Postgres.Hashes.Queries qualified as HQ
import Share.Postgres.IDs
import Share.Postgres.Sync.Queries qualified as SQ
import Share.Prelude
import Share.Web.Authorization qualified as AuthZ
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Sync.Common qualified as SyncCommon
import Unison.SyncV2.Types qualified as SyncV2
import UnliftIO.Concurrent qualified as UnliftIO

pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "migration:serialised_components" $ forever do
    gotResult <- processComponents authZReceipt
    if gotResult
      then pure ()
      else liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000
  newWorker scope "migration:serialised_entities" $ forever do
    gotResult <- processEntities authZReceipt
    if gotResult
      then pure ()
      else liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000

processEntities :: AuthZ.AuthZReceipt -> Background Bool
processEntities !_authZReceipt = do
  mayHash <- PG.runTransaction do
    Q.claimEntity >>= \case
      Nothing -> pure Nothing
      Just (hash32, codebaseUserId) -> do
        let codebaseEnv = CodebaseEnv codebaseUserId
        Codebase.codebaseMToTransaction codebaseEnv $ do
          entity <- SQ.expectEntity hash32
          let tempEntity = SyncCommon.entityToTempEntity id entity
          saveUnsandboxedSerializedEntities hash32 tempEntity
        pure (Just hash32)
  case mayHash of
    Just _hash -> do
      pure True
    Nothing -> pure False

-- | Components must be handled separately since they're sandboxed to specific users.
-- NOTE: this process doesn't insert the row into serialized_components, you'll need to do that manually after the automated migration is finished.
processComponents :: AuthZ.AuthZReceipt -> Background Bool
processComponents !_authZReceipt = do
  PG.runTransaction do
    Q.claimComponent >>= \case
      Nothing -> pure False
      Just (componentHashId, userId) -> do
            let codebaseEnv = CodebaseEnv userId
            Codebase.codebaseMToTransaction codebaseEnv $ do
              hash32 <- (Hash32.fromHash . unComponentHash) <$> HQ.expectComponentHashesOf id componentHashId
              entity <- SQ.expectEntity hash32
              componentSummaryDigest <- HQ.expectComponentSummaryDigest componentHashId
              let tempEntity = SyncCommon.entityToTempEntity id entity
              let (SyncV2.CBORBytes bytes) = SyncV2.serialiseCBORBytes tempEntity
              bytesId <- DefnQ.ensureBytesIdsOf id (BL.toStrict bytes)
              execute_
                [sql|
                INSERT INTO component_summary_digests_to_serialized_component_bytes_hash (component_hash_id, component_summary_digest, serialized_component_bytes_id)
                  VALUES (#{componentHashId}, #{componentSummaryDigest}, #{bytesId})
                |]
              pure True

saveUnsandboxedSerializedEntities :: (QueryM m) => Hash32 -> TempEntity -> m ()
saveUnsandboxedSerializedEntities hash entity = do
  let serialised = SyncV2.serialiseCBORBytes entity
  case entity of
    Entity.TC {} -> error "Unexpected term component"
    Entity.DC {} -> error "Unexpected decl component"
    Entity.P {} -> SQ.saveSerializedPatch hash serialised
    Entity.C {} -> SQ.saveSerializedCausal hash serialised
    Entity.N {} -> SQ.saveSerializedNamespace hash serialised
