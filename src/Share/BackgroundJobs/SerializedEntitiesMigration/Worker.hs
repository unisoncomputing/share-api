module Share.BackgroundJobs.SerializedEntitiesMigration.Worker (worker) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.SerializedEntitiesMigration.Queries qualified as Q
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Postgres qualified as PG
import Share.Postgres.Sync.Queries qualified as SQ
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Unison.Sync.Common qualified as SyncCommon
import UnliftIO.Concurrent qualified as UnliftIO

pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "migration:serialised_components" $ forever do
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
          SQ.saveSerializedEntities [(hash32, tempEntity)]
        pure (Just hash32)
  case mayHash of
    Just hash -> do
      Logging.logInfoText ("Migrated entity: " <> tShow hash)
      pure True
    Nothing -> pure False
