{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncStream.Impl (server) where

import Conduit
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBMQueue qualified as STM
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Except (runExceptT)
import Data.Conduit.Combinators qualified as Conduit
import Servant
import Servant.Conduit (ConduitToSourceIO (..))
import Servant.Types.SourceT qualified as SourceT
import Share.Codebase qualified as Codebase
import Share.IDs (ProjectBranchShortHand (..), ProjectReleaseShortHand (..), ProjectShortHand (..), UserHandle, UserId)
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Cursors qualified as Cursor
import Share.Postgres.Queries qualified as PGQ
import Share.Prelude
import Share.Project (Project (..))
import Share.User (User (..))
import Share.Utils.Unison (hash32ToCausalHash)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.SyncStream.Queries qualified as SSQ
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWTClaims (..))
import Unison.SyncV2.API qualified as SyncV2
import Unison.SyncV2.Types (DownloadEntitiesChunk (..))
import Unison.SyncV2.Types qualified as SyncV2
import UnliftIO qualified
import UnliftIO.Async qualified as Async

server :: Maybe UserId -> SyncV2.Routes WebAppServer
server mayUserId =
  SyncV2.Routes
    { downloadEntitiesStream = downloadEntitiesStreamImpl mayUserId
    }

parseBranchRef :: SyncV2.BranchRef -> Either Text (Either ProjectReleaseShortHand ProjectBranchShortHand)
parseBranchRef (SyncV2.BranchRef branchRef) =
  case parseRelease <|> parseBranch of
    Just a -> Right a
    Nothing -> Left $ "Invalid repo info: " <> branchRef
  where
    parseBranch :: Maybe (Either ProjectReleaseShortHand ProjectBranchShortHand)
    parseBranch = fmap Right . eitherToMaybe $ IDs.fromText @ProjectBranchShortHand branchRef
    parseRelease :: Maybe (Either ProjectReleaseShortHand ProjectBranchShortHand)
    parseRelease = fmap Left . eitherToMaybe $ IDs.fromText @ProjectReleaseShortHand branchRef

downloadEntitiesStreamImpl :: Maybe UserId -> SyncV2.DownloadEntitiesRequest -> WebApp (SourceIO SyncV2.DownloadEntitiesChunk)
downloadEntitiesStreamImpl mayCallerUserId (SyncV2.DownloadEntitiesRequest {causalHash = causalHashJWT, branchRef}) = do
  either emitErr id <$> runExceptT do
    addRequestTag "branch-ref" (SyncV2.unBranchRef branchRef)
    HashJWTClaims {hash = causalHash} <- lift (HashJWT.verifyHashJWT mayCallerUserId causalHashJWT >>= either respondError pure)
    codebase <-
      case parseBranchRef branchRef of
        Left err -> throwError (SyncV2.DownloadEntitiesInvalidBranchRef err branchRef)
        Right (Left (ProjectReleaseShortHand {userHandle, projectSlug})) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
            project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (SyncV2.DownloadEntitiesProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            pure (project, Nothing)
          authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (SyncV2.DownloadEntitiesNoReadPermission branchRef)
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
        Right (Right (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle})) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
            project <- (PGQ.projectByShortHand projectShortHand) `whenNothingM` throwError (SyncV2.DownloadEntitiesProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            mayContributorUserId <- for contributorHandle \ch -> fmap user_id $ (PGQ.userByHandle ch) `whenNothingM` throwError (SyncV2.DownloadEntitiesUserNotFound $ IDs.toText @UserHandle ch)
            pure (project, mayContributorUserId)
          authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (SyncV2.DownloadEntitiesNoReadPermission branchRef)
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
    q <- liftIO $ STM.newTBMQueueIO 10
    streamResults <- lift $ UnliftIO.toIO do
      Debug.debugLogM Debug.Temp "Starting source Stream"
      Codebase.runCodebaseTransaction codebase $ do
        (_bhId, causalId) <- CausalQ.expectCausalIdsOf id (hash32ToCausalHash causalHash)
        cursor <- SSQ.allSerializedDependenciesOfCausalCursor causalId
        Cursor.foldBatched cursor 1000 \batch -> do
          Debug.debugLogM Debug.Temp "Source stream batch"
          PG.transactionUnsafeIO $ STM.atomically $ STM.writeTBMQueue q batch
        PG.transactionUnsafeIO $ STM.atomically $ STM.closeTBMQueue q
    pure $ conduitToSourceIO do
      handle <- liftIO $ Async.async streamResults
      stream q handle
        Conduit..| ( Conduit.iterM \case
                       EntityChunk {hash} -> Debug.debugM Debug.Temp "Chunk " hash
                       ErrorChunk err -> Debug.debugM Debug.Temp "Error " err
                   )
  where
    stream :: STM.TBMQueue (NonEmpty (SyncV2.CBORBytes TempEntity, Hash32)) -> (Async.Async a) -> ConduitT () DownloadEntitiesChunk IO ()
    stream q handle = do
      let loop :: ConduitT () DownloadEntitiesChunk IO ()
          loop = do
            Debug.debugLogM Debug.Temp "Waiting for batch..."
            liftIO (STM.atomically (STM.readTBMQueue q)) >>= \case
              -- The queue is closed.
              Nothing -> do
                Debug.debugLogM Debug.Temp "Queue closed. finishing up!"
                pure ()
              Just batch -> do
                let chunks = batch <&> \(entityCBOR, hash) -> EntityChunk {hash, entityCBOR}
                Debug.debugLogM Debug.Temp $ "Emitting chunk of " <> show (length chunks) <> " entities"
                yieldMany chunks
                loop
      loop
      Debug.debugLogM Debug.Temp "Waiting for worker thread to finish"
      -- It _should_ have terminated by now, but just in case, cancel it.
      Async.cancel handle
      Debug.debugLogM Debug.Temp "Done!"

    emitErr :: SyncV2.DownloadEntitiesError -> SourceIO SyncV2.DownloadEntitiesChunk
    emitErr err = SourceT.source [ErrorChunk err]
