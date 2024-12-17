{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV2.Impl (server) where

import Conduit
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBMQueue qualified as STM
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Except (runExceptT)
import Data.List.NonEmpty qualified as NEL
import Servant
import Servant.Conduit (ConduitToSourceIO (..))
import Servant.Types.SourceT (SourceT (..))
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
import Share.Utils.Logging qualified as Logging
import Share.Utils.Unison (hash32ToCausalHash)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.SyncV2.Queries qualified as SSQ
import U.Codebase.Sqlite.Orphans ()
import Unison.Debug qualified as Debug
import Unison.Share.API.Hash (HashJWTClaims (..))
import Unison.SyncV2.API qualified as SyncV2
import Unison.SyncV2.Types (DownloadEntitiesChunk (..), EntityChunk (..), ErrorChunk (..), StreamInitInfo (..))
import Unison.SyncV2.Types qualified as SyncV2
import UnliftIO qualified
import UnliftIO.Async qualified as Async

batchSize :: Int32
batchSize = 1000

streamSettings :: StreamInitInfo
streamSettings = StreamInitInfo {version = SyncV2.Version 1, entitySorting = SyncV2.Unsorted, numEntities = Nothing}

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
downloadEntitiesStreamImpl mayCallerUserId (SyncV2.DownloadEntitiesRequest {causalHash = causalHashJWT, branchRef, knownHashes = _todo}) = do
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
    q <- UnliftIO.atomically $ do
      q <- STM.newTBMQueue 10
      STM.writeTBMQueue q (NEL.singleton $ InitialC $ streamSettings)
      pure q
    streamResults <- lift $ UnliftIO.toIO do
      Logging.logInfoText "Starting download entities stream"
      Codebase.runCodebaseTransaction codebase $ do
        Debug.debugM Debug.Temp "Getting IDs for:" causalHash
        (_bhId, causalId) <- CausalQ.expectCausalIdsOf id (hash32ToCausalHash causalHash)
        Debug.debugM Debug.Temp "Getting deps of" causalId
        cursor <- SSQ.allSerializedDependenciesOfCausalCursor causalId
        Debug.debugLogM Debug.Temp "Got cursor"
        Cursor.foldBatched cursor batchSize \batch -> do
          Debug.debugLogM Debug.Temp "Emitting batch"
          let entityChunkBatch = batch <&> \(entityCBOR, hash) -> EntityC (EntityChunk {hash, entityCBOR})
          PG.transactionUnsafeIO $ STM.atomically $ STM.writeTBMQueue q entityChunkBatch
        PG.transactionUnsafeIO $ STM.atomically $ STM.closeTBMQueue q
    liftIO $ Async.async streamResults
    -- pure $ sourceIOWithAsync streamResults $ conduitToSourceIO do
    pure $ conduitToSourceIO do
      stream q
  where
    -- Conduit..| ( Conduit.iterM \case
    --                InitialC init -> Debug.debugM Debug.Temp "Initial " init
    --                EntityC ec -> Debug.debugM Debug.Temp "Chunk " ec
    --                ErrorC err -> Debug.debugM Debug.Temp "Error " err
    --            )

    stream :: STM.TBMQueue (NonEmpty DownloadEntitiesChunk) -> ConduitT () DownloadEntitiesChunk IO ()
    stream q = do
      let loop :: ConduitT () DownloadEntitiesChunk IO ()
          loop = do
            Debug.debugLogM Debug.Temp "Waiting for batch..."
            liftIO (STM.atomically (STM.readTBMQueue q)) >>= \case
              -- The queue is closed.
              Nothing -> do
                Debug.debugLogM Debug.Temp "Queue closed. finishing up!"
                pure ()
              Just batch -> do
                Debug.debugLogM Debug.Temp $ "Emitting chunk of " <> show (length batch) <> " entities"
                yieldMany batch
                loop

      loop
      Debug.debugLogM Debug.Temp "Done!"

    emitErr :: SyncV2.DownloadEntitiesError -> SourceIO SyncV2.DownloadEntitiesChunk
    emitErr err = SourceT.source [ErrorC (ErrorChunk err)]

-- | Run an IO action in the background while streaming the results.
_sourceIOWithAsync :: IO a -> SourceIO r -> SourceIO r
_sourceIOWithAsync action (SourceT k) =
  SourceT \k' ->
    Async.withAsync action \_ -> k k'
