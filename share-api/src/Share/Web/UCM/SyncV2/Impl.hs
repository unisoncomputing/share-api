{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV2.Impl (server) where

import Codec.Serialise qualified as CBOR
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBMQueue qualified as STM
import Control.Monad.Except (ExceptT (ExceptT), withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
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
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project (Project (..))
import Share.User (User (..))
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant.Streaming
import Share.Utils.Unison (hash32ToCausalHash)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.SyncV2.Queries qualified as SSQ
import Share.Web.UCM.SyncV2.Types (IsCausalSpine (..), IsLibRoot (..))
import U.Codebase.Sqlite.Orphans ()
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWTClaims (..))
import Unison.SyncV2.API qualified as SyncV2
import Unison.SyncV2.Types (CausalDependenciesChunk (..), DependencyType (..), DownloadEntitiesChunk (..), EntityChunk (..), ErrorChunk (..), StreamInitInfo (..))
import Unison.SyncV2.Types qualified as SyncV2
import UnliftIO qualified

batchSize :: Int32
batchSize = 1000

streamSettings :: Hash32 -> Maybe SyncV2.BranchRef -> StreamInitInfo
streamSettings rootCausalHash rootBranchRef = StreamInitInfo {version = SyncV2.Version 1, entitySorting = SyncV2.DependenciesFirst, numEntities = Nothing, rootCausalHash, rootBranchRef}

server :: Maybe UserId -> SyncV2.Routes WebAppServer
server mayUserId =
  SyncV2.Routes
    { downloadEntitiesStream = downloadEntitiesStreamImpl mayUserId,
      causalDependenciesStream = causalDependenciesStreamImpl mayUserId
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

downloadEntitiesStreamImpl :: Maybe UserId -> SyncV2.DownloadEntitiesRequest -> WebApp (SourceIO (SyncV2.CBORStream SyncV2.DownloadEntitiesChunk))
downloadEntitiesStreamImpl mayCallerUserId (SyncV2.DownloadEntitiesRequest {causalHash = causalHashJWT, branchRef, knownHashes}) = do
  either emitErr id <$> runExceptT do
    addRequestTag "branch-ref" (SyncV2.unBranchRef branchRef)
    HashJWTClaims {hash = causalHash} <- lift (HashJWT.verifyHashJWT mayCallerUserId causalHashJWT >>= either respondError pure)
    codebase <-
      flip withExceptT (codebaseForBranchRef branchRef) \case
        CodebaseLoadingErrorProjectNotFound projectShortHand -> SyncV2.DownloadEntitiesProjectNotFound (IDs.toText projectShortHand)
        CodebaseLoadingErrorUserNotFound userHandle -> SyncV2.DownloadEntitiesUserNotFound (IDs.toText userHandle)
        CodebaseLoadingErrorNoReadPermission branchRef -> SyncV2.DownloadEntitiesNoReadPermission branchRef
        CodebaseLoadingErrorInvalidBranchRef err branchRef -> SyncV2.DownloadEntitiesInvalidBranchRef err branchRef
    q <- UnliftIO.atomically $ do
      q <- STM.newTBMQueue 10
      STM.writeTBMQueue q (Vector.singleton $ InitialC $ streamSettings causalHash (Just branchRef))
      pure q
    streamResults <- lift $ UnliftIO.toIO do
      Logging.logInfoText "Starting download entities stream"
      PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
        (_bhId, causalId) <- CausalQ.expectCausalIdsOf codebase id (hash32ToCausalHash causalHash)
        let knownCausalHashes = Set.map hash32ToCausalHash knownHashes
        cursor <- SSQ.allSerializedDependenciesOfCausalCursor codebase causalId knownCausalHashes
        Cursor.foldBatched cursor batchSize \batch -> do
          let entityChunkBatch = batch <&> \(entityCBOR, hash) -> EntityC (EntityChunk {hash, entityCBOR})
          PG.transactionUnsafeIO $ STM.atomically $ STM.writeTBMQueue q entityChunkBatch
        PG.transactionUnsafeIO $ STM.atomically $ STM.closeTBMQueue q
    pure $ sourceIOWithAsync streamResults $ conduitToSourceIO do
      queueToCBORStream q
  where
    emitErr :: SyncV2.DownloadEntitiesError -> SourceIO (SyncV2.CBORStream SyncV2.DownloadEntitiesChunk)
    emitErr err = SourceT.source [SyncV2.CBORStream . CBOR.serialise $ ErrorC (ErrorChunk err)]

causalDependenciesStreamImpl :: Maybe UserId -> SyncV2.CausalDependenciesRequest -> WebApp (SourceIO (SyncV2.CBORStream SyncV2.CausalDependenciesChunk))
causalDependenciesStreamImpl mayCallerUserId (SyncV2.CausalDependenciesRequest {rootCausal = causalHashJWT, branchRef}) = do
  respondExceptT do
    addRequestTag "branch-ref" (SyncV2.unBranchRef branchRef)
    HashJWTClaims {hash = causalHash} <- lift (HashJWT.verifyHashJWT mayCallerUserId causalHashJWT >>= either respondError pure)
    addRequestTag "root-causal" (tShow causalHash)
    codebase <- codebaseForBranchRef branchRef
    q <- UnliftIO.atomically $ STM.newTBMQueue 10
    streamResults <- lift $ UnliftIO.toIO do
      Logging.logInfoText "Starting causal dependencies stream"
      PG.runTransactionMode PG.ReadCommitted PG.Read $ do
        (_bhId, causalId) <- CausalQ.expectCausalIdsOf codebase id (hash32ToCausalHash causalHash)
        cursor <- SSQ.spineAndLibDependenciesOfCausalCursor codebase causalId
        Cursor.foldBatched cursor batchSize \batch -> do
          let depBatch =
                batch <&> \(causalHash, isCausalSpine, isLibRoot) ->
                  let dependencyType = case (isCausalSpine, isLibRoot) of
                        (IsCausalSpine, _) -> CausalSpineDependency
                        (_, IsLibRoot) -> LibDependency
                        _ -> error $ "Causal dependency which is neither spine nor lib root: " <> show causalHash
                   in CausalHashDepC {causalHash, dependencyType}
          PG.transactionUnsafeIO $ STM.atomically $ STM.writeTBMQueue q depBatch
        PG.transactionUnsafeIO $ STM.atomically $ STM.closeTBMQueue q
    pure $ sourceIOWithAsync streamResults $ queueToSourceIO q

data CodebaseLoadingError
  = CodebaseLoadingErrorProjectNotFound ProjectShortHand
  | CodebaseLoadingErrorUserNotFound UserHandle
  | CodebaseLoadingErrorNoReadPermission SyncV2.BranchRef
  | CodebaseLoadingErrorInvalidBranchRef Text SyncV2.BranchRef
  deriving stock (Show)
  deriving (Logging.Loggable) via Logging.ShowLoggable Logging.UserFault CodebaseLoadingError

instance ToServerError CodebaseLoadingError where
  toServerError = \case
    CodebaseLoadingErrorProjectNotFound projectShortHand -> (ErrorID "codebase-loading:project-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "Project not found: " <> (IDs.toText projectShortHand)})
    CodebaseLoadingErrorUserNotFound userHandle -> (ErrorID "codebase-loading:user-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "User not found: " <> (IDs.toText userHandle)})
    CodebaseLoadingErrorNoReadPermission branchRef -> (ErrorID "codebase-loading:no-read-permission", Servant.err403 {errBody = from . Text.encodeUtf8 $ "No read permission for branch ref: " <> (SyncV2.unBranchRef branchRef)})
    CodebaseLoadingErrorInvalidBranchRef err branchRef -> (ErrorID "codebase-loading:invalid-branch-ref", Servant.err400 {errBody = from . Text.encodeUtf8 $ "Invalid branch ref: " <> err <> " " <> (SyncV2.unBranchRef branchRef)})

codebaseForBranchRef :: SyncV2.BranchRef -> (ExceptT CodebaseLoadingError WebApp Codebase.CodebaseEnv)
codebaseForBranchRef branchRef = do
  case parseBranchRef branchRef of
    Left err -> throwError (CodebaseLoadingErrorInvalidBranchRef err branchRef)
    Right (Left (ProjectReleaseShortHand {userHandle, projectSlug})) -> do
      let projectShortHand = ProjectShortHand {userHandle, projectSlug}
      (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
        project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (CodebaseLoadingErrorProjectNotFound $ projectShortHand)
        pure (project, Nothing)
      authZToken <- lift AuthZ.hashJWTAuthOverride `whenLeftM` \_err -> throwError (CodebaseLoadingErrorNoReadPermission branchRef)
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
      pure $ Codebase.codebaseEnv authZToken codebaseLoc
    Right (Right (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle})) -> do
      let projectShortHand = ProjectShortHand {userHandle, projectSlug}
      (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
        project <- (PGQ.projectByShortHand projectShortHand) `whenNothingM` throwError (CodebaseLoadingErrorProjectNotFound projectShortHand)
        mayContributorUserId <- for contributorHandle \ch -> fmap user_id $ (UserQ.userByHandle ch) `whenNothingM` throwError (CodebaseLoadingErrorUserNotFound ch)
        pure (project, mayContributorUserId)
      authZToken <- lift AuthZ.hashJWTAuthOverride `whenLeftM` \_err -> throwError (CodebaseLoadingErrorNoReadPermission branchRef)
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
      pure $ Codebase.codebaseEnv authZToken codebaseLoc
