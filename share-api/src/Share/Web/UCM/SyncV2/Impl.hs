{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV2.Impl (server) where

import Codec.Serialise qualified as CBOR
import Conduit qualified as C
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBMQueue qualified as STM
import Control.Monad.Except (withExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Binary.Builder qualified as Builder
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Ki.Unlifted qualified as Ki
import Servant
import Servant.Conduit (ConduitToSourceIO (..))
import Servant.Types.SourceT (SourceT (..))
import Servant.Types.SourceT qualified as SourceT
import Share.IDs (UserId)
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Cursors qualified as Cursor
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.Unison (hash32ToCausalHash)
import Share.Web.App
import Share.Web.Errors
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.SyncCommon.Impl
import Share.Web.UCM.SyncCommon.Types
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
      queueToStream q
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
    pure $ sourceIOWithAsync streamResults $ conduitToSourceIO do
      queueToStream q

queueToStream :: forall a f. (CBOR.Serialise a, Foldable f) => STM.TBMQueue (f a) -> C.ConduitT () (SyncV2.CBORStream a) IO ()
queueToStream q = do
  let loop :: C.ConduitT () (SyncV2.CBORStream a) IO ()
      loop = do
        liftIO (STM.atomically (STM.readTBMQueue q)) >>= \case
          -- The queue is closed.
          Nothing -> do
            pure ()
          Just batches -> do
            batches
              & foldMap (CBOR.serialiseIncremental)
              & (SyncV2.CBORStream . Builder.toLazyByteString)
              & C.yield
            loop
  loop

-- | Run an IO action in the background while streaming the results.
--
-- Servant doesn't provide any easier way to do bracketing like this, all the IO must be
-- inside the SourceIO somehow.
sourceIOWithAsync :: IO a -> SourceIO r -> SourceIO r
sourceIOWithAsync action (SourceT k) =
  SourceT \k' ->
    Ki.scoped \scope -> do
      _ <- Ki.fork scope action
      k k'
