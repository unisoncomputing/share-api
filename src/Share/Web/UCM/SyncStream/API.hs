{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncStream.API (API, server) where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Servant
import Servant.Conduit (ConduitToSourceIO (..))
import Share.Codebase qualified as Codebase
import Share.IDs (UserId)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Cursors qualified as Cursor
import Share.Postgres.IDs (CausalHash)
import Share.Prelude
import Share.Utils.Servant (RequiredQueryParam)
import Share.Utils.Servant.CBOR (CBOR)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.UCM.SyncStream.Queries qualified as SSQ
import UnliftIO qualified
import UnliftIO.Async qualified as Async

type API = "download-causal" :> DownloadCausalStreamEndpoint

type DownloadCausalStreamEndpoint =
  AuthenticatedUserId
    :> RequiredQueryParam "causalHash" CausalHash
    :> StreamGet NewlineFraming CBOR (SourceIO Text)

server :: ServerT API WebApp
server =
  downloadCausalStreamEndpointConduit

downloadCausalStreamEndpointConduit :: UserId -> CausalHash -> WebApp (SourceIO Text)
downloadCausalStreamEndpointConduit callerUserId causalHash = do
  let authZReceipt = AuthZ.adminOverride
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase callerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  q <- liftIO $ STM.newTBQueueIO 10
  streamResults <- UnliftIO.toIO do
    Codebase.runCodebaseTransaction codebase $ do
      (_bhId, causalId) <- CausalQ.expectCausalIdsOf id causalHash
      cursor <- SSQ.allHashDependenciesOfCausalCursor causalId
      Cursor.foldBatched cursor 1000 \batch -> do
        PG.transactionUnsafeIO $ STM.atomically $ STM.writeTBQueue q batch
  pure $ conduitToSourceIO do
    handle <- liftIO $ Async.async streamResults
    stream q handle
  where
    stream :: STM.TBQueue (NonEmpty Text) -> Async.Async () -> ConduitT () Text IO ()
    stream q async = do
      let loop :: ConduitT () Text IO ()
          loop = do
            next <- liftIO . STM.atomically $ do
              STM.tryReadTBQueue q >>= \case
                Nothing -> do
                  Async.waitSTM async $> Nothing
                Just batch -> do
                  pure $ Just batch
            case next of
              Nothing -> pure ()
              Just batch -> do
                yieldMany batch
                liftIO $ threadDelay 1000000
                loop
      loop
