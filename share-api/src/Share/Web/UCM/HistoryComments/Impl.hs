module Share.Web.UCM.HistoryComments.Impl (server) where

import Conduit (ConduitT)
import Data.Void
import Network.WebSockets.Connection
import Servant
import Share.IDs
import Share.Utils.Servant.Streaming qualified as Streaming
import Share.Web.App (WebApp, WebAppServer)
import Share.Web.Errors (Unimplemented (Unimplemented), respondError)
import Unison.Server.HistoryComments.API qualified as HistoryComments
import Unison.Server.HistoryComments.Types (DownloadCommentsRequest (DownloadCommentsRequest), HistoryCommentChunk, UploadCommentsResponse)
import Unison.Util.Servant.CBOR
import Unison.Util.Websockets

server :: Maybe UserId -> HistoryComments.Routes WebAppServer
server mayCaller =
  HistoryComments.Routes
    { downloadHistoryComments = downloadHistoryCommentsStreamImpl mayCaller,
      uploadHistoryComments = uploadHistoryCommentsStreamImpl mayCaller
    }

wsMessageBufferSize :: Int
wsMessageBufferSize = 100

downloadHistoryCommentsStreamImpl :: Maybe UserId -> Connection -> WebApp ()
downloadHistoryCommentsStreamImpl mayUserId (DownloadCommentsRequest {}) = do
  _ <- error "AUTH CHECK HERE"
  respondError Unimplemented

-- Re-run the given STM action at most n times, collecting the results into a list.
-- If the action returns Nothing, stop early and return what has been collected so far, along with a Bool indicating whether the action was exhausted.
fetchChunk :: Int -> STM (Maybe a) -> STM ([a], Bool)
fetchChunk size action = do
  let go 0 = pure []
      go n = do
        optional action >>= \case
          Nothing -> do
            -- No more values available at the moment
            pure ([], False)
          Just Nothing -> do
            -- Queue is closed
            pure ([], True)
          Just (Just val) -> do
            (rest, exhausted) <- go (n - 1)
            pure (val : rest, exhausted)
  go size

uploadHistoryCommentsStreamImpl :: Maybe UserId -> BranchRef -> Connection -> WebApp ()
uploadHistoryCommentsStreamImpl mayUserId branchRef conn = do
  authZ <- error "AUTH CHECK HERE"
  projectId <- error "Process Branch Ref"
  withQueues @HistoryCommentChunk @_ wsMessageBufferSize wsMessageBufferSize conn \Queues {receive} -> do
    errVar <- newEmptyMVar
    let loop = do
          chunk <- fetchChunk insertCommentBatchSize do
            receive <&> fmap \case
                  HistoryCommentErrorChunk err -> Just (Left err)
                  chunk -> Just (Right chunk)
          PG.runTransaction $ Q.insertHistoryComments authZ projectId chunk
   loop
    where
      insertCommentBatchSize = 100
