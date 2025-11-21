module Share.Web.UCM.HistoryComments.Impl (server) where

import Conduit (ConduitT)
import Servant
import Share.IDs
import Share.Utils.Servant.Streaming qualified as Streaming
import Share.Web.App (WebApp, WebAppServer)
import Share.Web.Errors (Unimplemented (Unimplemented), respondError)
import Unison.Server.HistoryComments.API qualified as HistoryComments
import Unison.Server.HistoryComments.Types (DownloadCommentsRequest (DownloadCommentsRequest), HistoryCommentChunk, UploadCommentsResponse)
import Unison.Util.Servant.CBOR

server :: Maybe UserId -> HistoryComments.Routes WebAppServer
server mayCaller =
  HistoryComments.Routes
    { downloadHistoryComments = downloadHistoryCommentsStreamImpl mayCaller,
      uploadHistoryComments = uploadHistoryCommentsStreamImpl mayCaller
    }

downloadHistoryCommentsStreamImpl :: Maybe UserId -> DownloadCommentsRequest -> WebApp (SourceIO (CBORStream HistoryCommentChunk))
downloadHistoryCommentsStreamImpl mayUserId (DownloadCommentsRequest {}) = do
  respondError Unimplemented

uploadHistoryCommentsStreamImpl :: Maybe UserId -> SourceIO (CBORStream HistoryCommentChunk) -> WebApp UploadCommentsResponse
uploadHistoryCommentsStreamImpl mayUserId inputStream = do
  inputConduit :: ConduitT i HistoryCommentChunk IO () <- Streaming.toConduit inputStream
  _
