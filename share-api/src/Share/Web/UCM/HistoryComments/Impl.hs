module Share.Web.UCM.HistoryComments.Impl (server) where

import Share.IDs
import Share.Web.App (WebAppServer)
import Share.Web.Share.Webhooks.API qualified as HistoryComments

server :: Maybe UserId -> HistoryComments.Routes WebAppServer
server mayCaller =
  HistoryComments.Routes
    { downloadHistoryCommentsStream = downloadHistoryCommentsStreamImpl mayCaller,
      uploadHistoryCommentsStream = uploadHistoryCommentsStreamImpl mayCaller
    }
