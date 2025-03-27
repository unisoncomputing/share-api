module Share.Notifications.Impl (server) where

import Data.Time
import Servant
import Share.IDs
import Share.Notifications.API qualified as API
import Share.OAuth.Session (Session (..))
import Share.Web.App

server :: Session -> UserHandle -> ServerT API.API WebApp
server Session {sessionUserId} userHandle =
  API.Routes {API.getNotificationsEndpoint = getNotificationsEndpoint sessionUserId userHandle}

getNotificationsEndpoint :: UserId -> UserHandle -> Maybe Int -> Maybe UTCTime -> WebApp API.GetNotificationsResponse
getNotificationsEndpoint callerUserId userHandle limit afterTime = do
  error "TODO"
