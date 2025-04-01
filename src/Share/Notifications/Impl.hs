module Share.Notifications.Impl (server) where

import Data.Time
import Servant
import Share.IDs
import Share.Notifications.API qualified as API
import Share.Notifications.Queries qualified as NotificationQ
import Share.Postgres qualified as PG
import Share.Postgres.Ops qualified as UserQ
import Share.User (User (..))
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ

server :: UserHandle -> ServerT API.API WebApp
server userHandle =
  API.Routes
    { API.getNotificationsEndpoint = getNotificationsEndpoint userHandle,
      getDeliveryMechanismsEndpoint = getDeliveryMechanismsEndpoint userHandle,
      subscriptionsRoutes = subscriptionsRoutes userHandle,
      emailDeliveryRoutes = emailDeliveryRoutes userHandle,
      webhookDeliveryRoutes = webhookDeliveryRoutes userHandle,
      hubManagementRoutes = hubManagementRoutes userHandle
    }

getNotificationsEndpoint :: UserHandle -> UserId -> Maybe Int -> Maybe UTCTime -> Maybe API.StatusFilter -> WebApp API.GetNotificationsResponse
getNotificationsEndpoint userHandle callerUserId limit afterTime mayStatusFilter = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsGet callerUserId notificationUserId
  notifications <- PG.runTransaction $ NotificationQ.listNotificationHubEntries notificationUserId limit afterTime (API.getStatusFilter <$> mayStatusFilter)
  pure $ API.GetNotificationsResponse {notifications}
