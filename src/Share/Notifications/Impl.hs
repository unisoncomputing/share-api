module Share.Notifications.Impl (server) where

import Control.Lens
import Servant
import Servant.Server.Generic (AsServerT)
import Share.IDs
import Share.Notifications.API (GetHubEntriesCursor)
import Share.Notifications.API qualified as API
import Share.Notifications.Ops qualified as NotifOps
import Share.Notifications.Queries qualified as NotificationQ
import Share.Notifications.Types
import Share.Postgres qualified as PG
import Share.Postgres.Ops qualified as UserQ
import Share.User (User (..))
import Share.Utils.API (Cursor, Paged)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

hubRoutes :: UserHandle -> API.HubEntriesRoutes (AsServerT WebApp)
hubRoutes userHandle =
  API.HubEntriesRoutes
    { getHubEntriesEndpoint = getHubEntriesEndpoint userHandle,
      updateHubEntriesEndpoint = updateHubEntriesEndpoint userHandle
    }

server :: UserHandle -> ServerT API.API WebApp
server userHandle =
  API.Routes
    { hubRoutes = hubRoutes userHandle
    }

getHubEntriesEndpoint ::
  UserHandle ->
  UserId ->
  Maybe Int ->
  Maybe (Cursor GetHubEntriesCursor) ->
  Maybe API.StatusFilter ->
  WebApp (Paged GetHubEntriesCursor (NotificationHubEntry UnifiedDisplayInfo HydratedEvent))
getHubEntriesEndpoint userHandle callerUserId limit mayCursor mayStatusFilter = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsGet callerUserId notificationUserId
  notifications <- PG.runTransaction $ do
    notifs <- NotificationQ.listNotificationHubEntryPayloads notificationUserId limit mayCursor (API.getStatusFilter <$> mayStatusFilter)
    forOf (traversed . traversed) notifs NotifOps.hydrateEvent
  pure notifications

updateHubEntriesEndpoint :: UserHandle -> UserId -> API.UpdateHubEntriesRequest -> WebApp ()
updateHubEntriesEndpoint userHandle callerUserId API.UpdateHubEntriesRequest {notificationStatus, notificationIds} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsUpdate callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateNotificationHubEntries notificationIds notificationStatus
  pure ()
