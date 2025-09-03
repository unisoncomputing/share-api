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

subscriptionsRoutes :: UserHandle -> API.SubscriptionRoutes (AsServerT WebApp)
subscriptionsRoutes userHandle =
  API.SubscriptionRoutes
    { getSubscriptionsEndpoint = getSubscriptionsEndpoint userHandle,
      createSubscriptionEndpoint = createSubscriptionEndpoint userHandle,
      deleteSubscriptionEndpoint = deleteSubscriptionEndpoint userHandle,
      updateSubscriptionEndpoint = updateSubscriptionEndpoint userHandle
    }

server :: UserHandle -> ServerT API.API WebApp
server userHandle =
  API.Routes
    { hubRoutes = hubRoutes userHandle,
      subscriptionsRoutes = subscriptionsRoutes userHandle
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

getSubscriptionsEndpoint :: UserHandle -> UserId -> WebApp API.GetSubscriptionsResponse
getSubscriptionsEndpoint userHandle callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsView callerUserId notificationUserId
  subscriptions <- PG.runTransaction $ NotificationQ.listNotificationSubscriptions notificationUserId
  pure $ API.GetSubscriptionsResponse {subscriptions}

createSubscriptionEndpoint :: UserHandle -> UserId -> API.CreateSubscriptionRequest -> WebApp API.CreateSubscriptionResponse
createSubscriptionEndpoint subscriberHandle callerUserId API.CreateSubscriptionRequest {subscriptionScope, subscriptionProjectId, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} = do
  User {user_id = subscriberUserId} <- UserQ.expectUserByHandle subscriberHandle
  User {user_id = scopeUserId} <- UserQ.expectUserByHandle subscriptionScope
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId subscriberUserId
  -- NOTE: We allow creating any sort of notification subscription, even for things a user
  -- doesn't have access to, but the notification system will only actually create notifications if the caller has access to
  -- the resource of a given event for the permission associated to that topic via the
  -- 'topic_permission' SQL function.
  subscription <- PG.runTransaction $ do
    subscriptionId <- NotificationQ.createNotificationSubscription subscriberUserId scopeUserId subscriptionProjectId subscriptionTopics subscriptionTopicGroups subscriptionFilter
    NotificationQ.getNotificationSubscription subscriberUserId subscriptionId
  pure $ API.CreateSubscriptionResponse {subscription}

deleteSubscriptionEndpoint :: UserHandle -> UserId -> NotificationSubscriptionId -> WebApp ()
deleteSubscriptionEndpoint userHandle callerUserId subscriptionId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.deleteNotificationSubscription notificationUserId subscriptionId
  pure ()

updateSubscriptionEndpoint :: UserHandle -> UserId -> NotificationSubscriptionId -> API.UpdateSubscriptionRequest -> WebApp API.CreateSubscriptionResponse
updateSubscriptionEndpoint userHandle callerUserId subscriptionId API.UpdateSubscriptionRequest {subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId notificationUserId
  subscription <- PG.runTransaction $ do
    NotificationQ.updateNotificationSubscription notificationUserId subscriptionId subscriptionTopics subscriptionTopicGroups subscriptionFilter
    NotificationQ.getNotificationSubscription notificationUserId subscriptionId
  pure $ API.CreateSubscriptionResponse {subscription}
