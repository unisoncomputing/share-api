module Share.Notifications.Impl (server) where

import Control.Lens (forOf, traversed)
import Data.Time
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
import Share.Prelude
import Share.User (User (..))
import Share.Utils.API (Cursor, pagedOn)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ

hubRoutes :: UserHandle -> API.HubEntriesRoutes (AsServerT WebApp)
hubRoutes userHandle =
  API.HubEntriesRoutes
    { getHubEntriesEndpoint = getHubEntriesEndpoint userHandle,
      updateHubEntriesEndpoint = updateHubEntriesEndpoint userHandle
    }

deliveryMethodRoutes :: UserHandle -> API.DeliveryMethodRoutes (AsServerT WebApp)
deliveryMethodRoutes userHandle =
  API.DeliveryMethodRoutes
    { getDeliveryMethodsEndpoint = getDeliveryMethodsEndpoint userHandle,
      emailDeliveryRoutes = emailDeliveryRoutes userHandle,
      webhookDeliveryRoutes = webhookDeliveryRoutes userHandle
    }

subscriptionsRoutes :: UserHandle -> API.SubscriptionRoutes (AsServerT WebApp)
subscriptionsRoutes userHandle =
  API.SubscriptionRoutes
    { getSubscriptionsEndpoint = getSubscriptionsEndpoint userHandle,
      createSubscriptionEndpoint = createSubscriptionEndpoint userHandle,
      deleteSubscriptionEndpoint = deleteSubscriptionEndpoint userHandle,
      updateSubscriptionEndpoint = updateSubscriptionEndpoint userHandle,
      subscriptionResourceRoutes = subscriptionResourceRoutes userHandle
    }

subscriptionResourceRoutes :: UserHandle -> NotificationSubscriptionId -> API.SubscriptionResourceRoutes (AsServerT WebApp)
subscriptionResourceRoutes handle subscriptionId =
  API.SubscriptionResourceRoutes
    { subscriptionDeliveryMethodRoutes = subscriptionDeliveryMethodRoutes handle subscriptionId
    }

subscriptionDeliveryMethodRoutes :: UserHandle -> NotificationSubscriptionId -> API.SubscriptionDeliveryMethodRoutes (AsServerT WebApp)
subscriptionDeliveryMethodRoutes handle subscriptionId =
  API.SubscriptionDeliveryMethodRoutes
    { getSubscriptionDeliveryMethodsEndpoint = getSubscriptionDeliveryMethodsEndpoint handle subscriptionId,
      addSubscriptionDeliveryMethodsEndpoint = addSubscriptionDeliveryMethodsEndpoint handle subscriptionId,
      removeSubscriptionDeliveryMethodsEndpoint = removeSubscriptionDeliveryMethodsEndpoint handle subscriptionId
    }

emailDeliveryRoutes :: UserHandle -> API.EmailRoutes (AsServerT WebApp)
emailDeliveryRoutes userHandle =
  API.EmailRoutes
    { createEmailDeliveryMethodEndpoint = createEmailDeliveryMethodEndpoint userHandle,
      deleteEmailDeliveryMethodEndpoint = deleteEmailDeliveryMethodEndpoint userHandle,
      updateEmailDeliveryMethodEndpoint = updateEmailDeliveryMethodEndpoint userHandle
    }

webhookDeliveryRoutes :: UserHandle -> API.WebhookRoutes (AsServerT WebApp)
webhookDeliveryRoutes userHandle =
  API.WebhookRoutes
    { createWebhookEndpoint = createWebhookEndpoint userHandle,
      deleteWebhookEndpoint = deleteWebhookEndpoint userHandle,
      updateWebhookEndpoint = updateWebhookEndpoint userHandle
    }

server :: UserHandle -> ServerT API.API WebApp
server userHandle =
  API.Routes
    { hubRoutes = hubRoutes userHandle,
      deliveryMethodRoutes = deliveryMethodRoutes userHandle,
      subscriptionsRoutes = subscriptionsRoutes userHandle
    }

getHubEntriesEndpoint :: UserHandle -> UserId -> Maybe Int -> Maybe (Cursor GetHubEntriesCursor) -> Maybe API.StatusFilter -> WebApp API.GetHubEntriesResponse
getHubEntriesEndpoint userHandle callerUserId limit mayCursor mayStatusFilter = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsGet callerUserId notificationUserId
  notifications <- PG.runTransaction do
    notifs <- NotificationQ.listNotificationHubEntryPayloads notificationUserId limit afterTime (API.getStatusFilter <$> mayStatusFilter)
    forOf (traversed . traversed) notifs NotifOps.hydrateEvent
  paged <-
    notifications
      & pagedOn (\(NotificationHubEntry {hubEntryId, hubEntryCreatedAt}) -> (hubEntryCreatedAt, hubEntryId))
      & pure
  pure $ API.GetHubEntriesResponse {notifications = paged}

updateHubEntriesEndpoint :: UserHandle -> UserId -> API.UpdateHubEntriesRequest -> WebApp ()
updateHubEntriesEndpoint userHandle callerUserId API.UpdateHubEntriesRequest {notificationStatus, notificationIds} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsUpdate callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateNotificationHubEntries notificationIds notificationStatus
  pure ()

getDeliveryMethodsEndpoint :: UserHandle -> UserId -> WebApp API.GetDeliveryMethodsResponse
getDeliveryMethodsEndpoint userHandle callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsView callerUserId notificationUserId
  deliveryMethods <- NotifOps.listNotificationDeliveryMethods notificationUserId Nothing
  pure $ API.GetDeliveryMethodsResponse {deliveryMethods}

createEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> API.CreateEmailDeliveryMethodRequest -> WebApp API.CreateEmailDeliveryMethodResponse
createEmailDeliveryMethodEndpoint userHandle callerUserId API.CreateEmailDeliveryMethodRequest {email} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  emailDeliveryMethodId <- PG.runTransaction $ NotificationQ.createEmailDeliveryMethod notificationUserId email
  pure $ API.CreateEmailDeliveryMethodResponse {emailDeliveryMethodId}

deleteEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> NotificationEmailDeliveryMethodId -> WebApp ()
deleteEmailDeliveryMethodEndpoint userHandle callerUserId emailDeliveryMethodId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.deleteEmailDeliveryMethod notificationUserId emailDeliveryMethodId
  pure ()

updateEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> NotificationEmailDeliveryMethodId -> API.UpdateEmailDeliveryMethodRequest -> WebApp ()
updateEmailDeliveryMethodEndpoint userHandle callerUserId emailDeliveryMethodId API.UpdateEmailDeliveryMethodRequest {email} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateEmailDeliveryMethod notificationUserId emailDeliveryMethodId email
  pure ()

getSubscriptionDeliveryMethodsEndpoint :: UserHandle -> NotificationSubscriptionId -> UserId -> WebApp API.GetDeliveryMethodsResponse
getSubscriptionDeliveryMethodsEndpoint userHandle subscriptionId callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsView callerUserId notificationUserId
  deliveryMethods <- NotifOps.listNotificationDeliveryMethods notificationUserId (Just subscriptionId)
  pure $ API.GetDeliveryMethodsResponse {deliveryMethods}

addSubscriptionDeliveryMethodsEndpoint :: UserHandle -> NotificationSubscriptionId -> UserId -> API.AddSubscriptionDeliveryMethodsRequest -> WebApp ()
addSubscriptionDeliveryMethodsEndpoint userHandle subscriptionId callerUserId API.AddSubscriptionDeliveryMethodsRequest {deliveryMethods} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.addSubscriptionDeliveryMethods notificationUserId subscriptionId deliveryMethods
  pure ()

removeSubscriptionDeliveryMethodsEndpoint :: UserHandle -> NotificationSubscriptionId -> UserId -> API.RemoveSubscriptionDeliveryMethodsRequest -> WebApp ()
removeSubscriptionDeliveryMethodsEndpoint userHandle subscriptionId callerUserId API.RemoveSubscriptionDeliveryMethodsRequest {deliveryMethods} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.removeSubscriptionDeliveryMethods notificationUserId subscriptionId deliveryMethods
  pure ()

createWebhookEndpoint :: UserHandle -> UserId -> API.CreateWebhookRequest -> WebApp API.CreateWebhookResponse
createWebhookEndpoint userHandle callerUserId API.CreateWebhookRequest {url, name = webhookName} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  webhookId <- NotifOps.createWebhookDeliveryMethod notificationUserId url webhookName
  pure $ API.CreateWebhookResponse {webhookId}

deleteWebhookEndpoint :: UserHandle -> UserId -> NotificationWebhookId -> WebApp ()
deleteWebhookEndpoint userHandle callerUserId webhookDeliveryMethodId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  NotifOps.deleteWebhookDeliveryMethod notificationUserId webhookDeliveryMethodId
  pure ()

updateWebhookEndpoint :: UserHandle -> UserId -> NotificationWebhookId -> API.UpdateWebhookRequest -> WebApp ()
updateWebhookEndpoint userHandle callerUserId webhookDeliveryMethodId API.UpdateWebhookRequest {url} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  NotifOps.updateWebhookDeliveryMethod notificationUserId webhookDeliveryMethodId url
  pure ()

getSubscriptionsEndpoint :: UserHandle -> UserId -> WebApp API.GetSubscriptionsResponse
getSubscriptionsEndpoint userHandle callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsView callerUserId notificationUserId
  subscriptions <- PG.runTransaction $ NotificationQ.listNotificationSubscriptions notificationUserId
  pure $ API.GetSubscriptionsResponse {subscriptions}

createSubscriptionEndpoint :: UserHandle -> UserId -> API.CreateSubscriptionRequest -> WebApp API.CreateSubscriptionResponse
createSubscriptionEndpoint subscriberHandle callerUserId API.CreateSubscriptionRequest {subscriptionScope, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} = do
  User {user_id = subscriberUserId} <- UserQ.expectUserByHandle subscriberHandle
  User {user_id = scopeUserId} <- UserQ.expectUserByHandle subscriptionScope
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId subscriberUserId
  -- NOTE: We allow creating any sort of notification subscription, even for things a user
  -- doesn't have access to, but the notification system will only actually create notifications if the caller has access to
  -- the resource of a given event for the permission associated to that topic via the
  -- 'topic_permission' SQL function.
  subscription <- PG.runTransaction $ do
    subscriptionId <- NotificationQ.createNotificationSubscription subscriberUserId scopeUserId subscriptionTopics subscriptionTopicGroups subscriptionFilter
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
