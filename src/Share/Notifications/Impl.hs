module Share.Notifications.Impl (server) where

import Data.Set qualified as Set
import Data.Time
import Servant
import Servant.Server.Generic (AsServerT)
import Share.IDs
import Share.Notifications.API qualified as API
import Share.Notifications.Queries qualified as NotificationQ
import Share.Postgres qualified as PG
import Share.Postgres.Ops qualified as UserQ
import Share.User (User (..))
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ

hubRoutes :: UserHandle -> API.HubEntriesRoutes (AsServerT WebApp)
hubRoutes userHandle =
  API.HubEntriesRoutes
    { getHubEntriesEndpoint = getHubEntriesEndpoint userHandle,
      updateHubEntryEndpoint = updateHubEntryEndpoint userHandle
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
      updateSubscriptionEndpoint = updateSubscriptionEndpoint userHandle
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

getHubEntriesEndpoint :: UserHandle -> UserId -> Maybe Int -> Maybe UTCTime -> Maybe API.StatusFilter -> WebApp API.GetHubEntriesResponse
getHubEntriesEndpoint userHandle callerUserId limit afterTime mayStatusFilter = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsGet callerUserId notificationUserId
  notifications <- PG.runTransaction $ NotificationQ.listNotificationHubEntries notificationUserId limit afterTime (API.getStatusFilter <$> mayStatusFilter)
  pure $ API.GetHubEntriesResponse {notifications}

updateHubEntryEndpoint :: UserHandle -> UserId -> NotificationHubEntryId -> API.UpdateHubEntryRequest -> WebApp NoContent
updateHubEntryEndpoint userHandle callerUserId hubEntryId API.UpdateHubEntryRequest {notificationStatus} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkNotificationsUpdate callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateNotificationHubEntry hubEntryId notificationStatus
  pure NoContent

getDeliveryMethodsEndpoint :: UserHandle -> UserId -> WebApp API.GetDeliveryMethodsResponse
getDeliveryMethodsEndpoint userHandle callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsView callerUserId notificationUserId
  deliveryMethods <- fmap Set.fromList . PG.runTransaction $ NotificationQ.listNotificationDeliveryMethods notificationUserId
  pure $ API.GetDeliveryMethodsResponse {deliveryMethods}

createEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> API.CreateEmailDeliveryMethodRequest -> WebApp API.CreateEmailDeliveryMethodResponse
createEmailDeliveryMethodEndpoint userHandle callerUserId API.CreateEmailDeliveryMethodRequest {email} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  emailDeliveryMethodId <- PG.runTransaction $ NotificationQ.createEmailDeliveryMethod notificationUserId email
  pure $ API.CreateEmailDeliveryMethodResponse {emailDeliveryMethodId}

deleteEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> NotificationEmailDeliveryMethodId -> WebApp NoContent
deleteEmailDeliveryMethodEndpoint userHandle callerUserId emailDeliveryMethodId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.deleteEmailDeliveryMethod notificationUserId emailDeliveryMethodId
  pure NoContent

updateEmailDeliveryMethodEndpoint :: UserHandle -> UserId -> NotificationEmailDeliveryMethodId -> API.UpdateEmailDeliveryMethodRequest -> WebApp NoContent
updateEmailDeliveryMethodEndpoint userHandle callerUserId emailDeliveryMethodId API.UpdateEmailDeliveryMethodRequest {email} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateEmailDeliveryMethod notificationUserId emailDeliveryMethodId email
  pure NoContent

createWebhookEndpoint :: UserHandle -> UserId -> API.CreateWebhookRequest -> WebApp API.CreateWebhookResponse
createWebhookEndpoint userHandle callerUserId API.CreateWebhookRequest {url} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  webhookDeliveryMethodId <- PG.runTransaction $ NotificationQ.createWebhookDeliveryMethod notificationUserId url
  pure $ API.CreateWebhookResponse {webhookDeliveryMethodId}

deleteWebhookEndpoint :: UserHandle -> UserId -> NotificationWebhookId -> WebApp NoContent
deleteWebhookEndpoint userHandle callerUserId webhookDeliveryMethodId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.deleteWebhookDeliveryMethod notificationUserId webhookDeliveryMethodId
  pure NoContent

updateWebhookEndpoint :: UserHandle -> UserId -> NotificationWebhookId -> API.UpdateWebhookRequest -> WebApp NoContent
updateWebhookEndpoint userHandle callerUserId webhookDeliveryMethodId API.UpdateWebhookRequest {url} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkDeliveryMethodsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.updateWebhookDeliveryMethod notificationUserId webhookDeliveryMethodId url
  pure NoContent

getSubscriptionsEndpoint :: UserHandle -> UserId -> WebApp API.GetSubscriptionsResponse
getSubscriptionsEndpoint userHandle callerUserId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsView callerUserId notificationUserId
  subscriptions <- PG.runTransaction $ NotificationQ.listNotificationSubscriptions notificationUserId
  pure $ API.GetSubscriptionsResponse {subscriptions}

createSubscriptionEndpoint :: UserHandle -> UserId -> API.CreateSubscriptionRequest -> WebApp API.CreateSubscriptionResponse
createSubscriptionEndpoint userHandle callerUserId API.CreateSubscriptionRequest {subscriptionScope, subscriptionTopics, subscriptionFilter} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  User {user_id = scopeUserId} <- UserQ.expectUserByHandle subscriptionScope
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId notificationUserId
  -- NOTE: We allow creating any sort of notification subscription, even for things a user
  -- doesn't have access to, but the notification system will only actually create notifications if the caller has access to
  -- the resource of a given event for the permission associated to that topic via the
  -- 'topic_permission' SQL function.
  subscription <- PG.runTransaction $ do
    subscriptionId <- NotificationQ.createNotificationSubscription notificationUserId scopeUserId subscriptionTopics subscriptionFilter
    NotificationQ.getNotificationSubscription notificationUserId subscriptionId
  pure $ API.CreateSubscriptionResponse {subscription}

deleteSubscriptionEndpoint :: UserHandle -> UserId -> NotificationSubscriptionId -> WebApp NoContent
deleteSubscriptionEndpoint userHandle callerUserId subscriptionId = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId notificationUserId
  PG.runTransaction $ NotificationQ.deleteNotificationSubscription notificationUserId subscriptionId
  pure NoContent

updateSubscriptionEndpoint :: UserHandle -> UserId -> NotificationSubscriptionId -> API.UpdateSubscriptionRequest -> WebApp API.CreateSubscriptionResponse
updateSubscriptionEndpoint userHandle callerUserId subscriptionId API.UpdateSubscriptionRequest {subscriptionTopics, subscriptionFilter} = do
  User {user_id = notificationUserId} <- UserQ.expectUserByHandle userHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkSubscriptionsManage callerUserId notificationUserId
  subscription <- PG.runTransaction $ do
    NotificationQ.updateNotificationSubscription notificationUserId subscriptionId subscriptionTopics subscriptionFilter
    NotificationQ.getNotificationSubscription notificationUserId subscriptionId
  pure $ API.CreateSubscriptionResponse {subscription}
