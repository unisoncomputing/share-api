{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API
  ( API,
    Routes (..),
    HubEntriesRoutes (..),
    DeliveryMethodRoutes (..),
    SubscriptionRoutes (..),
    EmailRoutes (..),
    WebhookRoutes (..),
    GetHubEntriesCursor,
    StatusFilter (..),
    UpdateHubEntriesRequest (..),
    GetSubscriptionsResponse (..),
    CreateSubscriptionRequest (..),
    CreateSubscriptionResponse (..),
    UpdateSubscriptionRequest (..),
    SubscriptionDeliveryMethodRoutes (..),
    SubscriptionResourceRoutes (..),
    AddSubscriptionDeliveryMethodsRequest (..),
    RemoveSubscriptionDeliveryMethodsRequest (..),
    GetDeliveryMethodsResponse (..),
    CreateEmailDeliveryMethodRequest (..),
    CreateEmailDeliveryMethodResponse (..),
    UpdateEmailDeliveryMethodRequest (..),
    CreateWebhookRequest (..),
    CreateWebhookResponse (..),
    UpdateWebhookRequest (..),
  )
where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NEL
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Servant
import Share.IDs
import Share.Notifications.Types (DeliveryMethodId, HydratedEvent, NotificationDeliveryMethod, NotificationHubEntry, NotificationStatus, NotificationSubscription, NotificationTopic, NotificationTopicGroup, SubscriptionFilter)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude
import Share.Utils.API (Cursor, Paged)
import Share.Utils.URI (URIParam)
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { hubRoutes :: mode :- "hub" :> NamedRoutes HubEntriesRoutes,
    deliveryMethodRoutes :: mode :- "delivery-methods" :> NamedRoutes DeliveryMethodRoutes,
    subscriptionsRoutes :: mode :- "subscriptions" :> NamedRoutes SubscriptionRoutes
  }
  deriving stock (Generic)

data HubEntriesRoutes mode
  = HubEntriesRoutes
  { getHubEntriesEndpoint :: mode :- GetHubEntriesEndpoint,
    updateHubEntriesEndpoint :: mode :- UpdateHubEntriesEndpoint
  }
  deriving stock (Generic)

data DeliveryMethodRoutes mode
  = DeliveryMethodRoutes
  { getDeliveryMethodsEndpoint :: mode :- GetDeliveryMethodsEndpoint,
    emailDeliveryRoutes :: mode :- "emails" :> NamedRoutes EmailRoutes,
    webhookDeliveryRoutes :: mode :- "webhooks" :> NamedRoutes WebhookRoutes
  }
  deriving stock (Generic)

data SubscriptionRoutes mode
  = SubscriptionRoutes
  { getSubscriptionsEndpoint :: mode :- GetSubscriptionsEndpoint,
    createSubscriptionEndpoint :: mode :- CreateSubscriptionEndpoint,
    deleteSubscriptionEndpoint :: mode :- DeleteSubscriptionEndpoint,
    updateSubscriptionEndpoint :: mode :- UpdateSubscriptionEndpoint,
    subscriptionResourceRoutes :: mode :- Capture "subscriptionId" NotificationSubscriptionId :> NamedRoutes SubscriptionResourceRoutes
  }
  deriving stock (Generic)

data SubscriptionResourceRoutes mode
  = SubscriptionResourceRoutes
  { subscriptionDeliveryMethodRoutes :: mode :- "delivery-methods" :> NamedRoutes SubscriptionDeliveryMethodRoutes
  }
  deriving stock (Generic)

data SubscriptionDeliveryMethodRoutes mode
  = SubscriptionDeliveryMethodRoutes
  { getSubscriptionDeliveryMethodsEndpoint :: mode :- GetDeliveryMethodsEndpoint,
    addSubscriptionDeliveryMethodsEndpoint :: mode :- "add" :> AddSubscriptionDeliveryMethodsEndpoint,
    removeSubscriptionDeliveryMethodsEndpoint :: mode :- "remove" :> RemoveSubscriptionDeliveryMethodsEndpoint
  }
  deriving stock (Generic)

data EmailRoutes mode
  = EmailRoutes
  { createEmailDeliveryMethodEndpoint :: mode :- CreateEmailDeliveryMethodEndpoint,
    deleteEmailDeliveryMethodEndpoint :: mode :- DeleteEmailDeliveryMethodEndpoint,
    updateEmailDeliveryMethodEndpoint :: mode :- UpdateEmailDeliveryMethodEndpoint
  }
  deriving stock (Generic)

data WebhookRoutes mode
  = WebhookRoutes
  { createWebhookEndpoint :: mode :- CreateWebhookEndpoint,
    deleteWebhookEndpoint :: mode :- DeleteWebhookEndpoint,
    updateWebhookEndpoint :: mode :- UpdateWebhookEndpoint
  }
  deriving stock (Generic)

type GetSubscriptionsEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] GetSubscriptionsResponse

data GetSubscriptionsResponse
  = GetSubscriptionsResponse
  { subscriptions :: [NotificationSubscription NotificationSubscriptionId]
  }

instance ToJSON GetSubscriptionsResponse where
  toJSON GetSubscriptionsResponse {subscriptions} =
    object ["subscriptions" .= subscriptions]

instance FromJSON GetSubscriptionsResponse where
  parseJSON = withObject "GetSubscriptionsResponse" $ \o -> do
    subscriptions <- o .: "subscriptions"
    pure GetSubscriptionsResponse {subscriptions}

type CreateSubscriptionEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateSubscriptionRequest
    :> Post '[JSON] CreateSubscriptionResponse

data CreateSubscriptionRequest
  = CreateSubscriptionRequest
  { subscriptionScope :: UserHandle,
    subscriptionTopics :: Set NotificationTopic,
    subscriptionTopicGroups :: Set NotificationTopicGroup,
    subscriptionFilter :: Maybe SubscriptionFilter
  }

instance FromJSON CreateSubscriptionRequest where
  parseJSON = withObject "CreateSubscriptionRequest" $ \o -> do
    subscriptionScope <- o .: "scope"
    subscriptionTopics <- o .: "topics"
    subscriptionTopicGroups <- o .: "topicGroups"
    subscriptionFilter <- o .:? "filter"
    pure CreateSubscriptionRequest {subscriptionScope, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter}

data CreateSubscriptionResponse
  = CreateSubscriptionResponse
  { subscription :: NotificationSubscription NotificationSubscriptionId
  }

instance ToJSON CreateSubscriptionResponse where
  toJSON CreateSubscriptionResponse {subscription} =
    object ["subscription" .= subscription]

instance FromJSON CreateSubscriptionResponse where
  parseJSON = withObject "CreateSubscriptionResponse" $ \o -> do
    subscription <- o .: "subscription"
    pure CreateSubscriptionResponse {subscription}

type DeleteSubscriptionEndpoint =
  AuthenticatedUserId
    :> Capture "subscription_id" NotificationSubscriptionId
    :> Delete '[JSON] ()

type UpdateSubscriptionEndpoint =
  AuthenticatedUserId
    :> Capture "subscription_id" NotificationSubscriptionId
    :> ReqBody '[JSON] UpdateSubscriptionRequest
    :> Patch '[JSON] CreateSubscriptionResponse

data UpdateSubscriptionRequest
  = UpdateSubscriptionRequest
  { subscriptionTopics :: Maybe (Set NotificationTopic),
    subscriptionTopicGroups :: Maybe (Set NotificationTopicGroup),
    subscriptionFilter :: Maybe SubscriptionFilter
  }

instance FromJSON UpdateSubscriptionRequest where
  parseJSON = withObject "UpdateSubscriptionRequest" $ \o -> do
    subscriptionTopics <- o .:? "topics"
    subscriptionTopicGroups <- o .:? "topicGroups"
    subscriptionFilter <- o .:? "filter"
    pure UpdateSubscriptionRequest {subscriptionTopics, subscriptionTopicGroups, subscriptionFilter}

type GetDeliveryMethodsEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] GetDeliveryMethodsResponse

data GetDeliveryMethodsResponse
  = GetDeliveryMethodsResponse
  { deliveryMethods :: [NotificationDeliveryMethod]
  }

instance ToJSON GetDeliveryMethodsResponse where
  toJSON GetDeliveryMethodsResponse {deliveryMethods} =
    object ["deliveryMethods" .= deliveryMethods]

instance FromJSON GetDeliveryMethodsResponse where
  parseJSON = withObject "GetDeliveryMethodsResponse" $ \o -> do
    deliveryMethods <- o .: "deliveryMethods"
    pure GetDeliveryMethodsResponse {deliveryMethods}

newtype StatusFilter = StatusFilter
  { getStatusFilter :: NESet NotificationStatus
  }

instance FromHttpApiData StatusFilter where
  parseQueryParam q =
    for (Text.splitOn "," q) parseQueryParam
      <&> NEL.nonEmpty
      >>= \case
        Nothing -> Left "Empty status filter"
        Just statuses -> Right $ StatusFilter $ NESet.fromList statuses

instance ToHttpApiData StatusFilter where
  toQueryParam (StatusFilter statuses) =
    toList statuses
      <&> toQueryParam
      & Text.intercalate ","

instance FromJSON StatusFilter where
  parseJSON = Aeson.withArray "StatusFilter" $ \arr ->
    (traverse . traverse) parseJSON (NEL.nonEmpty $ toList arr) >>= \case
      Nothing -> fail "Empty status filter"
      Just statuses -> pure $ StatusFilter $ NESet.fromList statuses

type GetHubEntriesCursor = (UTCTime, NotificationHubEntryId)

type GetHubEntriesEndpoint =
  AuthenticatedUserId
    :> QueryParam "limit" Int
    :> QueryParam "cursor" (Cursor GetHubEntriesCursor)
    :> QueryParam "status" StatusFilter
    :> Get '[JSON] (Paged GetHubEntriesCursor (NotificationHubEntry UnifiedDisplayInfo HydratedEvent))

type UpdateHubEntriesEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] UpdateHubEntriesRequest
    :> Patch '[JSON] ()

data UpdateHubEntriesRequest
  = UpdateHubEntriesRequest
  { notificationStatus :: NotificationStatus,
    notificationIds :: NESet NotificationHubEntryId
  }

instance FromJSON UpdateHubEntriesRequest where
  parseJSON = withObject "UpdateHubEntriesRequest" $ \o -> do
    notificationStatus <- o .: "status"
    notificationIds <- o .: "notificationIds"
    pure UpdateHubEntriesRequest {notificationStatus, notificationIds}

type CreateEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateEmailDeliveryMethodRequest
    :> Post '[JSON] CreateEmailDeliveryMethodResponse

data CreateEmailDeliveryMethodRequest
  = CreateEmailDeliveryMethodRequest
  { email :: Email
  }

instance FromJSON CreateEmailDeliveryMethodRequest where
  parseJSON = withObject "CreateEmailDeliveryMethodRequest" $ \o -> do
    email <- o .: "email"
    pure CreateEmailDeliveryMethodRequest {email}

data CreateEmailDeliveryMethodResponse
  = CreateEmailDeliveryMethodResponse
  { emailDeliveryMethodId :: NotificationEmailDeliveryMethodId
  }

instance ToJSON CreateEmailDeliveryMethodResponse where
  toJSON CreateEmailDeliveryMethodResponse {emailDeliveryMethodId} =
    object ["emailDeliveryMethodId" .= emailDeliveryMethodId]

instance FromJSON CreateEmailDeliveryMethodResponse where
  parseJSON = withObject "CreateEmailDeliveryMethodResponse" $ \o -> do
    emailDeliveryMethodId <- o .: "emailDeliveryMethodId"
    pure CreateEmailDeliveryMethodResponse {emailDeliveryMethodId}

type DeleteEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> Capture "emailDeliveryMethodId" NotificationEmailDeliveryMethodId
    :> Delete '[JSON] ()

type UpdateEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> Capture "emailDeliveryMethodId" NotificationEmailDeliveryMethodId
    :> ReqBody '[JSON] UpdateEmailDeliveryMethodRequest
    :> Patch '[JSON] ()

data UpdateEmailDeliveryMethodRequest
  = UpdateEmailDeliveryMethodRequest
  { email :: Email
  }

instance FromJSON UpdateEmailDeliveryMethodRequest where
  parseJSON = withObject "UpdateEmailDeliveryMethodRequest" $ \o -> do
    email <- o .: "email"
    pure UpdateEmailDeliveryMethodRequest {email}

type CreateWebhookEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateWebhookRequest
    :> Post '[JSON] CreateWebhookResponse

data CreateWebhookRequest
  = CreateWebhookRequest
  { url :: URIParam,
    name :: Text
  }

instance FromJSON CreateWebhookRequest where
  parseJSON = withObject "CreateWebhookRequest" $ \o -> do
    url <- o .: "url"
    name <- o .: "name"
    pure CreateWebhookRequest {url, name}

data CreateWebhookResponse
  = CreateWebhookResponse
  { webhookId :: NotificationWebhookId
  }

instance ToJSON CreateWebhookResponse where
  toJSON CreateWebhookResponse {webhookId} =
    object ["webhookId" .= webhookId]

instance FromJSON CreateWebhookResponse where
  parseJSON = withObject "CreateWebhookResponse" $ \o -> do
    webhookId <- o .: "webhookId"
    pure CreateWebhookResponse {webhookId}

type DeleteWebhookEndpoint =
  AuthenticatedUserId
    :> Capture "webhookId" NotificationWebhookId
    :> Delete '[JSON] ()

type UpdateWebhookEndpoint =
  AuthenticatedUserId
    :> Capture "webhookId" NotificationWebhookId
    :> ReqBody '[JSON] UpdateWebhookRequest
    :> Patch '[JSON] ()

data UpdateWebhookRequest
  = UpdateWebhookRequest
  { url :: URIParam
  }

instance FromJSON UpdateWebhookRequest where
  parseJSON = withObject "UpdateWebhookRequest" $ \o -> do
    url <- o .: "url"
    pure UpdateWebhookRequest {url}

type AddSubscriptionDeliveryMethodsEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] AddSubscriptionDeliveryMethodsRequest
    :> Post '[JSON] ()

type RemoveSubscriptionDeliveryMethodsEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] RemoveSubscriptionDeliveryMethodsRequest
    :> Delete '[JSON] ()

data AddSubscriptionDeliveryMethodsRequest
  = AddSubscriptionDeliveryMethodsRequest
  { deliveryMethods :: NESet DeliveryMethodId
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON AddSubscriptionDeliveryMethodsRequest where
  parseJSON = withObject "AddSubscriptionDeliveryMethodsRequest" $ \o -> do
    deliveryMethods <- o .: "deliveryMethods"
    pure AddSubscriptionDeliveryMethodsRequest {deliveryMethods}

data RemoveSubscriptionDeliveryMethodsRequest
  = RemoveSubscriptionDeliveryMethodsRequest
  { deliveryMethods :: NESet DeliveryMethodId
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON RemoveSubscriptionDeliveryMethodsRequest where
  parseJSON = withObject "RemoveSubscriptionDeliveryMethodsRequest" $ \o -> do
    deliveryMethods <- o .: "deliveryMethods"
    pure RemoveSubscriptionDeliveryMethodsRequest {deliveryMethods}
