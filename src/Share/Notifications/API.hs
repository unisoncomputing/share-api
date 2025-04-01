{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API
  ( API,
    Routes (..),
    GetHubEntriesResponse (..),
    StatusFilter (..),
    UpdateHubEntryRequest (..),
    GetSubscriptionsResponse (..),
    CreateSubscriptionRequest (..),
    CreateSubscriptionResponse (..),
    UpdateSubscriptionRequest (..),
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
import Share.Notifications.Types (NotificationDeliveryMethod, NotificationHubEntry, NotificationStatus, NotificationSubscription)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude
import Share.Utils.URI (URIParam)

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { hubRoutes :: "hub" :> HubEntriesRoutes mode,
    deliveryMethodRoutes :: "delivery-methods" :> DeliveryMethodRoutes mode,
    subscriptionsRoutes :: "subscriptions" :> SubscriptionRoutes mode
  }
  deriving stock (Generic)

data HubEntriesRoutes mode
  = HubEntriesRoutes
  { getHubEntriesEndpoint :: mode :- GetHubEntriesEndpoint,
    updateHubEntryEndpoint :: mode :- UpdateHubEntryEndpoint
  }
  deriving stock (Generic)

data DeliveryMethodRoutes mode
  = DeliveryMethodRoutes
  { getDeliveryMethodsEndpoint :: mode :- GetDeliveryMethodsEndpoint,
    emailDeliveryRoutes :: "emails" :> EmailRoutes mode,
    webhookDeliveryRoutes :: "webhooks" :> WebhookRoutes mode
  }
  deriving stock (Generic)

data SubscriptionRoutes mode
  = SubscriptionRoutes
  { getSubscriptionsEndpoint :: mode :- GetSubscriptionsEndpoint,
    createSubscriptionEndpoint :: mode :- CreateSubscriptionEndpoint,
    deleteSubscriptionEndpoint :: mode :- DeleteSubscriptionEndpoint,
    updateSubscriptionEndpoint :: mode :- UpdateSubscriptionEndpoint
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

type CreateSubscriptionEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateSubscriptionRequest
    :> Post '[JSON] CreateSubscriptionResponse

data CreateSubscriptionRequest
  = CreateSubscriptionRequest
  { subscriptionScope :: UserHandle,
    subscriptionTopics :: NESet Text,
    subscriptionFilter :: StatusFilter
  }

instance FromJSON CreateSubscriptionRequest where
  parseJSON = withObject "CreateSubscriptionRequest" $ \o -> do
    subscriptionScope <- o .: "scope"
    subscriptionTopics <- o .: "topics"
    subscriptionFilter <- o .: "filter"
    pure CreateSubscriptionRequest {subscriptionScope, subscriptionTopics, subscriptionFilter}

data CreateSubscriptionResponse
  = CreateSubscriptionResponse
  { subscription :: NotificationSubscription NotificationSubscriptionId
  }

instance ToJSON CreateSubscriptionResponse where
  toJSON CreateSubscriptionResponse {subscription} =
    object ["subscription" .= subscription]

type DeleteSubscriptionEndpoint =
  AuthenticatedUserId
    :> Capture "subscription_id" NotificationSubscriptionId
    :> Delete '[JSON] NoContent

type UpdateSubscriptionEndpoint =
  AuthenticatedUserId
    :> Capture "subscription_id" NotificationSubscriptionId
    :> ReqBody '[JSON] UpdateSubscriptionRequest
    :> Patch '[JSON] CreateSubscriptionResponse

data UpdateSubscriptionRequest
  = UpdateSubscriptionRequest
  { subscriptionTopics :: Maybe (NESet Text),
    subscriptionFilter :: Maybe StatusFilter
  }

instance FromJSON UpdateSubscriptionRequest where
  parseJSON = withObject "UpdateSubscriptionRequest" $ \o -> do
    subscriptionTopics <- o .:? "topics"
    subscriptionFilter <- o .:? "filter"
    pure UpdateSubscriptionRequest {subscriptionTopics, subscriptionFilter}

type GetDeliveryMethodsEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] GetDeliveryMethodsResponse

data GetDeliveryMethodsResponse
  = GetDeliveryMethodsResponse
  { deliveryMethods :: Set NotificationDeliveryMethod
  }

instance ToJSON GetDeliveryMethodsResponse where
  toJSON GetDeliveryMethodsResponse {deliveryMethods} =
    object ["deliveryMethods" .= deliveryMethods]

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

instance FromJSON StatusFilter where
  parseJSON = Aeson.withArray "StatusFilter" $ \arr ->
    (traverse . traverse) parseJSON (NEL.nonEmpty $ toList arr) >>= \case
      Nothing -> fail "Empty status filter"
      Just statuses -> pure $ StatusFilter $ NESet.fromList statuses

type GetHubEntriesEndpoint =
  AuthenticatedUserId
    :> QueryParam "limit" Int
    :> QueryParam "after" UTCTime
    :> QueryParam "status" StatusFilter
    :> Get '[JSON] GetHubEntriesResponse

data GetHubEntriesResponse = GetHubEntriesResponse
  { notifications :: [NotificationHubEntry]
  }

instance ToJSON GetHubEntriesResponse where
  toJSON GetHubEntriesResponse {notifications} =
    object ["notifications" .= notifications]

type UpdateHubEntryEndpoint =
  AuthenticatedUserId
    :> Capture "hubEntryId" NotificationHubEntryId
    :> ReqBody '[JSON] UpdateHubEntryRequest
    :> Patch '[JSON] NoContent

data UpdateHubEntryRequest
  = UpdateHubEntryRequest
  { notificationStatus :: NotificationStatus
  }

type CreateEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateEmailDeliveryMethodRequest
    :> Post '[JSON] CreateEmailDeliveryMethodResponse

data CreateEmailDeliveryMethodRequest
  = CreateEmailDeliveryMethodRequest
  { email :: Text
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

type DeleteEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> Capture "emailDeliveryMethodId" NotificationEmailDeliveryMethodId
    :> Delete '[JSON] NoContent

type UpdateEmailDeliveryMethodEndpoint =
  AuthenticatedUserId
    :> Capture "emailDeliveryMethodId" NotificationEmailDeliveryMethodId
    :> ReqBody '[JSON] UpdateEmailDeliveryMethodRequest
    :> Patch '[JSON] NoContent

data UpdateEmailDeliveryMethodRequest
  = UpdateEmailDeliveryMethodRequest
  { email :: Text
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
  { url :: URIParam
  }

instance FromJSON CreateWebhookRequest where
  parseJSON = withObject "CreateWebhookRequest" $ \o -> do
    url <- o .: "url"
    pure CreateWebhookRequest {url}

data CreateWebhookResponse
  = CreateWebhookResponse
  { webhookDeliveryMethodId :: NotificationWebhookId
  }

type DeleteWebhookEndpoint =
  AuthenticatedUserId
    :> Capture "webhookDeliveryMethodId" NotificationWebhookId
    :> Delete '[JSON] NoContent

type UpdateWebhookEndpoint =
  AuthenticatedUserId
    :> Capture "webhookDeliveryMethodId" NotificationWebhookId
    :> ReqBody '[JSON] UpdateWebhookRequest
    :> Patch '[JSON] NoContent

data UpdateWebhookRequest
  = UpdateWebhookRequest
  { url :: URIParam
  }
