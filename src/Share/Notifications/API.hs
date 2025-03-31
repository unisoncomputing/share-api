{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API
  ( API,
    Routes (..),
    GetNotificationsResponse (..),
    StatusFilter (..),
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
import Share.IDs (NotificationSubscriptionId, UserHandle)
import Share.Notifications.Types (NotificationDeliveryMechanism, NotificationHubEntry, NotificationStatus, NotificationSubscription)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { getNotificationsEndpoint :: mode :- GetNotificationsEndpoint,
    getDeliveryMechanismsEndpoint :: mode :- GetDeliveryMechanismsEndpoint,
    subscriptionsRoutes :: Routes mode,
    emailDeliveryRoutes :: Routes mode,
    webhookDeliveryRoutes :: Routes mode,
    hubManagementRoutes :: Routes mode
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

type GetDeliveryMechanismsEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] GetDeliveryMechanismsResponse

data GetDeliveryMechanismsResponse
  = GetDeliveryMechanismsResponse
  { deliveryMechanisms :: Set NotificationDeliveryMechanism
  }

instance ToJSON GetDeliveryMechanismsResponse where
  toJSON GetDeliveryMechanismsResponse {deliveryMechanisms} =
    object ["deliveryMechanisms" .= deliveryMechanisms]

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

type GetNotificationsEndpoint =
  AuthenticatedUserId
    :> QueryParam "limit" Int
    :> QueryParam "after" UTCTime
    :> QueryParam "status" StatusFilter
    :> Get '[JSON] GetNotificationsResponse

data GetNotificationsResponse = GetNotificationsResponse
  { notifications :: [NotificationHubEntry]
  }

instance ToJSON GetNotificationsResponse where
  toJSON GetNotificationsResponse {notifications} =
    object ["notifications" .= notifications]
