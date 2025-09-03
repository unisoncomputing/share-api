{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API
  ( API,
    Routes (..),
    HubEntriesRoutes (..),
    SubscriptionRoutes (..),
    GetHubEntriesCursor,
    StatusFilter (..),
    UpdateHubEntriesRequest (..),
    GetSubscriptionsResponse (..),
    CreateSubscriptionRequest (..),
    CreateSubscriptionResponse (..),
    UpdateSubscriptionRequest (..),
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
import Share.Notifications.Types (HydratedEvent, NotificationHubEntry, NotificationStatus, NotificationSubscription, NotificationTopic, NotificationTopicGroup, SubscriptionFilter)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude
import Share.Utils.API (Cursor, Paged)
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { hubRoutes :: mode :- "hub" :> NamedRoutes HubEntriesRoutes,
    subscriptionsRoutes :: mode :- "subscriptions" :> NamedRoutes SubscriptionRoutes
  }
  deriving stock (Generic)

data HubEntriesRoutes mode
  = HubEntriesRoutes
  { getHubEntriesEndpoint :: mode :- GetHubEntriesEndpoint,
    updateHubEntriesEndpoint :: mode :- UpdateHubEntriesEndpoint
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
    subscriptionProjectId :: Maybe ProjectId,
    subscriptionTopics :: Set NotificationTopic,
    subscriptionTopicGroups :: Set NotificationTopicGroup,
    subscriptionFilter :: Maybe SubscriptionFilter
  }

instance ToJSON CreateSubscriptionRequest where
  toJSON CreateSubscriptionRequest {subscriptionScope, subscriptionProjectId, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} =
    object
      [ "scope" .= subscriptionScope,
        "projectId" .= subscriptionProjectId,
        "topics" .= subscriptionTopics,
        "topicGroups" .= subscriptionTopicGroups,
        "filter" .= subscriptionFilter
      ]

instance FromJSON CreateSubscriptionRequest where
  parseJSON = withObject "CreateSubscriptionRequest" $ \o -> do
    subscriptionScope <- o .: "scope"
    subscriptionProjectId <- o .:? "projectId"
    subscriptionTopics <- o .: "topics"
    subscriptionTopicGroups <- o .: "topicGroups"
    subscriptionFilter <- o .:? "filter"
    pure CreateSubscriptionRequest {subscriptionScope, subscriptionProjectId, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter}

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

instance ToJSON UpdateSubscriptionRequest where
  toJSON UpdateSubscriptionRequest {subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} =
    object
      [ "topics" .= subscriptionTopics,
        "topicGroups" .= subscriptionTopicGroups,
        "filter" .= subscriptionFilter
      ]

instance FromJSON UpdateSubscriptionRequest where
  parseJSON = withObject "UpdateSubscriptionRequest" $ \o -> do
    subscriptionTopics <- o .:? "topics"
    subscriptionTopicGroups <- o .:? "topicGroups"
    subscriptionFilter <- o .:? "filter"
    pure UpdateSubscriptionRequest {subscriptionTopics, subscriptionTopicGroups, subscriptionFilter}

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

instance ToJSON StatusFilter where
  toJSON (StatusFilter statuses) =
    toList statuses
      & toJSON

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

instance ToJSON UpdateHubEntriesRequest where
  toJSON UpdateHubEntriesRequest {notificationStatus, notificationIds} =
    object
      [ "status" .= notificationStatus,
        "notificationIds" .= notificationIds
      ]

instance FromJSON UpdateHubEntriesRequest where
  parseJSON = withObject "UpdateHubEntriesRequest" $ \o -> do
    notificationStatus <- o .: "status"
    notificationIds <- o .: "notificationIds"
    pure UpdateHubEntriesRequest {notificationStatus, notificationIds}
