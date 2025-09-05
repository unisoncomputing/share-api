{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API
  ( API,
    Routes (..),
    HubEntriesRoutes (..),
    GetHubEntriesCursor,
    StatusFilter (..),
    UpdateHubEntriesRequest (..),
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
import Share.Notifications.Types (HydratedEvent, NotificationHubEntry, NotificationStatus)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude
import Share.Utils.API (Cursor, Paged)
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { hubRoutes :: mode :- "hub" :> NamedRoutes HubEntriesRoutes
  }
  deriving stock (Generic)

data HubEntriesRoutes mode
  = HubEntriesRoutes
  { getHubEntriesEndpoint :: mode :- GetHubEntriesEndpoint,
    updateHubEntriesEndpoint :: mode :- UpdateHubEntriesEndpoint
  }
  deriving stock (Generic)

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
