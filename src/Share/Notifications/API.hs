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
import Data.List.NonEmpty qualified as NEL
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Servant
import Share.Notifications.Types (NotificationHubEntry, NotificationStatus)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Prelude

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { getNotificationsEndpoint :: mode :- GetNotificationsEndpoint
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
