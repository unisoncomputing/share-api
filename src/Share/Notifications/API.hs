{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Notifications.API (API, Routes (..), GetNotificationsResponse (..)) where

import Data.Aeson
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant
import Share.Notifications.Types (NotificationHubEntry)

type API = NamedRoutes Routes

data Routes mode
  = Routes
  { getNotificationsEndpoint :: mode :- GetNotificationsEndpoint
  }
  deriving stock (Generic)

type GetNotificationsEndpoint =
  QueryParam "limit" Int
    :> QueryParam "before" UTCTime
    :> Get '[JSON] GetNotificationsResponse

data GetNotificationsResponse = GetNotificationsResponse
  { notifications :: [NotificationHubEntry]
  }

instance ToJSON GetNotificationsResponse where
  toJSON GetNotificationsResponse {notifications} =
    object ["notifications" .= notifications]
