{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.API
  ( API,
    Routes (..),
  )
where

import GHC.Generics (Generic)
import Servant
import Servant.API.WebSocket (WebSocket)

data Routes mode = Routes
  { downloadEntities :: mode :- "download" :> DownloadEntitiesEndpoint
  }
  deriving stock (Generic)

type API = NamedRoutes Routes

type DownloadEntitiesEndpoint = WebSocket
