{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV2.API (API) where

import Servant
import Unison.SyncV2.API qualified as SyncV2

type API = NamedRoutes SyncV2.Routes
