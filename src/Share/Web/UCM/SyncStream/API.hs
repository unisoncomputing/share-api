{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncStream.API (API) where

import Servant
import Share.OAuth.Session (MaybeAuthenticatedUserId)
import Unison.SyncV2.API qualified as SyncV2

type API = MaybeAuthenticatedUserId :> NamedRoutes SyncV2.Routes
