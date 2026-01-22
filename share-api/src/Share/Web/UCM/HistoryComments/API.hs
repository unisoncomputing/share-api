{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.HistoryComments.API (API) where

import Servant
import Unison.Server.HistoryComments.API qualified as HistoryComments

type API = NamedRoutes HistoryComments.Routes
