{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.HistoryComments.API (API) where

import Servant

type API = NamedRoutes HistoryComments.Routes
