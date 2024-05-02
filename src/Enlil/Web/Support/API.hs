{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Support.API where

import Enlil.OAuth.Session
import Enlil.Web.Support.Types
import Servant

type API =
  "tickets"
    :> CreateTicketEndpoint

-- | POST /support/tickets
type CreateTicketEndpoint =
  AuthenticatedSession
    :> ReqBody '[JSON] SupportTicketRequest
    :> Post '[JSON] NoContent
