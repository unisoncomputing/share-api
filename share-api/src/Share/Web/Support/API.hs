{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Support.API where

import Share.OAuth.Session
import Share.Web.Support.Types
import Servant

type API =
  "tickets"
    :> CreateTicketEndpoint

-- | POST /support/tickets
type CreateTicketEndpoint =
  AuthenticatedSession
    :> ReqBody '[JSON] SupportTicketRequest
    :> Post '[JSON] NoContent
