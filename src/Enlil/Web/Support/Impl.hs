{-# LANGUAGE RecordWildCards #-}

module Enlil.Web.Support.Impl where

import Enlil.IDs qualified as IDs
import Enlil.OAuth.Session
import Enlil.Postgres.Ops qualified as PGO
import Enlil.Prelude
import Enlil.User
import Enlil.Web.App
import Enlil.Web.Support.API qualified as Support
import Enlil.Web.Support.Types
import Enlil.Web.Support.Zendesk qualified as Zendesk
import Servant

createTicketEndpoint :: Session -> SupportTicketRequest -> WebApp NoContent
createTicketEndpoint (Session {sessionUserId}) (SupportTicketRequest {..}) = do
  User {user_name, user_email, handle} <- PGO.expectUserById sessionUserId
  let zendeskTicket =
        Zendesk.ZendeskTicket
          { subject = subject,
            body = body,
            priority = priority,
            requesterName = fromMaybe (IDs.toText handle) user_name,
            requesterEmail = user_email,
            shareHandle = handle,
            shareUserId = sessionUserId
          }
  _success <- Zendesk.createTicket zendeskTicket
  pure NoContent

server :: ServerT Support.API WebApp
server = createTicketEndpoint
