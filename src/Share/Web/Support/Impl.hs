{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Support.Impl where

import Servant
import Share.ChatApps
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres.Ops qualified as PGO
import Share.Prelude
import Share.User
import Share.Web.App
import Share.Web.Support.API qualified as Support
import Share.Web.Support.Types
import Share.Web.Support.Zendesk qualified as Zendesk

createTicketEndpoint :: Session -> SupportTicketRequest -> WebApp NoContent
createTicketEndpoint (Session {sessionUserId}) (SupportTicketRequest {subject, body, priority}) = do
  User {user_name, user_email, handle} <- PGO.expectUserById sessionUserId
  let message :: MessageContent 'Slack
      message =
        MessageContent
          { preText = "New Support Ticket",
            title = subject,
            content = body
          }
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
