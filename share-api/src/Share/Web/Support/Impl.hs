{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Support.Impl where

import Data.Time qualified as Time
import Servant
import Share.ChatApps
import Share.ChatApps qualified as ChatApps
import Share.Env qualified as Env
import Share.OAuth.Session
import Share.Prelude
import Share.Web.App
import Share.Web.Errors (Unimplemented (..), respondError)
import Share.Web.Support.API qualified as Support
import Share.Web.Support.Types

createTicketEndpoint :: Session -> SupportTicketRequest -> WebApp NoContent
createTicketEndpoint (Session {sessionUserId}) (SupportTicketRequest {subject, body}) = do
  author <- authorFromUserId sessionUserId
  now <- liftIO $ Time.getCurrentTime
  let message :: ChatApps.MessageContent 'ChatApps.Slack
      message =
        MessageContent
          { preText = "New Support Ticket",
            title = subject,
            content = body,
            author,
            mainLink = authorLink author,
            thumbnailUrl = authorAvatarUrl author,
            timestamp = now
          }
  webhookURI <- asks Env.supportTicketWebhookURI
  case webhookURI of
    Nothing -> do
      respondError Unimplemented
    Just uri -> do
      ChatApps.sendMessage uri message
      pure NoContent

server :: ServerT Support.API WebApp
server = createTicketEndpoint
