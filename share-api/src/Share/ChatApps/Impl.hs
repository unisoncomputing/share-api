{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Share.ChatApps.Impl
  ( sendMessage,
    reportError,
  )
where

import Control.Monad.Trans.Except (except, runExceptT)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Time qualified as Time
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI)
import Share.ChatApps.Types
import Share.Env.Types qualified as Env
import Share.Prelude

chatAppTimeout :: HTTPClient.ResponseTimeout
chatAppTimeout = HTTPClient.responseTimeoutMicro $ 10 * 1_000_000 -- 10 seconds

sendMessage :: (MonadIO m, ToJSON (MessageContent provider)) => Env.Env ctx -> URI -> MessageContent provider -> m (Either ChatAppFailure ())
sendMessage env uri messageContent = runExceptT do
  req <-
    HTTPClient.requestFromURI uri
      & mapLeft InvalidRequest
      & except
  let req' =
        req
          { HTTPClient.method = "POST",
            HTTPClient.responseTimeout = chatAppTimeout,
            HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
            HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode messageContent
          }
  let proxiedHTTPManager = Env.proxiedHttpClient env
  resp <- liftIO $ HTTPClient.httpLbs req' proxiedHTTPManager
  case HTTPClient.responseStatus resp of
    httpStatus@(HTTP.Status status _)
      | status >= 400 -> throwError $ ErrorResponse httpStatus
      | otherwise -> pure ()

reportError :: (MonadIO m) => Env.Env ctx -> Text -> m ()
reportError env errBody = do
  now <- liftIO $ Time.getCurrentTime
  let message :: MessageContent 'Discord
      message =
        MessageContent
          { preText = "Uncaught Exception in Share Prod",
            title = "Uncaught Exception in Share Prod",
            content = errBody,
            author = Author (Just "Share Prod") Nothing Nothing,
            mainLink = Nothing,
            thumbnailUrl = Nothing,
            timestamp = now
          }
  let webhookURI = Env.supportTicketWebhookURI env
  case webhookURI of
    Nothing -> pure ()
    Just uri -> do
      _ <- sendMessage env uri message
      pure ()
