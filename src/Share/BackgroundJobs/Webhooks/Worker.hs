{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides the background worker for sending notification webhooks.
module Share.BackgroundJobs.Webhooks.Worker (worker) where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Ki.Unlifted qualified as Ki
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Types qualified as HTTP
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Webhooks.Queries qualified as WQ
import Share.BackgroundJobs.Webhooks.Queries qualified as WebhookQ
import Share.BackgroundJobs.Webhooks.Types (WebhookPayloadData)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Env qualified as Env
import Share.IDs (NotificationEventId, NotificationWebhookId)
import Share.JWT (JWTParam (..))
import Share.JWT qualified as JWT
import Share.Metrics qualified as Metrics
import Share.Notifications.Queries qualified as NQ
import Share.Notifications.Types (NotificationEvent (..), NotificationTopic, eventTopic)
import Share.Notifications.Webhooks.Secrets (WebhookConfig (..), WebhookSecretError)
import Share.Notifications.Webhooks.Secrets qualified as Webhooks
import Share.Postgres qualified as PG
import Share.Postgres.Notifications qualified as Notif
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.URI (URIParam (..))
import Share.Web.Authorization qualified as AuthZ
import UnliftIO qualified

data WebhookSendFailure
  = ReceiverError NotificationEventId NotificationWebhookId HTTP.Status BL.ByteString
  | InvalidRequest NotificationEventId NotificationWebhookId UnliftIO.SomeException
  | WebhookSecretFetchError NotificationEventId NotificationWebhookId WebhookSecretError
  | JWTError NotificationEventId NotificationWebhookId JWTError
  deriving stock (Show)

instance Logging.Loggable WebhookSendFailure where
  toLog = \case
    ReceiverError eventId webhookId status body ->
      Logging.textLog
        ( "Webhook receiver error: "
            <> Text.pack (show status)
            <> " "
            <> Text.decodeUtf8 (BL.toStrict body)
        )
        & Logging.withTag ("status", tShow status)
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.UserFault
    InvalidRequest eventId webhookId err ->
      Logging.textLog ("Invalid request: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.UserFault
    WebhookSecretFetchError eventId webhookId err ->
      Logging.textLog ("Failed to fetch webhook secret: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.Error
    JWTError eventId webhookId err ->
      Logging.textLog ("JWT error: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.Error

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60 -- 10 minutes

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "notifications:webhooks" $ forever do
    Notif.waitOnChannel Notif.WebhooksChannel (maxPollingIntervalSeconds * 1000000)
    processWebhooks authZReceipt
  where
    processWebhooks :: AuthZ.AuthZReceipt -> Background ()
    processWebhooks authZReceipt = do
      toIO <- UnliftIO.askRunInIO
      -- Need to unlift so we can use this in transactions
      let tryWebhookIO eventId eventData webhookId = toIO $ tryWebhook eventId eventData webhookId
      mayResult <- Metrics.recordWebhookSendingDuration $ PG.runTransaction $ runMaybeT $ do
        webhookInfo@(eventId, webhookId) <- MaybeT WQ.getUnsentWebhook
        mayErr <- lift $ attemptWebhookSend authZReceipt tryWebhookIO eventId webhookId
        pure (mayErr, webhookInfo)
      case mayResult of
        Just (Just err, _) -> do
          case err of
            WebhookSecretFetchError {} -> reportError err
            _ -> Logging.logErrorText $ "Webhook send errors: " <> tShow err
          -- If we got an error, we can retry it.
          -- TODO: Add some sort of backoff?
          processWebhooks authZReceipt
        (Just (Nothing, (eventId, webhookId))) -> do
          Logging.textLog ("Webhook sent successfully: " <> Text.pack (show webhookId))
            & Logging.withTag ("webhook_id", tShow webhookId)
            & Logging.withTag ("event_id", tShow eventId)
            & Logging.withSeverity Logging.Info
            & Logging.logMsg
          -- Keep processing releases until we run out of them.
          processWebhooks authZReceipt
        Nothing -> do
          -- No webhooks ready to try at the moment, we can wait till more are available.
          pure ()

--
webhookTimeout :: HTTPClient.ResponseTimeout
webhookTimeout = HTTPClient.responseTimeoutMicro (20 * 1000000 {- 20 seconds -})

data WebhookEventPayload jwt = WebhookEventPayload
  { -- | The event ID of the notification event.
    eventId :: NotificationEventId,
    -- | The time at which the event occurred.
    occurredAt :: UTCTime,
    -- | The topic of the notification event.
    topic :: NotificationTopic,
    -- | The data associated with the notification event.
    data_ :: WebhookPayloadData,
    -- | A signed token containing all of the same data.
    jwt :: jwt
  }
  deriving stock (Show, Eq)

deriving via JWT.JSONJWTClaims (WebhookEventPayload ()) instance JWT.AsJWTClaims (WebhookEventPayload ())

instance ToJSON (WebhookEventPayload JWTParam) where
  toJSON WebhookEventPayload {eventId, occurredAt, topic, data_, jwt} =
    Aeson.object
      [ "eventId" Aeson..= eventId,
        "occurredAt" Aeson..= occurredAt,
        "topic" Aeson..= topic,
        "data" Aeson..= data_,
        "signed" Aeson..= jwt
      ]

instance ToJSON (WebhookEventPayload ()) where
  toJSON WebhookEventPayload {eventId, occurredAt, topic, data_} =
    Aeson.object
      [ "eventId" Aeson..= eventId,
        "occurredAt" Aeson..= occurredAt,
        "topic" Aeson..= topic,
        "data" Aeson..= data_
      ]

instance FromJSON (WebhookEventPayload ()) where
  parseJSON = Aeson.withObject "WebhookEventPayload" $ \o ->
    WebhookEventPayload
      <$> o Aeson..: "eventId"
      <*> o Aeson..: "occurredAt"
      <*> o Aeson..: "topic"
      <*> o Aeson..: "data"
      <*> pure ()

tryWebhook ::
  NotificationEvent NotificationEventId UTCTime ->
  WebhookPayloadData ->
  NotificationWebhookId ->
  Background (Maybe WebhookSendFailure)
tryWebhook event eventData webhookId = UnliftIO.handleAny (\someException -> pure $ Just $ InvalidRequest event.eventId webhookId someException) do
  fmap (either Just (const Nothing)) $ runExceptT do
    proxiedHTTPManager <- asks Env.proxiedHttpClient
    WebhookConfig {uri = URIParam uri} <-
      lift (Webhooks.fetchWebhookConfig webhookId) >>= \case
        Left err -> throwError $ WebhookSecretFetchError event.eventId webhookId err
        Right config -> pure config
    jwtSettings <- asks Env.jwtSettings
    let payload =
          WebhookEventPayload
            { eventId = event.eventId,
              occurredAt = event.eventOccurredAt,
              topic = eventTopic event.eventData,
              data_ = eventData,
              jwt = ()
            }
    payloadJWT <-
      JWT.signJWT jwtSettings payload >>= \case
        Left jwtErr -> throwError $ JWTError event.eventId webhookId jwtErr
        Right jwt -> pure jwt
    let payloadWithJWT = payload {jwt = JWTParam payloadJWT}
    let reqResult =
          HTTPClient.requestFromURI uri <&> \req ->
            req
              { HTTPClient.method = "POST",
                HTTPClient.responseTimeout = webhookTimeout,
                HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
                HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode $ payloadWithJWT
              }
    req <- case reqResult of
      Left parseErr -> throwError $ InvalidRequest event.eventId webhookId parseErr
      Right req -> pure req
    resp <- liftIO $ HTTPClient.httpLbs req proxiedHTTPManager
    case HTTPClient.responseStatus resp of
      httpStatus@(HTTP.Status status _)
        | status >= 400 -> throwError $ ReceiverError event.eventId webhookId httpStatus $ HTTPClient.responseBody resp
        | otherwise -> pure ()

attemptWebhookSend ::
  AuthZ.AuthZReceipt ->
  (NotificationEvent NotificationEventId UTCTime -> WebhookPayloadData -> NotificationWebhookId -> IO (Maybe WebhookSendFailure)) ->
  NotificationEventId ->
  NotificationWebhookId ->
  PG.Transaction e (Maybe WebhookSendFailure)
attemptWebhookSend _authZReceipt tryWebhookIO eventId webhookId = do
  event <- NQ.expectEvent eventId
  eventData <- WebhookQ.hydrateEventData event.eventData
  PG.transactionUnsafeIO (tryWebhookIO event eventData webhookId) >>= \case
    Just err -> do
      WQ.recordFailedDeliveryAttempt eventId webhookId
      pure $ Just err
    Nothing -> do
      WQ.markWebhookAsDelivered eventId webhookId
      pure Nothing
