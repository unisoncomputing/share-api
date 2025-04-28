-- | This module provides the background worker for sending notification webhooks.
module Share.BackgroundJobs.Webhooks.Worker (worker) where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Data.Aeson (ToJSON)
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
import Share.BackgroundJobs.Workers (newWorker)
import Share.Env qualified as Env
import Share.IDs (NotificationEventId, NotificationWebhookId)
import Share.JWT (JWTParam)
import Share.Metrics qualified as Metrics
import Share.Notifications.Queries qualified as NQ
import Share.Notifications.Types (NotificationEvent (..), NotificationEventData, NotificationTopic, eventTopic)
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
      let tryWebhookIO eventId webhookId = toIO $ tryWebhook eventId webhookId
      (mayErr, maySentWebhook) <- Metrics.recordWebhookSendingDuration $ PG.runTransaction $ do
        mayWebhookInfo <- WQ.getUnsentWebhook
        mayErr <- runMaybeT $ (hoistMaybe mayWebhookInfo >>= (\(eventId, webhookId) -> MaybeT $ attemptWebhookSend authZReceipt tryWebhookIO eventId webhookId))
        pure (mayErr, mayWebhookInfo)
      case mayErr of
        Just err -> do
          case err of
            WebhookSecretFetchError {} -> reportError err
            _ -> Logging.logErrorText $ "Webhook send errors: " <> tShow err
        _ -> pure ()
      case maySentWebhook of
        Just (eventId, webhookId) -> do
          Logging.textLog ("Webhook sent successfully: " <> Text.pack (show webhookId))
            & Logging.withTag ("webhook_id", tShow webhookId)
            & Logging.withTag ("event_id", tShow eventId)
            & Logging.withSeverity Logging.Info
            & Logging.logMsg
          -- Keep processing releases until we run out of them.
          processWebhooks authZReceipt
        Nothing -> pure ()

--
webhookTimeout :: HTTPClient.ResponseTimeout
webhookTimeout = HTTPClient.responseTimeoutMicro (20 * 1000000 {- 20 seconds -})

data WebhookEventPayload = WebhookEventPayload
  { -- | The event ID of the notification event.
    eventId :: NotificationEventId,
    -- | The time at which the event occurred.
    occurredAt :: UTCTime,
    -- | The topic of the notification event.
    topic :: NotificationTopic,
    -- | The data associated with the notification event.
    data_ :: NotificationEventData,
    -- | A signed token containing all of the same data.
    jwt :: JWTParam
  }
  deriving stock (Show, Eq)

instance ToJSON WebhookEventPayload where
  toJSON WebhookEventPayload {eventId, occurredAt, topic, data_, jwt} =
    Aeson.object
      [ "eventId" Aeson..= eventId,
        "occurredAt" Aeson..= occurredAt,
        "topic" Aeson..= topic,
        "data" Aeson..= data_,
        "signed" Aeson..= jwt
      ]

tryWebhook ::
  NotificationEvent NotificationEventId UTCTime ->
  NotificationWebhookId ->
  Background (Maybe WebhookSendFailure)
tryWebhook event webhookId = do
  fmap (either Just (const Nothing)) $ runExceptT do
    proxiedHTTPManager <- asks Env.proxiedHttpClient
    WebhookConfig {uri = URIParam uri} <-
      lift (Webhooks.fetchWebhookConfig webhookId) >>= \case
        Left err -> throwError $ WebhookSecretFetchError event.eventId webhookId err
        Right config -> pure config
    let payloadJWT = error "TODO"
    let payload =
          WebhookEventPayload
            { eventId = event.eventId,
              occurredAt = event.eventOccurredAt,
              topic = eventTopic event.eventData,
              data_ = event.eventData,
              jwt = payloadJWT
            }
    let reqResult =
          HTTPClient.requestFromURI uri <&> \req ->
            req
              { HTTPClient.method = "POST",
                HTTPClient.responseTimeout = webhookTimeout,
                HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
                HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode $ payload
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
  (NotificationEvent NotificationEventId UTCTime -> NotificationWebhookId -> IO (Maybe WebhookSendFailure)) ->
  NotificationEventId ->
  NotificationWebhookId ->
  PG.Transaction e (Maybe WebhookSendFailure)
attemptWebhookSend _authZReceipt tryWebhookIO eventId webhookId = do
  event <- NQ.expectEvent eventId
  PG.transactionUnsafeIO (tryWebhookIO event webhookId) >>= \case
    Just err -> do
      WQ.recordFailedDeliveryAttempt eventId webhookId
      pure $ Just err
    Nothing -> do
      WQ.markWebhookAsDelivered eventId webhookId
      pure Nothing
