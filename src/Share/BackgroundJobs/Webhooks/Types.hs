{-# LANGUAGE StandaloneDeriving #-}

module Share.BackgroundJobs.Webhooks.Types
  ( WebhookSendFailure (..),
    WebhookEventPayload (..),
  )
where

import Control.Lens hiding ((.=))
import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Network.HTTP.Types qualified as HTTP
import Servant.Server
import Share.IDs
import Share.JWT (JWTParam (..))
import Share.JWT qualified as JWT
import Share.Notifications.Types
import Share.Notifications.Webhooks.Secrets (WebhookSecretError)
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import UnliftIO qualified

data WebhookSendFailure
  = ReceiverError NotificationEventId NotificationWebhookId HTTP.Status BL.ByteString
  | InvalidRequest NotificationEventId NotificationWebhookId UnliftIO.SomeException
  | WebhookSecretFetchError NotificationEventId NotificationWebhookId WebhookSecretError
  | JWTError NotificationEventId NotificationWebhookId JWTError
  deriving stock (Show)

instance ToServerError WebhookSendFailure where
  toServerError = \case
    ReceiverError _eventId _webhookId status _body ->
      ( ErrorID "webhook:receiver-error",
        err500
          { errBody =
              BL.fromStrict $
                Text.encodeUtf8 $
                  "Webhook receiver returned error status: "
                    <> Text.pack (show status)
          }
      )
    InvalidRequest _eventId _webhookId _err ->
      ( ErrorID "webhook:invalid-request",
        err400
          { errBody =
              BL.fromStrict $
                Text.encodeUtf8 $
                  "Invalid webhook request."
          }
      )
    WebhookSecretFetchError _eventId _webhookId _err ->
      ( ErrorID "webhook:secret-fetch-error",
        err500
          { errBody =
              BL.fromStrict $
                Text.encodeUtf8 $
                  "Failed to fetch webhook secret."
          }
      )
    JWTError _eventId _webhookId _err ->
      ( ErrorID "webhook:jwt-error",
        err500
          { errBody =
              BL.fromStrict $
                Text.encodeUtf8 $
                  "Failed to generate or verify JWT."
          }
      )

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

data WebhookEventPayload jwt = WebhookEventPayload
  { -- | The event ID of the notification event.
    eventId :: NotificationEventId,
    -- | The time at which the event occurred.
    occurredAt :: UTCTime,
    -- | The topic of the notification event.
    topic :: NotificationTopic,
    -- | The data associated with the notification event.
    data_ :: HydratedEvent,
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
  parseJSON = Aeson.withObject "WebhookEventPayload" $ \o -> do
    eventId <- o Aeson..: "eventId"
    occurredAt <- o Aeson..: "occurredAt"
    topic <- o Aeson..: "topic"
    data_ <- o Aeson..: "data"
    pure WebhookEventPayload {eventId, occurredAt, topic, data_, jwt = ()}
