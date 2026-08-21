-- | Storage for the sensitive parts of a webhook's configuration.
--
-- This used to live in Vault, but is now kept in the 'notification_webhook_uris' table.
--
-- None of the queries in this module perform any authorization; a webhook is owned by the
-- subscriber of the subscription it belongs to, so callers must scope the webhook ids they pass
-- in by owner (see 'Share.Notifications.Queries.webhooksForSubscription' and friends) before
-- reading or writing a config on a user's behalf.
module Share.Notifications.Webhooks.Secrets
  ( putWebhookConfig,
    fetchWebhookConfig,
    deleteWebhookConfig,
    WebhookConfig (..),
    WebhookSecretError (..),
  )
where

import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as Text
import Servant (ServerError (..))
import Servant.Server (err500)
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.URI (URIParam)
import Share.Web.Errors (ErrorID (..), ToServerError (..))

data WebhookSecretError
  = -- | The webhook exists, but has no URI stored for it.
    MissingWebhookURI NotificationWebhookId
  deriving stock (Eq, Show)

instance Logging.Loggable WebhookSecretError where
  toLog = \case
    MissingWebhookURI webhookId ->
      (Logging.textLog $ "No URI stored for webhook " <> IDs.toText webhookId)
        & Logging.withTag ("webhook_id", IDs.toText webhookId)
        & Logging.withSeverity Logging.Error

instance ToServerError WebhookSecretError where
  toServerError = \case
    MissingWebhookURI webhookId ->
      ( ErrorID "webhook:missing-webhook-uri",
        err500 {errBody = BL.fromStrict $ Text.encodeUtf8 $ "No URI stored for webhook " <> IDs.toText webhookId}
      )

-- | The parts of a webhook's configuration which we keep out of the main webhooks table.
data WebhookConfig
  = WebhookConfig
  { uri :: URIParam
  }
  deriving stock (Eq, Show)

-- | Set the config for the given webhook, replacing any existing config.
putWebhookConfig :: (PG.QueryA m) => NotificationWebhookId -> WebhookConfig -> m ()
putWebhookConfig webhookId (WebhookConfig {uri}) = do
  PG.execute_
    [PG.sql|
      INSERT INTO notification_webhook_uris (webhook_id, uri)
        VALUES (#{webhookId}, #{uri})
      ON CONFLICT (webhook_id)
        DO UPDATE SET uri = excluded.uri
    |]

-- | Fetch the config for the given webhook.
fetchWebhookConfig :: (PG.QueryM m) => NotificationWebhookId -> m (Either WebhookSecretError WebhookConfig)
fetchWebhookConfig webhookId = do
  PG.query1Col
    [PG.sql|
      SELECT nwu.uri
        FROM notification_webhook_uris nwu
      WHERE nwu.webhook_id = #{webhookId}
    |]
    <&> \case
      Nothing -> Left $ MissingWebhookURI webhookId
      Just uri -> Right $ WebhookConfig {uri}

-- | Delete the config for the given webhook.
--
-- Note that deleting the webhook itself (or its subscription) already cascades to its config,
-- this is only needed if you want to drop the config while keeping the webhook around.
deleteWebhookConfig :: (PG.QueryA m) => NotificationWebhookId -> m ()
deleteWebhookConfig webhookId = do
  PG.execute_
    [PG.sql|
      DELETE FROM notification_webhook_uris
      WHERE webhook_id = #{webhookId}
    |]
