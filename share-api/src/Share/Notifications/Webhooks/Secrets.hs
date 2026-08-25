-- | Access to the sensitive parts of a webhook's configuration.
--
-- This used to live in Vault, but is now just the 'uri' column on 'notification_webhooks'.
--
-- None of the queries in this module perform any authorization; a webhook is owned by the
-- subscriber of the subscription it belongs to, so callers must scope the webhook ids they pass
-- in by owner (see 'Share.Notifications.Queries.webhooksForSubscription' and friends) before
-- reading or writing a config on a user's behalf.
module Share.Notifications.Webhooks.Secrets
  ( putWebhookConfig,
    fetchWebhookConfig,
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
  = -- | Either there's no such webhook, or it has no URI set; a webhook whose URI hasn't been
    -- copied over from Vault yet will look like the latter.
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

data WebhookConfig
  = WebhookConfig
  { uri :: URIParam
  }
  deriving stock (Eq, Show)

-- | Set the config for an existing webhook, replacing whatever was there.
--
-- Webhooks are created with their URI already set (see
-- 'Share.Notifications.Queries.createWebhookDeliveryMethod'), so this is only for updates.
putWebhookConfig :: (PG.QueryA m) => NotificationWebhookId -> WebhookConfig -> m ()
putWebhookConfig webhookId (WebhookConfig {uri}) = do
  PG.execute_
    [PG.sql|
      UPDATE notification_webhooks
        SET uri = #{uri}
      WHERE id = #{webhookId}
    |]

-- | Fetch the config for the given webhook.
fetchWebhookConfig :: (PG.QueryM m) => NotificationWebhookId -> m (Either WebhookSecretError WebhookConfig)
fetchWebhookConfig webhookId = do
  PG.query1Col @(Maybe URIParam)
    [PG.sql|
      SELECT nw.uri
        FROM notification_webhooks nw
      WHERE nw.id = #{webhookId}
    |]
    -- The outer Maybe is "no such webhook", the inner one is "no URI on it".
    <&> join
    <&> \case
      Nothing -> Left $ MissingWebhookURI webhookId
      Just uri -> Right $ WebhookConfig {uri}
