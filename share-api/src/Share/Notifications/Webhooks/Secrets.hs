module Share.Notifications.Webhooks.Secrets
  ( putWebhookConfig,
    fetchWebhookConfig,
    deleteWebhookConfig,
    WebhookConfig (..),
    WebhookSecretError (..),
  )
where

import Control.Monad.Except
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant (ServerError (..))
import Servant.Client (ClientError, ClientM)
import Servant.Client qualified as ServantClient
import Servant.Server (err500)
import Share.App (AppM)
import Share.Env qualified as Env
import Share.IDs 
import Share.IDs qualified as IDs
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.URI (URIParam)
import Share.Web.Errors (ToServerError (..))
import Vault (SecretPath (..), SecretRequest (..))
import Vault qualified

data WebhookSecretError
  = InvalidSecretJSON NotificationWebhookId Text
  | VaultError ClientError
  deriving stock (Eq, Show)

instance Logging.Loggable WebhookSecretError where
  toLog = \case
    InvalidSecretJSON webhookId err ->
      (Logging.textLog $ "Invalid JSON in webhook config for " <> IDs.toText webhookId <> ": " <> err)
        & Logging.withSeverity Logging.Error
    VaultError err ->
      (Logging.textLog $ "Vault request failed: " <> Text.pack (show err))
        & Logging.withSeverity Logging.Error

instance ToServerError WebhookSecretError where
  toServerError = \case
    InvalidSecretJSON webhookId err ->
      ("webhook:vault:invalid-webhook-config", err500 {errBody = BL.fromStrict $ Text.encodeUtf8 $ "Invalid JSON in webhook config for " <> IDs.toText webhookId <> ": " <> err})
    VaultError err ->
      ("webhook:vault:request-failed", err500 {errBody = BL.fromStrict $ Text.encodeUtf8 $ "Vault request failed: " <> Text.pack (show err)})

runVaultClientM :: ClientM a -> AppM r (Either ClientError a)
runVaultClientM m = do
  clientEnv <- asks Env.vaultClientEnv
  liftIO $ ServantClient.runClientM m clientEnv

-- | Configuration for webhooks, which will be stashed in encrypted storage.
data WebhookConfig
  = WebhookConfig
  { uri :: URIParam
  }

instance Aeson.ToJSON WebhookConfig where
  toJSON (WebhookConfig uri) =
    Aeson.object
      [ "uri" .= uri
      ]

instance Aeson.FromJSON WebhookConfig where
  parseJSON = Aeson.withObject "WebhookConfig" $ \o -> do
    uri <- o .: "uri"
    return $ WebhookConfig uri

makeWebhookSecretPath :: NotificationWebhookId -> SecretPath
makeWebhookSecretPath webhookId =
  SecretPath $ Text.intercalate "/" ["webhooks", IDs.toText webhookId]

putWebhookConfig :: NotificationWebhookId -> WebhookConfig -> AppM r (Either WebhookSecretError ())
putWebhookConfig webhookId config = runExceptT do
  let secretPath = makeWebhookSecretPath webhookId
  userSecretsVaultMount <- asks Env.userSecretsVaultMount
  shareVaultToken <- asks Env.shareVaultToken
  let secretRequest = SecretRequest {options = Nothing, data_ = Aeson.toJSON config}
  withExceptT VaultError . ExceptT . runVaultClientM $ Vault.putSecret shareVaultToken userSecretsVaultMount secretPath secretRequest
  pure ()

fetchWebhookConfig :: NotificationWebhookId -> AppM r (Either WebhookSecretError WebhookConfig)
fetchWebhookConfig webhookId = runExceptT do
  let secretPath = makeWebhookSecretPath webhookId
  userSecretsVaultMount <- asks Env.userSecretsVaultMount
  shareVaultToken <- asks Env.shareVaultToken
  Vault.SecretResponse {Vault.data_ = Vault.SecretData v} <- withExceptT VaultError . ExceptT . runVaultClientM $ Vault.fetchSecret shareVaultToken userSecretsVaultMount secretPath
  case Aeson.fromJSON v of
    Aeson.Error err -> throwError $ InvalidSecretJSON webhookId (Text.pack err)
    Aeson.Success config -> pure config

deleteWebhookConfig :: NotificationWebhookId -> AppM r (Either WebhookSecretError ())
deleteWebhookConfig webhookId = runExceptT do
  let secretPath = makeWebhookSecretPath webhookId
  userSecretsVaultMount <- asks Env.userSecretsVaultMount
  shareVaultToken <- asks Env.shareVaultToken
  withExceptT VaultError . ExceptT . runVaultClientM $ Vault.deleteSecret shareVaultToken userSecretsVaultMount secretPath
  pure ()
