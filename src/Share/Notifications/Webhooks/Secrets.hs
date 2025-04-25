module Share.Notifications.Webhooks.Secrets
  ( putWebhookSecret,
    fetchWebhookSecret,
    WebhookConfig (..),
  )
where

import Control.Monad.Except
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Servant.Client (ClientError, ClientM)
import Servant.Client qualified as ServantClient
import Share.App (AppM)
import Share.Env qualified as Env
import Share.IDs (NotificationWebhookId)
import Share.IDs qualified as IDs
import Share.Prelude
import Share.Utils.URI (URIParam)
import Vault (SecretPath (..), SecretRequest (..))
import Vault qualified

data WebhookSecretError
  = InvalidSecretJSON NotificationWebhookId Text
  | VaultError ClientError

runVaultClientM :: ClientM a -> AppM r (Either ClientError a)
runVaultClientM m = do
  clientEnv <- asks Env.vaultClientEnv
  liftIO $ ServantClient.runClientM m clientEnv

-- | Configuration for webhooks, which will be stashed in encrypted storage.
data WebhookConfig
  = WebhookConfig
  { uri :: URIParam,
    headers :: [(Text, Text)],
    method :: Text,
    body :: Maybe Text
  }

instance Aeson.ToJSON WebhookConfig where
  toJSON (WebhookConfig uri headers method body) =
    Aeson.object
      [ "uri" .= uri,
        "headers" .= headers,
        "method" .= method,
        "body" .= body
      ]

instance Aeson.FromJSON WebhookConfig where
  parseJSON = Aeson.withObject "WebhookConfig" $ \o -> do
    uri <- o .: "uri"
    headers <- o .: "headers"
    method <- o .: "method"
    body <- o .:? "body"
    return $ WebhookConfig uri headers method body

makeWebhookSecretPath :: NotificationWebhookId -> SecretPath
makeWebhookSecretPath webhookId =
  SecretPath $ Text.intercalate "/" ["webhooks", IDs.toText webhookId]

putWebhookSecret :: NotificationWebhookId -> WebhookConfig -> AppM r (Either WebhookSecretError ())
putWebhookSecret webhookId config = runExceptT do
  let secretPath = makeWebhookSecretPath webhookId
  shareVaultMount <- asks Env.shareVaultMount
  shareVaultToken <- asks Env.shareVaultToken
  let secretRequest = SecretRequest {options = Nothing, data_ = Aeson.toJSON config}
  withExceptT VaultError . ExceptT . runVaultClientM $ Vault.putSecret shareVaultToken shareVaultMount secretPath secretRequest
  pure ()

fetchWebhookSecret :: NotificationWebhookId -> AppM r (Either WebhookSecretError WebhookConfig)
fetchWebhookSecret webhookId = runExceptT do
  let secretPath = makeWebhookSecretPath webhookId
  shareVaultMount <- asks Env.shareVaultMount
  shareVaultToken <- asks Env.shareVaultToken
  Vault.SecretResponse {Vault.data_ = v} <- withExceptT VaultError . ExceptT . runVaultClientM $ Vault.fetchSecret shareVaultToken shareVaultMount secretPath
  case Aeson.fromJSON v of
    Aeson.Error err -> throwError $ InvalidSecretJSON webhookId (Text.pack err)
    Aeson.Success config -> pure config
