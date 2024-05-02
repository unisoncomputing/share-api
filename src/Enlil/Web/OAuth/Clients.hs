module Enlil.Web.OAuth.Clients where

import Enlil.OAuth.Errors qualified as OAuthError
import Enlil.OAuth.Types (OAuthClientConfig (..), OAuthClientId (..), OAuthClientSecret)
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Queries qualified as Q
import Enlil.Prelude
import Enlil.Utils.URI (URIParam (..))
import Enlil.Web.App
import Enlil.Web.Errors
import Network.URI (URI (..), URIAuth (..))

redirectValidator :: OAuthClientConfig -> URI -> Bool
redirectValidator OAuthClientConfig {redirectHost = URIParam authorizedHost} redirectURI =
  isJust $ do
    redirectHost <- uriAuthority redirectURI <&> uriRegName
    cloudHost <- uriAuthority authorizedHost <&> uriRegName
    guard $ redirectHost == cloudHost

-- | Validate that the client is registered and that the redirect URI is valid for that client.
validateOAuthClientRedirectURI :: OAuthClientId -> URIParam -> WebApp OAuthClientConfig
validateOAuthClientRedirectURI clientId (URIParam redirectURI) = do
  PG.runTransaction (Q.getOAuthConfigForClient clientId) >>= \case
    Nothing -> respondError (OAuthError.UnknownClient clientId)
    Just config -> do
      when (not $ redirectValidator config redirectURI) $
        respondError (OAuthError.UnregisteredRedirectURI clientId (URIParam redirectURI))
      pure config

-- | Validate that the client is registered and that the redirect URI is valid for that client.
validateOAuthClientForTokenExchange :: OAuthClientId -> Maybe OAuthClientSecret -> URIParam -> WebApp OAuthClientConfig
validateOAuthClientForTokenExchange clientId clientSecretFromRequest (URIParam redirectURI) = do
  validateOAuthClientRedirectURI clientId (URIParam redirectURI) >>= \case
    config@OAuthClientConfig {clientSecret = clientSecretFromConfig} ->
      if clientSecretFromRequest == clientSecretFromConfig
        then pure config
        else respondError (OAuthError.MismatchedClientSecret clientId)
