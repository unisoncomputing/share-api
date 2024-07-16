{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Identity Provider preset for Share.
module Share.OAuth.IdentityProvider.Share
  ( productionShareIdentityProvider,
    stagingShareIdentityProvider,
    localShareIdentityProvider,
    shareIdentityProviderForDeployment,
  )
where

import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Client.TLS qualified as HTTP
import Network.URI qualified as URI
import Servant
import Servant.Client
import Servant.Client.Core.Auth (AuthenticatedRequest (..))
import Share.OAuth.API
import Share.OAuth.IdentityProvider.Types (IdentityProviderConfig (..))
import Share.OAuth.Scopes
import Share.OAuth.Session
import Share.OAuth.Types
import Share.Utils.Deployment (Deployment (..))
import Share.Utils.URI (URIParam (..), addQueryParam)
import UnliftIO
import Web.Cookie (SetCookie)

runClientEither :: (MonadIO m) => BaseUrl -> ClientM a -> m (Either ClientError a)
runClientEither baseURL m = do
  httpClient <- liftIO $ HTTP.getGlobalManager
  let env = mkClientEnv httpClient baseURL
  resp <- liftIO $ runClientM m env
  pure resp

shareIdentityProviderForDeployment :: Deployment -> IdentityProviderConfig
shareIdentityProviderForDeployment = \case
  Production -> productionShareIdentityProvider
  Staging -> stagingShareIdentityProvider
  Local -> localShareIdentityProvider

-- | The share identity provider for use in production.
productionShareIdentityProvider :: IdentityProviderConfig
productionShareIdentityProvider =
  mkShareIdentityProvider
    (BaseUrl Https "api.unison-lang.org" 443 "")
    (fromJust . URI.parseURI $ "https://api.unison-lang.org/oauth/authorize")

-- | The share identity provider for use on staging.
stagingShareIdentityProvider :: IdentityProviderConfig
stagingShareIdentityProvider =
  mkShareIdentityProvider
    (BaseUrl Https "staging.api.unison-lang.org" 443 "")
    (fromJust . URI.parseURI $ "https://staging.api.unison-lang.org/oauth/authorize")

-- | Identity provider for the local development share instance.
localShareIdentityProvider :: IdentityProviderConfig
localShareIdentityProvider =
  mkShareIdentityProvider
    (BaseUrl Http "localhost" 5424 "")
    (fromJust . URI.parseURI $ "http://localhost:5424/oauth/authorize")

-- | Make an identity provider config.
mkShareIdentityProvider :: BaseUrl -> URI -> IdentityProviderConfig
mkShareIdentityProvider baseShareURL baseAuthorizationURI =
  IdentityProviderConfig
    { exchangeCodeForToken,
      authorizationURI
    }
  where
    -- Construct an authorization URL from the parameters required.
    authorizationURI :: URI -> OAuthClientId -> PendingSessionId -> PKCEChallenge -> PKCEChallengeMethod -> Scopes -> URI
    authorizationURI redirectURI clientId psid challenge pkceChallengeMethod scopes =
      baseAuthorizationURI
        & addQueryParam "state" psid
        & addQueryParam "redirect_uri" (URIParam redirectURI)
        & addQueryParam "response_type" ResponseTypeCode
        & addQueryParam "scope" scopes
        & addQueryParam "client_id" clientId
        & addQueryParam "code_challenge" challenge
        & addQueryParam "code_challenge_method" pkceChallengeMethod

    identityProviderAPI :: Proxy IdentityProviderAPI
    identityProviderAPI = Proxy

    _authorizationClient ::
      ResponseType ->
      OAuthClientId ->
      URIParam ->
      Scopes ->
      Text ->
      PKCEChallenge ->
      PKCEChallengeMethod ->
      AuthenticatedRequest MaybeAuthenticatedSession ->
      ClientM
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Location" String]
            NoContent
        )
    tokenClient ::
      TokenRequest ->
      ClientM (Headers '[Header "Cache-Control" String] TokenResponse)
    (_authorizationClient :<|> tokenClient) = client identityProviderAPI

    exchangeCodeForToken :: (MonadIO m) => OAuthClientId -> OAuthClientSecret -> Code -> URI -> PKCEVerifier -> m (Either ClientError TokenResponse)
    exchangeCodeForToken clientId clientSecret code redirectURI pkceVerifier = runClientEither baseShareURL $ do
      getResponse
        <$> tokenClient
          TokenRequest
            { grantType = AuthorizationCode,
              code,
              redirectURI = URIParam redirectURI,
              pkceVerifier,
              clientId,
              clientSecret = Just clientSecret
            }
