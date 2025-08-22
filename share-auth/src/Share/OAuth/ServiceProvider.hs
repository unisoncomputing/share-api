{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.OAuth.ServiceProvider
  ( ServiceProviderConfig (..),
    ServiceProviderAPI,
    SessionCallback,
    SessionCallbackData (..),
    serviceProviderServer,
    Cookies.defaultCookieSettings,
    JWT.defaultJWTSettings,
    JWT.JWTSettings,
    verifySessionToken,
    -- Endpoints
    loginEndpoint,
    loginEndpointWithData,
    logoutEndpoint,
    redirectReceiverEndpoint,
  )
where

import Control.Monad.Except
import Control.Monad.Trans (MonadTrans (..))
import Crypto.JWT (JWTError)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Redis qualified as Redis
import Servant
import Share.JWT (JWTParam (..))
import Share.JWT qualified as JWT
import Share.OAuth.API
import Share.OAuth.IdentityProvider.Types (IdentityProviderConfig (IdentityProviderConfig), authorizationURI, exchangeCodeForToken)
import Share.OAuth.PKCE qualified as PKCE
import Share.OAuth.Redis qualified as Redis
import Share.OAuth.Scopes
import Share.OAuth.Session
import Share.OAuth.Session qualified as Session
import Share.OAuth.Types
import Share.Utils.IDs
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Utils.URI (URIParam, setPathAndQueryParams, unpackURI)
import UnliftIO
import Web.Cookie (SetCookie (..))

data ServiceProviderConfig = ServiceProviderConfig
  { -- | The settings used to sign and verify cookies. See 'defaultCookieSettings'
    cookieSettings :: Cookies.CookieSettings,
    -- | The settings used to sign and verify JWTs. See 'defaultJWTSettings'
    jwtSettings :: JWT.JWTSettings,
    -- | Where to redirect users after logging them out.
    redirectAfterLogout :: URI,
    -- | The client id used to authenticate with the identity provider.
    oauthClientID :: OAuthClientId,
    -- | The client secret used to authenticate with the identity provider.
    oauthClientSecret :: OAuthClientSecret,
    -- | The scopes we're requesting from the identity provider.
    -- Note that `openid` is typically required.
    scopes :: Scopes,
    -- | The base uri of the service provider where you've mounted the oauth endpoints.
    -- e.g. https://api.unison.cloud
    baseServiceURI :: URI,
    -- | The audience to stamp on tokens we issue via the identity provider.
    -- This must match what's configured in the identity provider's client config.
    -- E.g. api.unison.cloud
    serviceAudience :: JWT.Audience,
    -- | Name to use for session cookies.
    sessionCookieKey :: Text
  }

data SessionCallbackData = SessionCallbackData
  { session :: Session,
    -- The URI the user started the login flow from, you may wish to redirect them back here
    returnToURI :: Maybe URI,
    -- Additional data passed to the login endpoint
    additionalData :: Maybe Aeson.Value
  }

-- | Provides the application with the user's authenticated session.
-- The caller should provide a URI to redirect the user to.
type SessionCallback m =
  Either RedirectReceiverErr SessionCallbackData ->
  m URI

-- | Mount this server at the base URI of your service provider as an implementation of the 'ServiceProviderAPI'
--
-- E.g.
--
-- @@
--
--  type MyAPI = ServiceProviderAPI :<|> OtherRoutes...
--  myServiceProviderConfig :: ServiceProviderConfig
--  myServiceProviderConfig = ServiceProviderConfig
--                              { cookieSettings = defaultCookieSettings onLocal
--                              , jwtSettings = defaultJWTSettings hs256Key "https://api.unison.cloud"
--                              , redirectAfterLogout = "https://unison.cloud"
--                              , oauthClientID = "my-client-id"
--                              , oauthClientSecret = "my-client-secret"
--                              , scopes = Scopes (Set.fromList [Scopes.OpenId, Scopes.Cloud])
--                              , baseServiceURI = "https://api.unison.cloud"
--                              , issuer = "https://api.unison-lang.org"
--                              , audience = "api.unison.cloud"
--                              }
--  mySessionCallback :: Either RedirectReceiverErr Session -> AppM URI
--  mySessionCallback (Left err) = pure $ linkToMyErrorPage err
--  mySessionCallback (Right Session{sessionUserId}) = pure $ linkToMyHomePage sessionUserId
--  myServer = serviceProviderServer myServiceProviderConfig mySessionCallback
--             :<|> otherRoutes...
-- @@
serviceProviderServer ::
  (Redis.MonadRedis m, MonadIO m) =>
  -- | The configuration for the identity provider we're authenticating against.
  -- See Share.OAuth.IdentityProvider.Share for presets to use
  IdentityProviderConfig ->
  -- | The configuration of your service. See 'ServiceProviderConfig'
  ServiceProviderConfig ->
  -- | A callback which will be called with the user's session after they've logged in, or in
  -- the case of an error.
  -- Must return the URI to redirect the user to, typically the main page or an error page
  -- depending on the result passed to the callback.
  SessionCallback m ->
  ServerT ServiceProviderAPI m
serviceProviderServer idp serviceProvider@(ServiceProviderConfig {redirectAfterLogout, cookieSettings, sessionCookieKey}) sessionCallback =
  loginEndpoint idp serviceProvider
    :<|> logoutEndpoint redirectAfterLogout cookieSettings sessionCookieKey
    :<|> redirectReceiverEndpoint idp serviceProvider sessionCallback

-- | Log in the user and set a session cookie.
loginEndpoint ::
  (Redis.MonadRedis m, MonadIO m) =>
  IdentityProviderConfig ->
  ServiceProviderConfig ->
  Maybe URIParam ->
  Maybe Text ->
  m
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Location" String]
        NoContent
    )
loginEndpoint idpConfig spConfig returnToURI preferredHandle = do
  loginEndpointWithData idpConfig spConfig (preferredHandle <&> \handle -> toJSON $ Map.singleton ("handle" :: Text) handle) returnToURI

-- | Log in the user and set a session cookie, propagating a value to the session callback.
loginEndpointWithData ::
  (Redis.MonadRedis m, MonadIO m, Aeson.ToJSON additionalData) =>
  IdentityProviderConfig ->
  ServiceProviderConfig ->
  Maybe additionalData ->
  Maybe URIParam ->
  m
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Location" String]
        NoContent
    )
loginEndpointWithData (IdentityProviderConfig {authorizationURI}) serviceProvider@ServiceProviderConfig {oauthClientID, scopes, cookieSettings} additionalData returnToURI = do
  (pkceVerifier, pkceChallenge, pkceMethod) <- PKCE.generatePkce
  pSesh <- Redis.liftRedis $ Redis.newPendingSession Session.LoginPage pkceVerifier (unpackURI <$> returnToURI) (toJSON <$> additionalData)
  let psid = (Session.pendingId pSesh)
  let redirectURI = redirectURIForServiceProvider serviceProvider
  authorizationURI redirectURI oauthClientID psid pkceChallenge pkceMethod scopes
    & redirectTo
    & setPendingSessionCookie cookieSettings psid
    & pure

-- | Sets a cookie in the user's browser which will be upgraded to a session
-- when they return from the identity provider.
setPendingSessionCookie ::
  ( Cookies.CookieSettings ->
    PendingSessionId ->
    LocationHeader ->
    Headers
      '[Header "Set-Cookie" Cookies.SetCookie, Header "Location" String]
      NoContent
  )
setPendingSessionCookie cookieSettings pSessionId loc =
  let cookieVal = fromId pSessionId
      cookie = (Cookies.newSetCookie cookieSettings pendingSessionCookieKey cookieVal) {setCookieMaxAge = Just (60 * 5)}
   in addHeader @"Set-Cookie" cookie loc

-- | Builds the URI which identity providers will redirect clients back to after
-- authentication.
redirectURIForServiceProvider :: ServiceProviderConfig -> URI
redirectURIForServiceProvider ServiceProviderConfig {baseServiceURI} =
  baseServiceURI
    & setPathAndQueryParams ["oauth", "redirect"] mempty

type LocationHeader = Headers '[Header "Location" String] NoContent

-- | Log out the user by telling the browser to clear the session cookies.
-- Note that this doesn't invalidate the session itself, it just removes its cookie from the current browser.
logoutEndpoint :: (Redis.MonadRedis m) => URI -> Cookies.CookieSettings -> Text -> ServerT LogoutEndpoint m
logoutEndpoint afterLogoutURI cookieSettings sessionCookieKey = do
  pure . clearSession $ redirectTo afterLogoutURI
  where
    clearSession =
      addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings sessionCookieKey)
        . addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings pendingSessionCookieKey)

redirectTo :: URI -> LocationHeader
redirectTo uri = addHeader (show uri) NoContent

-- | The endpoint which identity providers will redirect users to after authentication.
redirectReceiverEndpoint ::
  forall m.
  (Redis.MonadRedis m, MonadIO m) =>
  IdentityProviderConfig ->
  ServiceProviderConfig ->
  -- | Callback to complete the oauth flow. Which is passed an error message or an access token.
  -- User will be redirected to URI returned by this callback.
  SessionCallback m ->
  Maybe Code ->
  Maybe OAuth2State ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Cookies.CookieVal PendingSessionCookieKey PendingSessionId) ->
  Maybe Session ->
  m
    ( Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" String] NoContent
    )
redirectReceiverEndpoint IdentityProviderConfig {exchangeCodeForToken} serviceProviderConfig@(ServiceProviderConfig {cookieSettings, sessionCookieKey, jwtSettings, oauthClientID, oauthClientSecret}) callback mayCode mayStatePSID mayErrorType mayErrorDescription mayCookiePSID _authSession = handleErrs $ do
  case mayErrorType of
    Just errorType ->
      throwError (ErrorFromIdentityProvider errorType (fromMaybe "" mayErrorDescription))
    Nothing -> do
      cookiePSID <- case Cookies.cookieVal mayCookiePSID of
        Nothing -> throwError MissingOrExpiredPendingSession
        Just psid -> pure psid
      PendingSession {pkceVerifier, returnToURI, additionalData} <- ensurePendingSession cookiePSID
      TokenResponse {accessToken} <- case (mayCode, mayStatePSID) of
        (Just code, Just statePSID) -> do
          tokenForCode code statePSID cookiePSID pkceVerifier
        (Nothing, _) -> do
          throwError MissingCode
        (_, Nothing) -> do
          throwError MissingState
      -- It's a little annoying, but there's no way to get the session out of the JWT
      -- without verifying it, so we do that, then end up just re-signing it again to set it
      -- with ServantAuth.
      session <-
        verifySessionToken serviceProviderConfig accessToken >>= \case
          Left jwtErr -> throwError $ InvalidJWTFromIDP jwtErr
          Right session -> pure session
      setSessionCookie session >>= \case
        Nothing -> throwError FailedToCreateSession
        Just setAuthHeaders -> do
          let sessionCallbackData = SessionCallbackData {session, returnToURI, additionalData}
          redirectURI <- lift (callback (Right sessionCallbackData))
          pure . clearPendingSessionCookie . setAuthHeaders . redirectTo $ redirectURI
  where
    setSessionCookie :: (MonadIO n, AddHeader "Set-Cookie" SetCookie resp resp') => Session -> n (Maybe (resp -> resp'))
    setSessionCookie sess = do
      liftIO (JWT.createSignedCookie jwtSettings cookieSettings sessionCookieKey sess) >>= \case
        Left _err -> pure Nothing
        Right cookie -> pure . Just $ addHeader @"Set-Cookie" cookie
    handleErrs m =
      runExceptT m >>= \case
        Right response -> pure response
        Left err -> do
          redirectURI <- (callback (Left err))
          pure . clearSessionCookie . clearPendingSessionCookie . redirectTo $ redirectURI

    tokenForCode ::
      ( Code ->
        PendingSessionId ->
        PendingSessionId ->
        PKCEVerifier ->
        ExceptT RedirectReceiverErr m TokenResponse
      )
    tokenForCode code statePSID cookiePSID pkceVerifier = do
      if (statePSID /= cookiePSID)
        then do
          lift . Redis.liftRedis $ Redis.failPendingSession cookiePSID
          throwError (MismatchedState cookiePSID statePSID)
        else do
          let redirectURI = redirectURIForServiceProvider serviceProviderConfig
          liftIO (exchangeCodeForToken oauthClientID oauthClientSecret code redirectURI pkceVerifier) >>= either throwIO pure

    ensurePendingSession :: PendingSessionId -> ExceptT RedirectReceiverErr m PendingSession
    ensurePendingSession psid = do
      pending <- lift . Redis.liftRedis $ Redis.getPendingSession psid
      case pending of
        Nothing -> do
          throwError MissingOrExpiredPendingSession
        Just pSession -> pure $ pSession

    -- Clear the pending session
    clearPendingSessionCookie :: (AddHeader "Set-Cookie" SetCookie orig new) => orig -> new
    clearPendingSessionCookie =
      addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings pendingSessionCookieKey)

    clearSessionCookie :: (AddHeader "Set-Cookie" SetCookie orig new) => orig -> new
    clearSessionCookie = addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings sessionCookieKey)

-- | Decodes an Access Token into a session and verifies the following:
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifySessionToken :: forall m. (MonadIO m) => ServiceProviderConfig -> AccessToken -> m (Either JWTError Session)
verifySessionToken ServiceProviderConfig {jwtSettings} (AccessToken (JWTParam signedJWT)) = do
  JWT.verifyJWT jwtSettings signedJWT
