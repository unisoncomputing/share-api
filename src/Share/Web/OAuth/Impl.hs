{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module implements the endpoints for performing an OAuth2 flow using the PKCE extension.
-- Here's a high-level overview of OAuth2: https://developer.okta.com/blog/2019/10/21/illustrated-guide-to-oauth-and-oidc
-- Here's a good reference guide to various steps: https://www.oauth.com/
-- Here's a good reference for PKCE specifically: https://www.oauth.com/oauth2-servers/pkce/
module Share.Web.OAuth.Impl
  ( serviceProviderServer,
    identityProviderServer,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Network.URI (parseURI)
import Servant
import Share.App (shareAud, shareIssuer)
import Share.Env qualified as Env
import Share.Github qualified as Github
import Share.IDs (PendingSessionId, UserHandle, fromId)
import Share.IDs qualified as IDs
import Share.JWT qualified as JWT
import Share.Metrics qualified as Metrics
import Share.OAuth.API qualified as OAuth
import Share.OAuth.Errors qualified as OAuthError
import Share.OAuth.PKCE qualified as PKCE
import Share.OAuth.Redis qualified as Redis
import Share.OAuth.Scopes
import Share.OAuth.Session (PendingSession (..), Session (..))
import Share.OAuth.Session qualified as Session
import Share.OAuth.Types (AccessToken, AuthenticationRequest (..), Code, GrantType (AuthorizationCode), OAuth2State, OAuthClientConfig (..), OAuthClientId, PKCEChallenge, PKCEChallengeMethod, RedirectReceiverErr (..), ResponseType (ResponseTypeCode), TokenRequest (..), TokenResponse (..), TokenType (BearerToken))
import Share.OAuth.Types qualified as OAuth
import Share.Postgres qualified as PG
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.User (User (User))
import Share.User qualified as User
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant
import Share.Utils.Servant.Cookies (CookieVal, cookieVal)
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Utils.URI (URIParam (URIParam), unpackURI)
import Share.Utils.URI qualified as URI
import Share.Web.App
import Share.Web.Authentication.AccessToken qualified as AccessToken
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.OAuth.Clients (validateOAuthClientForTokenExchange, validateOAuthClientRedirectURI)
import Web.Cookie as Cookie (SetCookie (..))

data AuthError = HandleAlreadyTaken UserHandle URI
  deriving (Eq, Show)

instance Logging.Loggable AuthError where
  toLog = \case
    (HandleAlreadyTaken handle _) ->
      Logging.textLog ("User handle already taken: " <> Text.pack (show handle))
        & Logging.withSeverity Logging.UserFault

instance ToServerError AuthError where
  toServerError (HandleAlreadyTaken _handle uri) =
    (ErrorID "handle-already-taken", serverErrorRedirect uri)

-- | The URL used to kick off a login using Share for oauth2 clients like the Cloud app or UCM.
-- if the user isn't already logged in it will authenticate them via the Github login page.
authorizationEndpoint ::
  ResponseType ->
  OAuthClientId ->
  URIParam ->
  Scopes ->
  Text ->
  PKCEChallenge ->
  PKCEChallengeMethod ->
  Maybe Session ->
  WebApp (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
authorizationEndpoint ResponseTypeCode oauthClientId redirectURI scopes state pkceChallenge pkceChallengeMethod existingSession = do
  validateRequest
  -- We don't currently use pkce with github, but the session interface is simpler if we
  -- always generate a verifier anyways.
  (pkceVerifier, _pkceChallenge, _pkceChallengeMethod) <- PKCE.generatePkce
  pSesh <- Redis.liftRedis $ Redis.newPendingSession (Session.OAuthRequest authRequest) pkceVerifier Nothing Nothing
  let psid = pendingId pSesh
  Env.Env {cookieSettings} <- ask
  case existingSession of
    Just {} -> do
      -- We're already logged in, no need to go through Github, we can redirect directly to
      -- the redirect receiver.
      redirectReceiverUri <- sharePathQ ["oauth", "redirect"] (Map.fromList [])
      redirectTo redirectReceiverUri
        & setPendingSessionCookie cookieSettings (pendingId pSesh)
        & pure
    Nothing -> do
      setPendingSessionCookie cookieSettings psid <$> loginWithGithub psid
  where
    authRequest :: AuthenticationRequest
    authRequest = AuthenticationRequest {clientId = oauthClientId, redirectURI, state, scopes, pkceChallenge, pkceChallengeMethod}
    validateRequest :: WebApp ()
    validateRequest = do
      when (not $ containsScope OpenId scopes) $ do
        respondError (OAuthError.OpenIDScopeRequired authRequest)

tokenEndpoint :: TokenRequest -> WebApp (Headers '[Header "Cache-Control" String] TokenResponse)
tokenEndpoint (TokenRequest AuthorizationCode code redirectURI pkceVerifier callerClientId mayClientSecret) = do
  OAuthClientConfig {audience = URIParam aud} <- validateOAuthClientForTokenExchange callerClientId mayClientSecret redirectURI
  (sid, uid, authRequest) <- Redis.liftRedis $ Redis.exchangeOAuth2Code code callerClientId redirectURI pkceVerifier
  accessToken <- AccessToken.createAccessToken (Set.singleton aud) uid sid (scopes authRequest)
  tokenResponse <- newTokenResponse accessToken authRequest
  pure (withHeaders tokenResponse)
  where
    withHeaders :: forall a. a -> (Headers '[Header "Cache-Control" String] a)
    withHeaders = addHeader "no-store"
    newTokenResponse :: AccessToken -> AuthenticationRequest -> WebApp TokenResponse
    newTokenResponse accessToken (AuthenticationRequest {scopes}) = do
      let idToken = Nothing
          tokenType = BearerToken
          expiresIn = AccessToken.accessTokenTTL
          scope = scopes
      pure $
        TokenResponse
          { refreshToken = Nothing,
            accessToken,
            idToken,
            tokenType,
            expiresIn,
            scope
          }

-- | Endpoint which completes the oauth2 flow with Github, or Share (if the user was already
-- authenticated)
redirectReceiverEndpoint ::
  Maybe OAuth.Code ->
  Maybe OAuth2State ->
  Maybe Text ->
  Maybe Text ->
  Maybe (CookieVal OAuth.PendingSessionCookieKey PendingSessionId) ->
  Maybe Session ->
  WebApp (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
redirectReceiverEndpoint _mayGithubCode _mayStatePSID (Just errorType) mayErrorDescription _mayCookiePSID _authSession = do
  case errorType of
    -- The user didn't grant us the permissions we need for their github account.
    "access_denied" -> do
      Logging.logUserFaultText "User rejected required authentication permissions on identity provider"
      errorRedirect AccountCreationGitHubPermissionsRejected
    otherErrType -> do
      Logging.logErrorText ("Github authentication error: " <> otherErrType <> " " <> fold mayErrorDescription)
      errorRedirect UnspecifiedError
redirectReceiverEndpoint mayGithubCode mayStatePSID _errorType@Nothing _mayErrorDescription mayCookiePSID existingAuthSession = do
  cookiePSID <- case cookieVal mayCookiePSID of
    Nothing -> respondError $ MissingOrExpiredPendingSession
    Just psid -> pure psid
  PendingSession {loginRequest, returnToURI = unvalidatedReturnToURI, additionalData} <- ensurePendingSession cookiePSID
  newOrPreExistingUser <- case (mayGithubCode, mayStatePSID, existingAuthSession) of
    -- The user has an already valid session, we can use that.
    (_, _, Just session) -> do
      user <- (PGO.expectUserById (sessionUserId session))
      pure (UserQ.PreExisting user)
    -- The user has completed the Github flow, we can log them in or create a new user.
    (Just githubCode, Just statePSID, _) -> do
      (ghUser, ghEmail) <-
        -- Skip the github flow when developing locally, and just use some dummy github user
        -- data.
        if Deployment.onLocal
          then pure localGithubUserInfo
          else getGithubUserInfo githubCode statePSID cookiePSID
      mayPreferredHandle <- runMaybeT do
        obj <- hoistMaybe additionalData
        case Aeson.fromJSON obj of
          Aeson.Error _err -> do
            Logging.logErrorText ("Failed to parse additional data: " <> tShow obj)
            empty
          Aeson.Success m -> do
            handle <- hoistMaybe $ Map.lookup ("handle" :: Text) m
            hoistMaybe . eitherToMaybe $ IDs.fromText handle
      completeGithubFlow ghUser ghEmail mayPreferredHandle
    (Nothing, _, _) -> do
      respondError $ MissingCode
    (_, Nothing, _) -> do
      respondError $ MissingState
  let (User {User.user_id = uid}) = UserQ.getNewOrPreExisting newOrPreExistingUser
  when (UserQ.isNew newOrPreExistingUser) do
    Metrics.tickUserSignup
  Env.Env {cookieSettings} <- ask
  iss <- shareIssuer
  -- If this was an oauth request we redirect back to the client, otherwise we just create a
  -- session and send the user to the homepage.
  case loginRequest of
    Session.LoginPage -> do
      mayReturnToURI <- runMaybeT do
        uri <- hoistMaybe unvalidatedReturnToURI
        -- Ignore any non-unison redirections.
        guardM . lift $ isTrustedURI uri
        pure uri
      landingPageURI <- whenNothing mayReturnToURI $ mkLoginLandingPageURI newOrPreExistingUser
      aud <- shareAud
      session <- Session.createSession iss aud uid
      setSessionCookie session >>= \case
        Nothing -> respondError $ InternalServerError "session-create-failure" ("Failed to create session" :: Text)
        Just setAuthHeaders -> do
          let response = addHeader @"Location" (URI.uriToString landingPageURI) NoContent
          pure . clearPendingSessionCookie cookieSettings $ setAuthHeaders response
    Session.OAuthRequest authRequest@AuthenticationRequest {clientId, redirectURI} -> do
      OAuthClientConfig {audience = URIParam aud} <- validateOAuthClientRedirectURI clientId redirectURI
      landingPageURI <- mkUCMLoginSuccessLandingPageURI
      let AuthenticationRequest {redirectURI = URI.URIParam redirectURI, state = clientOAuthState, scopes = _scopes} = authRequest
      (session, code) <- Redis.liftRedis $ Redis.createOAuth2Code iss (Set.singleton aud) uid authRequest
      let redirectURIWithParams = addParamsToRedirectURI redirectURI landingPageURI clientOAuthState code
      let response = addHeader @"Location" (URI.uriToString redirectURIWithParams) $ NoContent
      setSessionCookie session >>= \case
        Nothing -> respondError $ InternalServerError "session-create-failure" ("Failed to create session" :: Text)
        Just setAuthHeaders -> pure . clearPendingSessionCookie cookieSettings $ setAuthHeaders response
  where
    localGithubUserInfo :: (Github.GithubUser, Github.GithubEmail)
    localGithubUserInfo =
      ( Github.GithubUser
          { githubHandle = "LocalGithubUser",
            githubUserId = 1,
            githubUserAvatarUrl = URIParam $ fromJust $ parseURI "https://avatars.githubusercontent.com/u/0?v=4",
            githubUserName = Just "Local Github User"
          },
        Github.GithubEmail
          { githubEmailEmail = "local@example.com",
            githubEmailIsPrimary = True,
            githubEmailIsVerified = True
          }
      )

    getGithubUserInfo ::
      ( OAuth.Code ->
        PendingSessionId ->
        PendingSessionId ->
        WebApp (Github.GithubUser, Github.GithubEmail)
      )
    getGithubUserInfo githubCode statePSID cookiePSID = do
      when (statePSID /= cookiePSID) do
        Redis.liftRedis $ Redis.failPendingSession cookiePSID
        respondError (MismatchedState cookiePSID statePSID)
      token <- Github.githubTokenForCode githubCode
      ghUser <- Github.githubUser token
      ghEmail <- Github.primaryGithubEmail token
      pure (ghUser, ghEmail)
    completeGithubFlow :: Github.GithubUser -> Github.GithubEmail -> Maybe UserHandle -> WebApp (UserQ.NewOrPreExisting User)
    completeGithubFlow ghUser@(Github.GithubUser githubHandle _githubUserId _avatarUrl _name) ghEmail mayPreferredHandle = do
      userHandle <- case mayPreferredHandle of
        Just handle -> pure handle
        Nothing -> case IDs.fromText @UserHandle (Text.toLower githubHandle) of
          Left _err -> errorRedirect $ AccountCreationInvalidHandle (Text.toLower githubHandle)
          Right handle -> pure handle
      PG.tryRunTransaction (UserQ.findOrCreateGithubUser AuthZ.userCreationOverride ghUser ghEmail userHandle) >>= \case
        Left (UserQ.UserHandleTaken _) -> do
          handleTakenUri <- (shareUIPathQ ["finish-signup"] (Map.fromList [("state", "handle-taken"), ("conflictingHandle", IDs.toText userHandle)]))
          respondError $ HandleAlreadyTaken userHandle handleTakenUri
        Left (UserQ.InvalidUserHandle err handle) -> do
          Logging.logErrorText ("Invalid user handle: " <> handle <> " " <> err)
          errorRedirect $ AccountCreationInvalidHandle handle
        Right u -> pure u

    mkLoginLandingPageURI :: UserQ.NewOrPreExisting User -> WebApp URI
    mkLoginLandingPageURI (UserQ.New {}) = shareUIPathQ [] (Map.singleton "event" "new-user-log-in")
    mkLoginLandingPageURI (UserQ.PreExisting {}) = shareUIPathQ [] (Map.singleton "event" "log-in")
    mkUCMLoginSuccessLandingPageURI :: WebApp URI
    mkUCMLoginSuccessLandingPageURI = shareUIPath ["ucm-connected"]
    ensurePendingSession :: PendingSessionId -> WebApp PendingSession
    ensurePendingSession psid = do
      pending <- Redis.liftRedis $ Redis.getPendingSession psid
      case pending of
        Nothing -> do
          respondError MissingOrExpiredPendingSession
        Just pSession -> pure pSession
    addParamsToRedirectURI :: URI -> URI -> Text -> Code -> URI
    addParamsToRedirectURI uri landingPageURI state code =
      uri
        & URI.addQueryParam "state" state
        & URI.addQueryParam "code" code
        & URI.addQueryParam "next" (URIParam landingPageURI)

serviceProviderServer :: ServerT OAuth.ServiceProviderAPI WebApp
serviceProviderServer =
  loginEndpoint
    :<|> logoutEndpoint
    :<|> redirectReceiverEndpoint

identityProviderServer :: ServerT OAuth.IdentityProviderAPI WebApp
identityProviderServer =
  authorizationEndpoint
    :<|> tokenEndpoint

-- | This endpoint kicks off a login for users of the Share web app.
loginEndpoint :: ServerT OAuth.LoginEndpoint WebApp
loginEndpoint returnToURI mayPreferredHandle = do
  -- We don't currently use pkce with github, but the session interface is simpler if we
  -- always generate a verifier anyways.
  (pkceVerifier, _pkceChallenge, _pkceChallengeMethod) <- PKCE.generatePkce
  pSesh <- Redis.liftRedis $ Redis.newPendingSession Session.LoginPage pkceVerifier (unpackURI <$> returnToURI) (mayPreferredHandle <&> \handle -> toJSON $ Map.singleton ("handle" :: Text) handle)
  let psid = pendingId pSesh
  Env.Env {cookieSettings} <- ask
  setPendingSessionCookie cookieSettings psid <$> loginWithGithub psid

loginWithGithub ::
  PendingSessionId ->
  WebApp
    ( Headers
        '[Header "Location" String]
        NoContent
    )
loginWithGithub psid = do
  githubAuthURI <-
    if Deployment.onLocal
      then skipGithubLoginURL psid
      else Github.githubAuthenticationURI psid
  pure $ redirectTo githubAuthURI
  where
    skipGithubLoginURL :: OAuth2State -> WebApp URI
    skipGithubLoginURL oauth2State = do
      sharePathQ ["oauth", "redirect"] $
        Map.fromList
          [ ("code", "code"),
            ("state", toQueryParam oauth2State)
          ]

-- | Log out the user by telling the browser to clear the session cookies.
-- Note that this doesn't (yet) invalidate the session itself, it just removes its cookie from the
-- current browser.
logoutEndpoint :: ServerT OAuth.LogoutEndpoint WebApp
logoutEndpoint = do
  Env.Env {cookieSettings, sessionCookieKey} <- ask
  uri <- shareUIPathQ [] (Map.singleton "event" "log-out")
  pure $ clearPendingSessionCookie cookieSettings . clearSessionCookie cookieSettings sessionCookieKey $ redirectTo uri

setPendingSessionCookie :: (AddHeader "Set-Cookie" SetCookie orig new) => Cookies.CookieSettings -> PendingSessionId -> orig -> new
setPendingSessionCookie cookieSettings pSessionId =
  let cookieKey = OAuth.pendingSessionCookieKey
      cookieVal = fromId pSessionId
      cookie = (Cookies.newSetCookie cookieSettings cookieKey cookieVal) {setCookieMaxAge = Just (60 * 5 {- 5 minute expiry -})}
   in addHeader @"Set-Cookie" cookie

setSessionCookie :: (AddHeader "Set-Cookie" SetCookie resp resp') => Session -> WebApp (Maybe (resp -> resp'))
setSessionCookie sess = do
  Env.Env {cookieSettings, jwtSettings, sessionCookieKey} <- ask
  liftIO (JWT.createSignedCookie jwtSettings cookieSettings sessionCookieKey sess) >>= \case
    Left err -> do
      Logging.logErrorText $ "Failed to create session cookie: " <> tShow sess <> " " <> tShow err
      pure Nothing
    Right cookie -> pure . Just $ addHeader @"Set-Cookie" cookie

clearSessionCookie :: (AddHeader "Set-Cookie" SetCookie orig new) => Cookies.CookieSettings -> Text -> orig -> new
clearSessionCookie cookieSettings sessionCookieKey = addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings sessionCookieKey)

-- | Clear the pending session
clearPendingSessionCookie :: (AddHeader "Set-Cookie" SetCookie orig new) => Cookies.CookieSettings -> orig -> new
clearPendingSessionCookie cookieSettings =
  addHeader @"Set-Cookie" (Cookies.clearCookie cookieSettings OAuth.pendingSessionCookieKey)
