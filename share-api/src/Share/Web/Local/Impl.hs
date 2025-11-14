{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- Endpoints which are only active on the local deployment -}
module Share.Web.Local.Impl where

import Control.Monad.Random (randomIO)
import Control.Monad.Reader
import Data.Set qualified as Set
import Servant
import Share.App (shareAud, shareIssuer)
import Share.Env.Types
import Share.Env.Types qualified as Env
import Share.IDs
import Share.JWT (JWTParam (..))
import Share.JWT qualified as JWT
import Share.OAuth.Scopes (Scopes (Scopes))
import Share.OAuth.Scopes qualified as Scopes
import Share.OAuth.Session (Session)
import Share.OAuth.Session qualified as Session
import Share.OAuth.Types (AccessToken (..))
import Share.Postgres qualified as PG
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.User
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant.Cookies (SetCookie)
import Share.Web.App
import Share.Web.Authentication.AccessToken qualified as AccessToken
import Share.Web.Errors
import Share.Web.Errors qualified as Errors
import Share.Web.Local.API

-- | Login to the specified user without checking credentials.
-- Only available when running locally.
localLoginEndpoint ::
  UserHandle -> WebApp (Headers '[Header "Set-Cookie" SetCookie] Text)
localLoginEndpoint userHandle = do
  (User {user_id}) <-
    PG.runTransaction (UserQ.userByHandle userHandle) >>= \case
      Nothing -> Errors.respondError $ Errors.EntityMissing (ErrorID "no-user-for-handle") "No user for this handle"
      Just u -> pure u

  iss <- shareIssuer
  aud <- shareAud
  session <- liftIO $ Session.createSession (Session.ServiceName Env.serviceName) iss aud user_id
  setSessionCookie session >>= \case
    Nothing -> Errors.respondError $ Errors.InternalServerError "local:failed-create-session" ("Failed to create session" :: Text)
    Just setAuthHeaders -> do
      pure $ setAuthHeaders "Logged in to the test account."
  where
    setSessionCookie :: (AddHeader "Set-Cookie" SetCookie resp resp') => Session -> WebApp (Maybe (resp -> resp'))
    setSessionCookie sess = do
      Env.Env {cookieSettings, jwtSettings, sessionCookieKey} <- ask
      liftIO (JWT.createSignedCookie jwtSettings cookieSettings sessionCookieKey sess) >>= \case
        Left err -> do
          Logging.logErrorText $ "Failed to create session cookie: " <> tShow sess <> " " <> tShow err
          pure Nothing
        Right cookie -> pure . Just $ addHeader @"Set-Cookie" cookie

-- | Return an access token for the specified user.
-- Only available when running locally.
localAccessTokenEndpoint ::
  UserHandle ->
  WebApp Text
localAccessTokenEndpoint userHandle = do
  (User {user_id}) <-
    PG.runTransaction (UserQ.userByHandle userHandle) >>= \case
      Nothing -> Errors.respondError $ Errors.EntityMissing (ErrorID "no-user-for-handle") "No user for this handle"
      Just u -> pure u
  sessionID <- randomIO
  aud <- shareAud
  AccessToken (JWTParam accessToken) <- AccessToken.createAccessToken aud user_id sessionID (Scopes $ Set.fromList [Scopes.OpenId])
  pure (JWT.signedJWTToText $ accessToken)

-- | All of these endpoints are only accessible on the local deployment, otherwise they just 404.
server :: ServerT API WebApp
server = do
  let userRoutes = \userHandle ->
        localLoginEndpoint userHandle
          :<|> localAccessTokenEndpoint userHandle
  hoistServer
    (Proxy @API)
    guardLocal
    ( userRoutes
    )
  where
    -- Local endpoints should just 404 on any other deployment.
    guardLocal :: forall x. WebApp x -> WebApp x
    guardLocal m = do
      if Deployment.onLocal
        then m
        else Errors.respondError $ Errors.EntityMissing (ErrorID "local-on-nonlocal") "Not Found"
