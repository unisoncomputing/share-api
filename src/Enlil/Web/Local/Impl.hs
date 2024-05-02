{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- Endpoints which are only active on the local deployment -}
module Enlil.Web.Local.Impl where

import Control.Monad.Random (randomIO)
import Control.Monad.Reader
import Data.Set qualified as Set
import Enlil.App (enlilAud, enlilIssuer)
import Enlil.Env
import Enlil.Env qualified as Env
import Enlil.IDs
import Enlil.JWT (JWTParam (..))
import Enlil.JWT qualified as JWT
import Enlil.OAuth.Scopes (Scopes (Scopes))
import Enlil.OAuth.Scopes qualified as Scopes
import Enlil.OAuth.Session (Session)
import Enlil.OAuth.Session qualified as Session
import Enlil.OAuth.Types (AccessToken (..))
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Queries qualified as Q
import Enlil.Prelude
import Enlil.User
import Enlil.Utils.Deployment qualified as Deployment
import Enlil.Utils.Logging qualified as Logging
import Enlil.Utils.Servant.Cookies (SetCookie)
import Enlil.Web.App
import Enlil.Web.Authentication.AccessToken qualified as AccessToken
import Enlil.Web.Errors
import Enlil.Web.Errors qualified as Errors
import Enlil.Web.Local.API
import Servant

-- | Login to the specified user without checking credentials.
-- Only available when running locally.
localLoginEndpoint ::
  UserHandle -> WebApp (Headers '[Header "Set-Cookie" SetCookie] Text)
localLoginEndpoint userHandle = do
  (User {user_id}) <-
    PG.runTransaction (Q.userByHandle userHandle) >>= \case
      Nothing -> Errors.respondError $ Errors.EntityMissing (ErrorID "no-user-for-handle") "No user for this handle"
      Just u -> pure u

  iss <- enlilIssuer
  aud <- enlilAud
  session <- liftIO $ Session.createSession iss aud user_id
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
    PG.runTransaction (Q.userByHandle userHandle) >>= \case
      Nothing -> Errors.respondError $ Errors.EntityMissing (ErrorID "no-user-for-handle") "No user for this handle"
      Just u -> pure u
  sessionID <- randomIO
  aud <- enlilAud
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
