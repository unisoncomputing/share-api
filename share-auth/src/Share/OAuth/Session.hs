{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Share.OAuth.Session
  ( Session (..),
    AdditionalSessionClaims (..),
    PendingSession (..),
    LoginRequest (..),
    AuthenticatedSession,
    AuthenticatedUserId,
    MaybeAuthenticatedSession,
    MaybeAuthenticatedUserId,
    pattern MaybeAuthedUserID,
    pattern AuthenticatedUser,
    pattern Unauthenticated,
    createSession,
    AuthCheckCtx,
    addAuthCheckCtx,
  )
where

import Control.Applicative
import Control.Lens hiding ((:>))
import Control.Monad.Random
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.JWT qualified as JWT
import Data.Aeson
import Data.Binary
import Data.Binary.Instances.Time ()
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Share.JWT qualified as JWT
import Share.OAuth.Types
import Share.Utils.Binary
import Share.Utils.IDs qualified as IDs
import Share.Utils.Servant.Cookies qualified as Cookies
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as Network
import Network.URI
import Network.Wai qualified as Wai
import Servant
import Servant.Server.Experimental.Auth qualified as ServantAuth
import Web.Cookie (parseCookies)

-- | Requires a valid session cookie to be present in the request and provides it as an
-- argument to the handler
--
-- e.g. @Session -> ...@
type AuthenticatedSession = Servant.AuthProtect "require-session"

type instance ServantAuth.AuthServerData (Servant.AuthProtect "require-session") = Session

-- | Requires a valid session cookie to be present in the request,
-- provides the authenticated user's user-id as an argument to the handler
--
-- e.g. @UserId -> ...@
type AuthenticatedUserId = Servant.AuthProtect "require-user-id"

type instance ServantAuth.AuthServerData (AuthProtect "require-user-id") = UserId

-- | Used for endpoints with optional auth.
-- Provides 'Just' the session if a valid session cookie is present in the request,
-- otherwise provides 'Nothing'.
--
-- e.g. @Maybe Session -> ...@
type MaybeAuthenticatedSession = Servant.AuthProtect "maybe-session"

type instance ServantAuth.AuthServerData (AuthProtect "maybe-session") = Maybe Session

-- | Used for endpoints with optional auth.
-- Provides 'Just' the user ID if a valid session cookie is present in the request,
-- otherwise provides 'Nothing'.
type MaybeAuthenticatedUserId = Servant.AuthProtect "maybe-user-id"

type instance ServantAuth.AuthServerData (AuthProtect "maybe-user-id") = Maybe UserId

-- | An additional check to perform on a session. Returns True if valid, False otherwise.
type SessionCheck = (Session -> Handler Bool)

-- | A custom auth handler which checks the session cookie and the JWT token and returns the
-- authenticated session if there is one.
-- If a valid session is authenticated then the additional session check is performed, if that
-- check fails the user is considered unauthenticated.
checkOptionalAuthenticatedSession ::
  Text ->
  SessionCheck ->
  Cookies.CookieSettings {- Not actually used yet, but will probably need it soon so this makes it future-compatible -} ->
  JWT.JWTSettings ->
  ServantAuth.AuthHandler Wai.Request (Maybe Session)
checkOptionalAuthenticatedSession sessionCookieName sessCheck _cookieSettings jwtSettings = ServantAuth.mkAuthHandler authHandler
  where
    authHandler :: Wai.Request -> Handler (Maybe Session)
    authHandler req = do
      let guardCheck :: Session -> MaybeT Handler Session
          guardCheck s = do
            lift (sessCheck s) >>= guard
            pure s
      runMaybeT $ (jwtSession req >>= guardCheck) <|> (cookieSession req >>= guardCheck)
    jwtSession :: Wai.Request -> MaybeT Handler Session
    jwtSession req = do
      tokenBytes <- MaybeT . pure $ do
        authHdr <- lookup "Authorization" $ Wai.requestHeaders req
        let bearer = "Bearer "
            (mbearer, rest) = BS.splitAt (BS.length bearer) authHdr
        guard (mbearer == bearer)
        pure rest
      signedJWT <- MaybeT . pure . eitherToMaybe $ JWT.textToSignedJWT (Text.decodeUtf8 tokenBytes)
      MaybeT . liftIO . fmap eitherToMaybe $ JWT.verifyJWT jwtSettings signedJWT
    cookieSession :: Wai.Request -> MaybeT Handler Session
    cookieSession req = do
      jwtCookieBytes <- MaybeT . pure $ do
        cookies' <- lookup Network.hCookie $ Wai.requestHeaders req
        let cookies = parseCookies cookies'
        lookup (Text.encodeUtf8 sessionCookieName) cookies
      signedJWT <- MaybeT . pure . eitherToMaybe $ JWT.textToSignedJWT (Text.decodeUtf8 jwtCookieBytes)
      MaybeT . liftIO . fmap eitherToMaybe $ JWT.verifyJWT jwtSettings signedJWT

    eitherToMaybe = \case
      Left _ -> Nothing
      Right a -> Just a

-- | Make an auth handler using an additional session check function.
checkRequiredAuthenticatedSession :: Text -> SessionCheck -> Cookies.CookieSettings -> JWT.JWTSettings -> ServantAuth.AuthHandler Wai.Request (Session)
checkRequiredAuthenticatedSession sessionCookieName sessCheck cookieSettings jwtSettings =
  ServantAuth.mkAuthHandler $ \req -> do
    ServantAuth.unAuthHandler (checkOptionalAuthenticatedSession sessionCookieName sessCheck cookieSettings jwtSettings) req >>= \case
      Nothing -> throwError (Servant.err401 {errReasonPhrase = "Unauthenticated", errBody = "Unauthenticated: Please ensure you have a valid session."})
      Just a -> pure a

-- | The context args required for auth handlers.
type AuthCheckCtx =
  '[ ServantAuth.AuthHandler Wai.Request Session,
     ServantAuth.AuthHandler Wai.Request UserId,
     ServantAuth.AuthHandler Wai.Request (Maybe Session),
     ServantAuth.AuthHandler Wai.Request (Maybe UserId)
   ]

-- | Here we provide the handlers for each AuthRequirement type.
--
-- You can combine this with your app's context using '.++' at both the term and type
-- level
authCheckCtx ::
  Cookies.CookieSettings ->
  JWT.JWTSettings ->
  Text ->
  Servant.Context AuthCheckCtx
authCheckCtx cookieSettings jwtSettings sessionCookieName =
  (checkRequiredAuthenticatedSession sessionCookieName defaultSessionCheck cookieSettings jwtSettings)
    :. (mapAuthHandler sessionUserId $ checkRequiredAuthenticatedSession sessionCookieName defaultSessionCheck cookieSettings jwtSettings)
    :. (checkOptionalAuthenticatedSession sessionCookieName defaultSessionCheck cookieSettings jwtSettings)
    :. (mapAuthHandler (fmap sessionUserId) $ checkOptionalAuthenticatedSession sessionCookieName defaultSessionCheck cookieSettings jwtSettings)
    :. Servant.EmptyContext
  where
    -- By default we don't have any additional checks aside from those performed as part of
    -- JWT decoding.
    defaultSessionCheck _session = pure True
    -- A functor instance is added in 0.20, but we're on 0.19 for now.
    mapAuthHandler :: (a -> a') -> ServantAuth.AuthHandler req a -> ServantAuth.AuthHandler req a'
    mapAuthHandler f (ServantAuth.AuthHandler h) = ServantAuth.AuthHandler $ \req -> f <$> h req

addAuthCheckCtx ::
  Cookies.CookieSettings ->
  JWT.JWTSettings ->
  Text ->
  Servant.Context (appCtx :: [Type]) ->
  Servant.Context (AuthCheckCtx .++ appCtx)
addAuthCheckCtx cookieSettings jwtSettings sessionCookieName appCtx =
  authCheckCtx cookieSettings jwtSettings sessionCookieName .++ appCtx

-- | Unpacks an auth result into a Maybe, and extracts the user id.
-- This is useful for pattern matching on the result of an auth check and extracting the user id.
pattern MaybeAuthedUserID :: Maybe UserId -> Maybe Session
pattern MaybeAuthedUserID mayUserId <- (getAuthenticatedUserId -> mayUserId)

{-# COMPLETE MaybeAuthedUserID #-}

-- | Only matches if a session was successfully authenticated
pattern AuthenticatedUser :: UserId -> Maybe Session
pattern AuthenticatedUser userId <- Just (Session {sessionUserId = userId})

-- | Matches if the user is unauthenticated, regardless of the reason.
pattern Unauthenticated :: Maybe Session
pattern Unauthenticated <- MaybeAuthedUserID Nothing

{-# COMPLETE AuthenticatedUser, Unauthenticated #-}

getAuthenticatedUserId :: Maybe Session -> Maybe UserId
getAuthenticatedUserId = \case
  Just ses -> Just $ sessionUserId ses
  _ -> Nothing

data AdditionalSessionClaims = AdditionalSessionClaims
  { termsAndConditionsAccepted :: Bool
  }
  deriving stock (Show, Eq, Ord)

-- | Note: This session will be signed into the user's cookie unencrypted.
-- Don't put anything sensitive in here.
data Session = Session
  { sessionId :: SessionId,
    sessionUserId :: UserId,
    sessionCreated :: UTCTime,
    sessionExpiry :: UTCTime,
    sessionIssuer :: URI,
    sessionAudience :: Set URI
  }
  deriving stock (Show)
  deriving (Binary) via JSONBinary Session

instance JWT.ToJWT Session where
  encodeJWT (Session (SessionId sessionId) userID created expiry issuer aud) =
    JWT.emptyClaimsSet
      & JWT.claimSub ?~ (JWT.string # IDs.toText userID)
      & JWT.claimJti ?~ IDs.toText (JTI sessionId)
      & JWT.claimIat ?~ JWT.NumericDate created
      & JWT.claimExp ?~ JWT.NumericDate expiry
      & JWT.claimIss ?~ (JWT.uri # issuer)
      & JWT.claimAud ?~ JWT.Audience (review JWT.uri <$> Set.toList aud)

instance JWT.FromJWT Session where
  decodeJWT claims = do
    sessionUserId <- maybeToEither "Missing sub claim" (claims ^? JWT.claimSub . _Just . JWT.string) >>= IDs.fromText
    sessionId <-
      maybeToEither "Missing jti claim" (claims ^? JWT.claimJti . _Just) >>= \txt -> do
        JTI uuid <- IDs.fromText txt
        pure $ SessionId uuid
    sessionCreated <- maybeToEither "Missing iat claim" (claims ^? JWT.claimIat . _Just . to (\(JWT.NumericDate utcTime) -> utcTime))
    sessionExpiry <- maybeToEither "Missing exp claim" (claims ^? JWT.claimExp . _Just . to (\(JWT.NumericDate utcTime) -> utcTime))
    sessionIssuer <- maybeToEither "Missing iss claim" (claims ^? JWT.claimIss . _Just . JWT.uri)
    let sessionAudience = claims & setOf (JWT.claimAud . _Just . folding (\(JWT.Audience auds) -> auds) . JWT.uri)
    pure $ Session {..}
    where
      maybeToEither :: e -> Maybe a -> Either e a
      maybeToEither e = maybe (Left e) Right

instance ToJSON Session where
  toJSON s = toJSON $ JWT.encodeJWT s

instance FromJSON Session where
  parseJSON v = do
    claims <- parseJSON v
    either (fail . Text.unpack) pure $ JWT.decodeJWT claims

data PendingSession = PendingSession
  { pendingId :: PendingSessionId,
    loginRequest :: LoginRequest,
    pkceVerifier :: PKCEVerifier,
    -- The URI that the user started the login flow from,
    -- you may wish to redirect them back here after login.
    returnToURI :: Maybe URI,
    -- Any additional data about the login/signup that the service provider wanted to pass
    -- through to the auth process.
    additionalData :: Maybe Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Binary) via JSONBinary PendingSession

data LoginRequest
  = -- The login was kicked off by an oauth client, e.g. UCM
    OAuthRequest AuthenticationRequest
  | --  The login was kicked off by visiting the 'login' page.
    LoginPage
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (Binary) via JSONBinary LoginRequest

-- | Sessions expire in 30 days
sessionTTL :: NominalDiffTime
sessionTTL =
  (30 * nominalDay)

createSession :: MonadIO m => URI -> Set URI -> UserId -> m Session
createSession sessionIssuer sessionAudience sessionUserId = do
  sessionId <- randomIO
  sessionCreated <- liftIO getCurrentTime
  let sessionExpiry = addUTCTime sessionTTL sessionCreated
  let session = Session {..}
  pure session
