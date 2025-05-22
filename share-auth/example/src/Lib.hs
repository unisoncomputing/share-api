{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib (main) where

import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (DiffTime)
import Database.Redis qualified as R
import GHC.Stack (HasCallStack)
import Network.URI qualified as URI
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Share.JWT qualified as JWT
import Share.OAuth.API (ServiceProviderAPI)
import Share.OAuth.IdentityProvider.Share qualified as Share
import Share.OAuth.Scopes (Scopes (..))
import Share.OAuth.Scopes qualified as Scopes
import Share.OAuth.ServiceProvider (ServiceProviderConfig (..), SessionCallbackData)
import Share.OAuth.ServiceProvider qualified as Auth
import Share.OAuth.Session (AuthCheckCtx, AuthenticatedUserId, MaybeAuthenticatedUserId, addAuthCheckCtx)
import Share.OAuth.Types (OAuthClientId (..), OAuthClientSecret (OAuthClientSecret), RedirectReceiverErr, UserId)
import Share.Utils.Servant.Cookies qualified as Cookies
import UnliftIO

-- | An example application endpoint which is optionally authenticated.
-- The handler can check which user the caller is authenticated as, if any.
type MayAuthedEndpoint = MaybeAuthenticatedUserId :> Get '[JSON] String

-- | An example application endpoint which REQUIRES authentication.
-- It will throw a 401 if the caller is not authenticated.
type AuthedEndpoint = AuthenticatedUserId :> Get '[JSON] String

-- | An example application endpoint which handles errors from the OAuth2 flow.
type ErrorEndpoint = QueryParam "error" String :> Get '[JSON] String

-- An example application API.
-- We include the 'ServiceProviderAPI' here.
type MyAPI =
  ServiceProviderAPI
    :<|> ("may-authed" :> MayAuthedEndpoint)
    :<|> ("authed" :> AuthedEndpoint)
    :<|> "error" :> ErrorEndpoint

-- | A handler which checks if the user is authenticated.
mayAuthedEndpoint :: (MonadIO m) => Maybe UserId -> m String
mayAuthedEndpoint mayCallerUserId = do
  case mayCallerUserId of
    Nothing -> pure "no user"
    Just userId -> do
      pure $ "Hello, " <> show userId

-- | A handler which requires an authenticated user.
authedEndpoint :: (MonadIO m) => UserId -> m String
authedEndpoint callerUserId = do
  pure $ "Hello, " <> show callerUserId

-- | A handler which displays errors from the OAuth2 flow.
errorEndpoint :: (Applicative m) => Maybe String -> m String
errorEndpoint err = do
  pure $ fromMaybe "no error" err

-- | A helper function for constructing URIs from constant strings.
unsafeURI :: (HasCallStack) => String -> URI.URI
unsafeURI = fromJust . URI.parseURI

-- | A session callback which redirects the user to either an error page
-- or the authed handler endpoint depending on whether the oauth2 login succeeds.
mySessionCallback :: (Applicative m) => Either RedirectReceiverErr SessionCallbackData -> m URI
mySessionCallback (Left err) = pure . fromJust . URI.parseURI $ "http://cloud:3030/error?error=" <> show err
mySessionCallback (Right _session) = pure $ unsafeURI "http://cloud:3030/authed"

-- | The main application entrypoint.
main :: IO ()
main = do
  putStrLn "loading Redis"
  redisConn <- R.checkedConnect R.defaultConnectInfo
  putStrLn "booting up"

  jwtSettings <- case JWT.defaultJWTSettings signingKey (Just legacyKey) rotatedKeys acceptedAudiences acceptedIssuers of
    Left cryptoError -> throwIO cryptoError
    Right jwtS -> do
      pure jwtS

  Warp.run 3030 $ serveWithContext (Proxy @MyAPI) (ctx jwtSettings) (myServer redisConn jwtSettings)
  putStrLn "exiting"
  pure ()
  where
    ctxProxy :: Proxy (AuthCheckCtx .++ '[Cookies.CookieSettings, JWT.JWTSettings])
    ctxProxy = Proxy
    apiProxy :: Proxy MyAPI
    apiProxy = Proxy
    -- The api context required by servant-auth
    appCtx :: JWT.JWTSettings -> (Context '[Cookies.CookieSettings, JWT.JWTSettings])
    appCtx jwtSettings = cookieSettings :. jwtSettings :. EmptyContext
    sessionCookieKey :: Text
    sessionCookieKey = "session"
    ctx :: JWT.JWTSettings -> Context (AuthCheckCtx .++ '[Cookies.CookieSettings, JWT.JWTSettings])
    ctx jwtSettings = addAuthCheckCtx cookieSettings jwtSettings "session" (appCtx jwtSettings)
    serviceProviderEndpoints :: JWT.JWTSettings -> ServerT ServiceProviderAPI R.Redis
    serviceProviderEndpoints jwtSettings = Auth.serviceProviderServer Share.localShareIdentityProvider (spConfig jwtSettings) mySessionCallback
    myServer :: R.Connection -> JWT.JWTSettings -> Server MyAPI
    myServer conn jwtSettings =
      Servant.hoistServerWithContext apiProxy ctxProxy (unRedis conn) $
        serviceProviderEndpoints jwtSettings
          :<|> mayAuthedEndpoint
          :<|> authedEndpoint
          :<|> errorEndpoint
    unRedis :: R.Connection -> R.Redis a -> Servant.Handler a
    unRedis conn m = liftIO $ R.runRedis conn m
    cookieDefaultTTL :: Maybe DiffTime
    cookieDefaultTTL = Just $ 60 * 60 * 24 * 7 -- 1 week
    cookieSettings :: Cookies.CookieSettings
    cookieSettings = Cookies.defaultCookieSettings onLocal cookieDefaultTTL
    spConfig :: JWT.JWTSettings -> ServiceProviderConfig
    spConfig jwtSettings =
      ServiceProviderConfig
        { cookieSettings,
          jwtSettings = jwtSettings,
          redirectAfterLogout = unsafeURI "http://cloud:3030/logged-out",
          oauthClientID = OAuthClientId "my-client-id",
          oauthClientSecret = OAuthClientSecret "my-client-secret",
          scopes = Scopes (Set.fromList [Scopes.OpenId, Scopes.Cloud]),
          baseServiceURI = api,
          serviceAudience,
          sessionCookieKey
        }
    onLocal = True
    -- Ensure you use cryptographically secure 32-byte keys in production use.
    -- And don't re-use keys.
    hs256Key = "example-32-byte-hs256Key-jayhuxr"
    edDSAKey = "example-32-byte-edDSAKey-dxencne"
    legacyKey = JWT.KeyDescription {JWT.key = hs256Key, JWT.alg = JWT.HS256}
    signingKey = JWT.KeyDescription {JWT.key = edDSAKey, JWT.alg = JWT.Ed25519}
    rotatedKeys = Set.empty
    api = unsafeURI "http://cloud:3030"
    serviceAudience = api
    acceptedAudiences = Set.singleton serviceAudience
    issuer = unsafeURI "http://localhost:5424"
    acceptedIssuers = Set.singleton issuer
