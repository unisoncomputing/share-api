{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Share
  ( startApp,
  )
where

import Control.Monad.Except
import Control.Monad.Random (randomIO)
import Control.Monad.Reader
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Typeable qualified as Typeable
import Data.UUID (UUID)
import Data.Vault.Lazy as Vault
import Ki.Unlifted qualified as Ki
import Network.HTTP.Types (HeaderName, statusCode)
import Network.HTTP.Types qualified as HTTP
import Network.URI qualified as URI
import Network.Wai
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal qualified as Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip qualified as Gzip
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import Servant
import Share.App
import Share.BackgroundJobs qualified as BackgroundJobs
import Share.BackgroundJobs.Monad (runBackground)
import Share.Env.Types qualified as Env
import Share.IDs (RequestId, UserId)
import Share.IDs qualified as IDs
import Share.JWT qualified as JWT
import Share.Metrics (requestMetricsMiddleware, serveMetricsMiddleware)
import Share.OAuth.Session (AuthCheckCtx, MaybeAuthenticatedUserId, addAuthCheckCtx)
import Share.Postgres.Notifications qualified as PGNotif
import Share.Prelude
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging (LogMsg (..), logErrorText)
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant
import Share.Utils.Servant.Cookies (CookieVal)
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Utils.Servant.RawRequest (RawRequest)
import Share.Web.API qualified as Web
import Share.Web.App (WebApp, localRequestCtx)
import Share.Web.App qualified as WebApp
import Share.Web.Errors
import Share.Web.Impl qualified as Web
import System.Log.FastLogger (FastLogger, FormattedTime, LogStr)
import System.Log.Raven qualified as Sentry
import System.Log.Raven.Types qualified as Sentry
import UnliftIO qualified
import UnliftIO.STM qualified as STM

startApp :: Env.Env () -> IO ()
startApp env = do
  app <- mkShareServer env
  Ki.scoped \scope -> do
    runBackground env "notifications-init" $ do PGNotif.initialize scope
    runBackground env "background-jobs" $ do BackgroundJobs.startWorkers scope
    run (Env.serverPort env) app

newtype UncaughtException err = UncaughtException err
  deriving stock (Show)

instance (Show err) => Logging.Loggable (UncaughtException err) where
  toLog = Logging.withSeverity Logging.UserFault . Logging.showLog

instance ToServerError (UncaughtException a) where
  toServerError _ = (ErrorID "uncaught-exception", internalServerError)

-- | Converts every exception into a ServerError to ensure users always get a reasonable
-- response even if it's a 500.
toServantHandler :: Env.Env () -> WebApp a -> Handler a
toServantHandler env appM =
  let catchErrors m = do
        UnliftIO.tryAny m >>= \case
          Left (UnliftIO.SomeException (Typeable.cast @_ @ServerError -> Just serverErr)) -> do
            pure $ Left serverErr
          Left (UnliftIO.SomeException (Typeable.cast @_ @SomeServerError -> Just (SomeServerError serverErr))) -> do
            Logging.logMsg $ Logging.toLog serverErr
            let (_errId, servantErr) = toServerError serverErr
            pure $ Left servantErr
          Left (UnliftIO.SomeException err) -> do
            let addSentryData sr =
                  sr
                    { Sentry.srEnvironment = Just (show Deployment.deployment),
                      Sentry.srCulprit = Just "critical-uncaught-exception",
                      Sentry.srLevel = Sentry.Fatal
                    }
            sentryService <- asks Env.sentryService
            liftIO $ Sentry.register sentryService "errors" Sentry.Error (show err) addSentryData
            logErrorText ("Uncaught exception: " <> tShow err)
            pure $ Left err500
          Right a -> pure (Right a)
   in Handler . ExceptT $ do
        -- fresh request ctx for each request.
        reqCtx <- WebApp.freshRequestCtx
        runAppM (env {Env.ctx = reqCtx}) $ catchErrors appM

-- | Uses context from the request to set up an appropriate RequestCtx
type WrapperAPI = (RawRequest :> Header "X-NO-CACHE" Text :> Cookies.Cookie "NO-CACHE" Text :> Header "X-RequestID" RequestId :> MaybeAuthenticatedUserId :> Web.API)

mkShareServer :: Env.Env () -> IO Application
mkShareServer env = do
  reqTagsKey <- Vault.newKey
  let reqLoggerMiddleware = mkReqLogger reqTagsKey (Env.timeCache env) (Env.logger env)
  metricsMiddleware <- serveMetricsMiddleware env
  openTelemetryMiddleware <- newOpenTelemetryWaiMiddleware
  let appCtx :: (Context '[Cookies.CookieSettings, JWT.JWTSettings])
      appCtx = Env.cookieSettings env :. Env.jwtSettings env :. EmptyContext
  let ctx :: Context (AuthCheckCtx .++ '[Cookies.CookieSettings, JWT.JWTSettings])
      ctx = addAuthCheckCtx (Env.cookieSettings env) (Env.jwtSettings env) (Env.sessionCookieKey env) appCtx
  let waiApp =
        appServer reqTagsKey
          & hoistServerWithContext appAPI ctxType (toServantHandler env)
          & serveWithContext appAPI ctx
          & openTelemetryMiddleware
          & requestIDMiddleware
          & requestMetricsMiddleware Web.api
          & metricsMiddleware
          & skipOnLocal corsMiddleware
          & Gzip.gzip gzipSettings
          & reqLoggerMiddleware
  pure waiApp
  where
    gzipSettings =
      Gzip.def
        { Gzip.gzipFiles = Gzip.GzipCompress,
          Gzip.gzipCheckMime = \mime -> Gzip.defaultCheckMime mime || mime == "application/cbor"
        }
    ctxType = Proxy @(AuthCheckCtx .++ '[Cookies.CookieSettings, JWT.JWTSettings])
    uriFromReq req =
      (Env.apiOrigin env)
        { uriPath = BSC.unpack $ rawPathInfo req,
          uriQuery = BSC.unpack $ rawQueryString req
        }
    -- Add some global context to the request.
    appServer :: Vault.Key WebApp.ReqTagsVar -> Wai.Request -> Maybe Text -> (Maybe (CookieVal "NO-CACHE" Text)) -> Maybe RequestId -> Maybe UserId -> ServerT Web.API WebApp
    appServer reqTagsKey req noCacheHeader (Cookies.cookieVal -> noCacheCookie) mayRequestID mayUserId =
      let reqMethod = Wai.requestMethod req
          addReqCtx m = do
            let isSet = \case
                  Nothing -> False
                  Just v
                    | lower <- Text.toLower v, lower == "false" -> False
                    | otherwise -> True
            let useCaching = not Deployment.onLocal && not (isSet noCacheHeader) && not (isSet noCacheCookie)
            let reqTags =
                  Map.fromList
                    [ ("caching-disabled", showBool $ not useCaching),
                      ("request-id", maybe "<none>" IDs.toText mayRequestID),
                      ("authenticated-user-id", maybe "<unauthenticated>" IDs.toText mayUserId),
                      ("path", Text.intercalate "/" $ pathInfo req),
                      ("commit", tShow (Env.commitHash env))
                    ]
            localRequestCtx
              ( \reqCtx ->
                  reqCtx
                    { WebApp.useCaching = useCaching,
                      WebApp.requestId = mayRequestID,
                      WebApp.authenticatedUser = mayUserId,
                      WebApp.pathInfo = pathInfo req,
                      WebApp.rawURI = Just $ uriFromReq req,
                      -- If there's a request tags var set on the request's vault via middleware, use that.
                      WebApp.reqTagsVar = fromMaybe (WebApp.reqTagsVar reqCtx) . Vault.lookup reqTagsKey . Wai.vault $ req
                    }
              )
              do
                reqTagsVar <- asks (WebApp.reqTagsVar . Env.ctx)
                STM.atomically $ STM.modifyTVar' reqTagsVar (<> reqTags)
                m

          -- Individual endpoints my specify a shorter timeout if they like, but we
          -- shouldn't compromise our global limit.
          -- Admin/local endpoints have a longer timeout to accomodate things like migrations.
          timeoutSeconds =
            case (Wai.pathInfo req) of
              -- Very long timeouts on local or admin routes
              ("admin" : _) -> (24 * 60 * 60)
              ("local" : _) -> (24 * 60 * 60)
              -- Temporary timeout extension for UCM until I can get on top of some of the new
              -- perf issues.
              ("ucm" : "v1" : _) -> 15 * 60
              -- GET requests shouldn't be doing much hard-work.
              -- Usually if they take a long time it's due to something like an infinite loop
              -- in a doc render and we want to shut it down before it consumes too many
              -- resources.
              _ | reqMethod == HTTP.methodGet -> 30
              -- All other requests (POST, PUT, PATCH, etc.) have a 120 second timeout.
              _ -> 120
       in hoistServerWithContext Web.api ctxType (addReqCtx . reportExceptions . withTimeoutSeconds (timeoutSeconds * localMultiplier)) (Web.server)

    -- Bigger timeouts on local
    localMultiplier :: NominalDiffTime
    localMultiplier = if Deployment.onLocal then 10 else 1

    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

    -- Ensure we log and report all non-server-error exceptions
    reportExceptions :: WebApp a -> WebApp a
    reportExceptions m =
      UnliftIO.tryAny m >>= \case
        Left (UnliftIO.SomeException (Typeable.cast @_ @ServerError -> Just serverErr)) -> do
          UnliftIO.throwIO serverErr
        Left (UnliftIO.SomeException (Typeable.cast @_ @SomeServerError -> Just (SomeServerError serverErr))) -> do
          respondError serverErr
        Left (UnliftIO.SomeException err) -> do
          respondError $ UncaughtException err
        Right a -> pure a
    appAPI :: Proxy WrapperAPI
    appAPI = Proxy
    skipOnLocal :: Middleware -> Middleware
    skipOnLocal m = if Deployment.onLocal then id else m
    corsPolicy :: Request -> Maybe CorsResourcePolicy
    corsPolicy req = case Deployment.deployment of
      Deployment.Local -> Nothing
      Deployment.Staging ->
        case (List.lookup "Origin" $ requestHeaders req) >>= URI.parseAbsoluteURI . BSC.unpack of
          Just uri
            | isValidSubdomain uri ->
                let rootSubdomain = uri {uriPath = [], uriQuery = [], uriFragment = []}
                 in -- Allow requests from any subdomain of the share staging origin.
                    Just $ defaultCorsPolicy [BSC.pack . show @URI $ rootSubdomain]
          _ -> Just $ defaultCorsPolicy []
      Deployment.Production ->
        Just $ defaultCorsPolicy []
      where
        isValidSubdomain origin = fromMaybe False $ do
          originHostParts <- List.splitOn "." . URI.uriRegName <$> URI.uriAuthority origin
          shareHostParts <- List.splitOn "." . URI.uriRegName <$> URI.uriAuthority (Env.shareUiOrigin env)
          cloudHostParts <- List.splitOn "." . URI.uriRegName <$> URI.uriAuthority (Env.cloudUiOrigin env)
          pure $ shareHostParts `List.isSuffixOf` originHostParts || cloudHostParts `List.isSuffixOf` originHostParts
        -- Require that the request come from one of the known cloud or share origins.
        defaultCorsPolicy additionalOrigins =
          simpleCorsResourcePolicy
            { corsOrigins = Just ([BSC.pack . show @URI $ Env.shareUiOrigin env, BSC.pack . show @URI $ Env.cloudUiOrigin env] <> additionalOrigins, True {- allow receiving cookies in requests made from these origins -}),
              corsRequestHeaders = "X-XSRF-TOKEN" : simpleHeaders,
              corsMethods = ["PATCH", "DELETE", "PUT"] <> simpleMethods
            }
    corsMiddleware :: Middleware
    corsMiddleware = cors corsPolicy

mkReqLogger :: Vault.Key WebApp.ReqTagsVar -> IO FormattedTime -> FastLogger -> Middleware
mkReqLogger reqTagsKey timeCache logger app = do
  \req responder -> do
    let hasDebugHeader = any (\(headerName, _) -> headerName == "X-DEBUG") (Wai.requestHeaders req)
    if Deployment.onLocal || hasDebugHeader
      then verboseRequestResponseLogger app req responder
      else standardReqLoggingMiddleware app req responder
  where
    formatter :: FormattedTime -> Request -> HTTP.Status -> NominalDiffTime -> Map Text Text -> LogStr
    formatter timestamp req (statusCode -> respStatus) responseTimeSeconds reqTags =
      Logging.logFmtFormatter timestamp $
        LogMsg
          { severity = statusSeverity respStatus,
            callstack = Nothing,
            msg = "",
            tags =
              Map.fromList
                [ ("status", tShow respStatus),
                  ("response-time-ms", tShow (realToFrac @NominalDiffTime @Double responseTimeSeconds * 1000)),
                  ("user-agent", maybe "" Text.decodeUtf8 $ requestHeaderUserAgent req),
                  ("method", Text.decodeUtf8 $ requestMethod req),
                  ("path", Text.decodeUtf8 $ rawPathInfo req),
                  ("request-id", Text.decodeUtf8 . fromMaybe "" . Prelude.lookup requestIDHeader . Wai.requestHeaders $ req)
                ]
                <> reqTags
          }
    statusSeverity :: Int -> Logging.Severity
    statusSeverity = \case
      status
        | status >= 500 -> Logging.Error
        | status >= 400 -> Logging.UserFault
        | otherwise -> Logging.Info
    standardReqLoggingMiddleware :: Middleware
    standardReqLoggingMiddleware app req responder = do
      t0 <- getCurrentTime
      -- Stash a request specific TVar in the vault so we can add tags to it during the
      -- request, but can still access it when formatting logs for the request.
      reqTagsV <- STM.newTVarIO mempty
      let newVault = Vault.insert reqTagsKey reqTagsV (Wai.vault req)
      let req' = req {Wai.vault = newVault}
      app req' $ \res -> do
        t1 <- getCurrentTime
        date <- liftIO timeCache
        rspRcv <- responder res
        let status = responseStatus res
            duration = t1 `diffUTCTime` t0
        reqTags <- STM.readTVarIO reqTagsV
        liftIO . logger $ formatter date req status duration reqTags
        return rspRcv

requestIDHeader :: HeaderName
requestIDHeader = "X-RequestID"

-- Middleware that generates a random UUID for each request, and modifies both the request and response headers to
-- include it.
requestIDMiddleware :: Middleware
requestIDMiddleware app req responder = do
  reqID <- randomIO @UUID
  let header = (requestIDHeader, Text.encodeUtf8 . tShow $ reqID)
  app
    req {requestHeaders = header : requestHeaders req}
    \response -> responder (Wai.mapResponseHeaders (header :) response)

verboseRequestResponseLogger :: Middleware
verboseRequestResponseLogger app req responder = do
  case requestBodyLength req of
    ChunkedBody -> putStrLn "Request Body: Unknown Size"
    KnownLength wo -> putStrLn $ "Request Body: " <> show wo <> " bytes"
  putStrLn $ "Request Headers: " <> show (requestHeaders req)
  logStdoutDev app req $ \resp -> do
    -- Response headers
    putStrLn $ "Response Headers: " <> show (responseHeaders resp)
    BL.putStr $ "Response Body: "
    case resp of
      Wai.ResponseFile _ _ filePath _ -> putStrLn $ "<ResponseFile: " <> filePath <> ">"
      Wai.ResponseBuilder _ _ builder -> do
        let bytes = Builder.toLazyByteString builder
        putStrLn $ show (BL.length bytes) <> " bytes ("
        BL.putStrLn $ Builder.toLazyByteString builder
        BL.putStrLn ")"
      Wai.ResponseStream {} -> putStrLn "<ResponseStream>"
      Wai.ResponseRaw {} -> putStrLn "<ResponseRaw>"
    responder resp
