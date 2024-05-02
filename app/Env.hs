{-# LANGUAGE RecordWildCards #-}

module Env
  ( withEnv,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Char qualified as Char
import Data.Either.Combinators
import Data.Functor
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Redis qualified as Redis
import Enlil.Env
import Enlil.JWT qualified as JWT
import Enlil.Prelude
import Enlil.Utils.Deployment qualified as Deployment
import Enlil.Utils.Logging qualified as Logging
import Enlil.Utils.Servant.Cookies qualified as Cookies
import Enlil.Web.Authentication (cookieSessionTTL)
import Hasql.Pool qualified as Pool
import Network.URI (parseURI)
import Servant.API qualified as Servant
import System.Environment (lookupEnv)
import System.Exit
import System.Log.FastLogger qualified as FL
import System.Log.Raven qualified as Sentry
import System.Log.Raven.Transport.HttpConduit qualified as Sentry
import System.Log.Raven.Types qualified as Sentry
import Unison.Runtime.Interface as RT

withEnv :: (Env () -> IO a) -> IO a
withEnv action = do
  apiOrigin <- fromEnv "ENLIL_API_ORIGIN" (pure . maybeToEither "Invalid ENLIL_API_ORIGIN" . parseURI)
  serverPort <- fromEnv "ENLIL_SERVER_PORT" readPort
  shouldCheckForMigration <-
    lookupEnv "ENLIL_MIGRATE_ON_STARTUP" <&> \case
      Nothing -> False
      Just val -> case map Char.toLower val of
        'f' : _ -> False
        '0' : _ -> False
        [] -> False
        _ -> True
  postgresConfig <- fromEnv "ENLIL_POSTGRES" (pure . Right . Text.pack)
  postgresConnMax <- fromEnv "ENLIL_POSTGRES_CONN_MAX" (pure . maybeToEither "Invalid ENLIL_POSTGRES_CONN_MAX" . readMaybe)
  githubClientID <- fromEnv "ENLIL_GITHUB_CLIENTID" (pure . Right . Text.pack)
  githubClientSecret <- fromEnv "ENLIL_GITHUB_CLIENT_SECRET" (pure . Right . Text.pack)
  hs256Key <- fromEnv "ENLIL_HMAC_KEY" (pure . Right . BS.pack)
  zendeskAPIUser <- fromEnv "ENLIL_ZENDESK_API_USER" (pure . Right . BS.pack)
  zendeskAPIToken <- fromEnv "ENLIL_ZENDESK_API_TOKEN" (pure . Right . BS.pack)
  let zendeskAuth = Servant.BasicAuthData zendeskAPIUser zendeskAPIToken
  commitHash <- fromEnv "ENLIL_COMMIT" (pure . Right . Text.pack)
  minLogSeverity <-
    lookupEnv "ENLIL_LOG_LEVEL" >>= \case
      Nothing -> pure Logging.Info
      Just (map toUpper -> "DEBUG") -> pure Logging.Debug
      Just (map toUpper -> "INFO") -> pure Logging.Info
      Just (map toUpper -> "ERROR") -> pure Logging.Error
      Just (map toUpper -> "USERERROR") -> pure Logging.UserFault
      Just x -> putStrLn ("Unknown logging level: " <> x) >> exitWith (ExitFailure 1)
  shareUiOrigin <- fromEnv "ENLIL_SHARE_UI_ORIGIN" (pure . maybeToEither "Invalid ENLIL_SHARE_UI_ORIGIN" . parseURI)
  websiteOrigin <- fromEnv "ENLIL_HOMEPAGE_ORIGIN" (pure . maybeToEither "Invalid ENLIL_HOMEPAGE_ORIGIN" . parseURI)
  cloudUiOrigin <- fromEnv "ENLIL_CLOUD_UI_ORIGIN" (pure . maybeToEither "Invalid ENLIL_CLOUD_UI_ORIGIN" . parseURI)
  maxParallelismPerDownloadRequest <- fromEnv "ENLIL_MAX_PARALLELISM_PER_DOWNLOAD_REQUEST" (pure . maybeToEither "Invalid ENLIL_MAX_PARALLELISM_PER_DOWNLOAD_REQUEST" . readMaybe)
  maxParallelismPerUploadRequest <- fromEnv "ENLIL_MAX_PARALLELISM_PER_UPLOAD_REQUEST" (pure . maybeToEither "Invalid ENLIL_MAX_PARALLELISM_PER_UPLOAD_REQUEST" . readMaybe)
  cloudWebsiteOrigin <- fromEnv "ENLIL_CLOUD_HOMEPAGE_ORIGIN" (pure . maybeToEither "Invalid ENLIL_CLOUD_HOMEPAGE_ORIGIN" . parseURI)

  sentryService <-
    lookupEnv "ENLIL_SENTRY_DSN" >>= \case
      Nothing -> do
        putStrLn "No Sentry configuration detected."
        Sentry.disabledRaven
      Just dsn -> do
        let defaultTags = HM.fromList [("deployment", show Deployment.deployment), ("service", "share")]
        let sentryTags r = r {Sentry.srTags = defaultTags `HM.union` Sentry.srTags r}
        Sentry.initRaven dsn sentryTags Sentry.sendRecord Sentry.stderrFallback

  redisConfig <-
    (fromEnv "ENLIL_REDIS" (pure . Redis.parseConnectInfo)) <&> \r ->
      let tlsParams
            | Deployment.onLocal = Nothing
            | otherwise = Nothing
       in r {Redis.connectTLSParams = tlsParams}
  let acceptedAudiences = Set.singleton apiOrigin
  let jwtSettings = JWT.defaultJWTSettings hs256Key acceptedAudiences apiOrigin
  let cookieSettings = Cookies.defaultCookieSettings Deployment.onLocal (Just (realToFrac cookieSessionTTL))
  let sessionCookieKey = tShow Deployment.deployment <> "-share-session"
  redisConnection <- Redis.checkedConnect redisConfig
  pgConnectionPool <-
    Pool.acquire postgresConnMax Nothing (Text.encodeUtf8 postgresConfig)
  timeCache <- FL.newTimeCache FL.simpleTimeFormat -- E.g. 05/Sep/2023:13:23:56 -0700
  sandboxedRuntime <- RT.startRuntime True RT.Persistent "enlil"
  let requestCtx = ()
  -- We use a zero-width-space to separate log-lines on ingestion, this allows us to use newlines for
  -- formatting, but without affecting log-grouping.
  let zeroWidthSpace = "\x200B"
  FL.withFastLogger (FL.LogStderr FL.defaultBufSize) $ \logger -> do
    action $ Env {logger = (logger . (\msg -> zeroWidthSpace <> msg <> "\n")), ..}
  where
    readPort p = pure $ maybeToRight "ENLIL_PORT was not a number" (readMaybe p)

fromEnv :: String -> (String -> IO (Either String a)) -> IO a
fromEnv var from = do
  val <- lookupEnv var
  case val of
    Nothing -> exitErr (Left "Variable not set")
    Just val' -> do
      v <- from val'
      exitErr v
  where
    exitErr (Right a) = pure a
    exitErr (Left err) = putStrLn ("Error with " <> var <> ": " <> err) >> exitWith (ExitFailure 1)
