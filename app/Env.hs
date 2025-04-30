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
import Share.Env
import Share.JWT qualified as JWT
import Share.Prelude
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Web.Authentication (cookieSessionTTL)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Network.URI (parseURI, uriScheme)
import Servant.API qualified as Servant
import Servant.Client qualified as ServantClient
import System.Environment (lookupEnv)
import System.Exit
import System.Log.FastLogger qualified as FL
import System.Log.Raven qualified as Sentry
import System.Log.Raven.Transport.HttpConduit qualified as Sentry
import System.Log.Raven.Types qualified as Sentry
import Unison.Runtime.Interface as RT
import Data.Time.Clock qualified as Time
import Network.HTTP.Client.TLS qualified as TLS
import Network.HTTP.Client qualified as HTTPClient
import Vault qualified
import Data.ByteString.Char8 qualified as BSC

withEnv :: (Env () -> IO a) -> IO a
withEnv action = do
  apiOrigin <- fromEnv "SHARE_API_ORIGIN" (pure . maybeToEither "Invalid SHARE_API_ORIGIN" . parseURI)
  serverPort <- fromEnv "SHARE_SERVER_PORT" readPort
  shouldCheckForMigration <-
    lookupEnv "SHARE_MIGRATE_ON_STARTUP" <&> \case
      Nothing -> False
      Just val -> case map Char.toLower val of
        'f' : _ -> False
        '0' : _ -> False
        [] -> False
        _ -> True
  postgresConfig <- fromEnv "SHARE_POSTGRES" (pure . Right . Text.pack)
  postgresConnMax <- fromEnv "SHARE_POSTGRES_CONN_MAX" (pure . maybeToEither "Invalid SHARE_POSTGRES_CONN_MAX" . readMaybe)
  githubClientID <- fromEnv "SHARE_GITHUB_CLIENTID" (pure . Right . Text.pack)
  githubClientSecret <- fromEnv "SHARE_GITHUB_CLIENT_SECRET" (pure . Right . Text.pack)
  hs256Key <- fromEnv "SHARE_HMAC_KEY" (pure . Right . BS.pack)
  edDSAKey <- fromEnv "SHARE_EDDSA_KEY" (pure . Right . BS.pack)
  zendeskAPIUser <- fromEnv "SHARE_ZENDESK_API_USER" (pure . Right . BS.pack)
  zendeskAPIToken <- fromEnv "SHARE_ZENDESK_API_TOKEN" (pure . Right . BS.pack)
  let zendeskAuth = Servant.BasicAuthData zendeskAPIUser zendeskAPIToken
  commitHash <- fromEnv "SHARE_COMMIT" (pure . Right . Text.pack)
  minLogSeverity <-
    lookupEnv "SHARE_LOG_LEVEL" >>= \case
      Nothing -> pure Logging.Info
      Just (map toUpper -> "DEBUG") -> pure Logging.Debug
      Just (map toUpper -> "INFO") -> pure Logging.Info
      Just (map toUpper -> "ERROR") -> pure Logging.Error
      Just (map toUpper -> "USERERROR") -> pure Logging.UserFault
      Just x -> putStrLn ("Unknown logging level: " <> x) >> exitWith (ExitFailure 1)
  shareUiOrigin <- fromEnv "SHARE_SHARE_UI_ORIGIN" (pure . maybeToEither "Invalid SHARE_SHARE_UI_ORIGIN" . parseURI)
  websiteOrigin <- fromEnv "SHARE_HOMEPAGE_ORIGIN" (pure . maybeToEither "Invalid SHARE_HOMEPAGE_ORIGIN" . parseURI)
  cloudUiOrigin <- fromEnv "SHARE_CLOUD_UI_ORIGIN" (pure . maybeToEither "Invalid SHARE_CLOUD_UI_ORIGIN" . parseURI)
  maxParallelismPerDownloadRequest <- fromEnv "SHARE_MAX_PARALLELISM_PER_DOWNLOAD_REQUEST" (pure . maybeToEither "Invalid SHARE_MAX_PARALLELISM_PER_DOWNLOAD_REQUEST" . readMaybe)
  maxParallelismPerUploadRequest <- fromEnv "SHARE_MAX_PARALLELISM_PER_UPLOAD_REQUEST" (pure . maybeToEither "Invalid SHARE_MAX_PARALLELISM_PER_UPLOAD_REQUEST" . readMaybe)
  cloudWebsiteOrigin <- fromEnv "SHARE_CLOUD_HOMEPAGE_ORIGIN" (pure . maybeToEither "Invalid SHARE_CLOUD_HOMEPAGE_ORIGIN" . parseURI)

  sentryService <-
    lookupEnv "SHARE_SENTRY_DSN" >>= \case
      Nothing -> do
        putStrLn "No Sentry configuration detected."
        Sentry.disabledRaven
      Just dsn -> do
        let defaultTags = HM.fromList [("deployment", show Deployment.deployment), ("service", "share")]
        let sentryTags r = r {Sentry.srTags = defaultTags `HM.union` Sentry.srTags r}
        Sentry.initRaven dsn sentryTags Sentry.sendRecord Sentry.stderrFallback

  redisConfig <-
    (fromEnv "SHARE_REDIS" (pure . Redis.parseConnectInfo)) <&> \r ->
      let tlsParams
            | Deployment.onLocal = Nothing
            | otherwise = Nothing
       in r {Redis.connectTLSParams = tlsParams}
  let acceptedAudiences = Set.singleton apiOrigin
  let legacyKey = JWT.KeyDescription {JWT.key = hs256Key, JWT.alg = JWT.HS256}
  let signingKey = JWT.KeyDescription {JWT.key = edDSAKey, JWT.alg = JWT.Ed25519}
  hashJWTJWK <- case JWT.keyDescToJWK legacyKey of
    Left err -> throwIO err
    Right (_thumbprint, jwk) -> pure jwk
  -- I explicitly add the legacy key to the validation keys, so that the thumbprinted
  -- version of the key is used for validation, which is needed for HashJWTs which are signed
  -- with a 'kid'.
  let validationKeys = Set.fromList [legacyKey]
  jwtSettings <- case JWT.defaultJWTSettings signingKey (Just legacyKey) validationKeys acceptedAudiences apiOrigin of
    Left cryptoError -> throwIO cryptoError
    Right settings -> pure settings
  let cookieSettings = Cookies.defaultCookieSettings Deployment.onLocal (Just (realToFrac cookieSessionTTL))
  let sessionCookieKey = tShow Deployment.deployment <> "-share-session"
  redisConnection <- Redis.checkedConnect redisConfig
  -- Set some very conservative defaults
  let pgConnectionAcquisitionTimeout = Time.secondsToDiffTime 60 -- 1 minute
  -- Helps prevent leaking connections if they somehow get forgotten about.
  let pgConnectionMaxIdleTime = Time.secondsToDiffTime (60 * 5) -- 5 minutes
  -- Limiting max lifetime helps cycle connections which may have accumulated memory cruft.
  let pgConnectionMaxLifetime = Time.secondsToDiffTime (60 * 60) -- 1 hour
  let pgSettings = Pool.settings [Pool.staticConnectionSettings (Text.encodeUtf8 postgresConfig), Pool.size postgresConnMax, Pool.acquisitionTimeout pgConnectionAcquisitionTimeout, Pool.idlenessTimeout pgConnectionMaxIdleTime, Pool.agingTimeout pgConnectionMaxLifetime]
  pgConnectionPool <- Pool.acquire pgSettings
  timeCache <- FL.newTimeCache FL.simpleTimeFormat -- E.g. 05/Sep/2023:13:23:56 -0700
  sandboxedRuntime <- RT.startRuntime True RT.Persistent "share"

  -- Vault setup
  unproxiedHttpClient <- TLS.newTlsManager
  vaultHost <- fromEnv "VAULT_HOST" parseBaseUrl
  userSecretsVaultMount <- fromEnv "USER_SECRETS_VAULT_MOUNT" ((fmap . fmap) Vault.SecretMount . nonEmptyTextParser "USER_SECRETS_VAULT_MOUNT")
  shareVaultToken <- fromEnv "VAULT_TOKEN" ((fmap . fmap) Vault.VaultToken . nonEmptyTextParser "VAULT_TOKEN")
  let vaultClientEnv = ServantClient.mkClientEnv unproxiedHttpClient vaultHost



  proxiedHttpClient <- do
    if  Deployment.onLocal
       then TLS.newTlsManager
       else do
          httpProxyHost <- fromEnv "SHARE_PROXY_HOST" (\proxyHost -> case parseURI proxyHost of
            Nothing -> pure $ Left "Invalid SHARE_PROXY_ADDRESS"
            Just uri -> if uriScheme uri == "http:" || uriScheme uri == "https:"
              then pure $ Right (BSC.pack proxyHost)
              else pure $ Left "SHARE_PROXY_ADDRESS must be http or https")
          httpProxyPort <- fromEnv "SHARE_PROXY_PORT" (pure . maybeToEither "Invalid SHARE_PROXY_PORT" . readMaybe)

          -- http proxy setup
          let proxyOverride = HTTPClient.useProxy (HTTPClient.Proxy{HTTPClient.proxyHost = httpProxyHost, HTTPClient.proxyPort = httpProxyPort})
          let proxiedManagerSettings =
                TLS.tlsManagerSettings
                & HTTPClient.managerSetProxy proxyOverride
          TLS.newTlsManagerWith proxiedManagerSettings

  -- Logging setup
  let ctx = ()
  -- We use a zero-width-space to separate log-lines on ingestion, this allows us to use newlines for
  -- formatting, but without affecting log-grouping.
  let zeroWidthSpace = "\x200B"
  FL.withFastLogger (FL.LogStderr FL.defaultBufSize) $ \logger -> do
    action $ Env {logger = (logger . (\msg -> zeroWidthSpace <> msg <> "\n")), ..}
  where
    readPort p = pure $ maybeToRight "SHARE_PORT was not a number" (readMaybe p)
    nonEmptyTextParser :: Text -> String -> IO (Either String Text)
    nonEmptyTextParser varName = \case
      "" -> pure . Left . Text.unpack $ "Expected a value for env var " <> varName <> ", but got an empty string"
      str -> pure . Right $ Text.pack str

    parseBaseUrl :: String -> IO (Either String ServantClient.BaseUrl)
    parseBaseUrl str = do
      u <- ServantClient.parseBaseUrl str
      pure $ Right u

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
