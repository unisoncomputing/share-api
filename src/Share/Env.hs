module Share.Env
  ( Env (..),
  )
where

import Crypto.JOSE.JWK qualified as JWK
import Database.Redis qualified as R
import Hasql.Pool qualified as Hasql
import Network.HTTP.Client qualified as HTTPClient
import Network.URI (URI)
import OpenTelemetry.Trace qualified as Trace
import Servant.Client qualified as S
import Share.JWT qualified as JWT
import Share.Prelude
import Share.Utils.Logging.Types qualified as Logging
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Utils.Tags (HasTags (..))
import System.Log.FastLogger (FormattedTime, LogStr)
import System.Log.Raven.Types (SentryService)
import Unison.Codebase.Runtime (Runtime)
import Unison.Symbol (Symbol)
import Vault qualified

data Env ctx = Env
  { redisConnection :: R.Connection,
    pgConnectionPool :: Hasql.Pool,
    logger :: LogStr -> IO (),
    timeCache :: IO FormattedTime,
    minLogSeverity :: Logging.Severity,
    -- Config
    apiOrigin :: URI, -- E.g. "https://api.unison-lang.org"
    shareUiOrigin :: URI, -- E.g. "https://share.unison-lang.org"
    websiteOrigin :: URI, -- E.g. "https://www.unison-lang.org"
    cloudUiOrigin :: URI, -- E.g. "https://app.unison.cloud"
    cloudWebsiteOrigin :: URI, -- E.g. "https://www.unison.cloud"
    supportTicketWebhookURI :: Maybe URI,
    vaultClientEnv :: S.ClientEnv,
    userSecretsVaultMount :: Vault.SecretMount,
    shareVaultToken :: Vault.VaultToken,
    -- An HTTP client for making requests which are configured by end users, and thus
    -- shouldn't
    proxiedHttpClient :: HTTPClient.Manager,
    serverPort :: Int,
    redisConfig :: R.ConnectInfo,
    postgresConfig :: Text,
    githubClientID :: Text,
    githubClientSecret :: Text,
    jwtSettings :: JWT.JWTSettings,
    hashJWTJWK :: JWK.JWK,
    cookieSettings :: Cookies.CookieSettings,
    sessionCookieKey :: Text,
    sandboxedRuntime :: Runtime Symbol,
    sentryService :: SentryService,
    -- The commit hash of the currently running version of Share
    commitHash :: Text,
    ctx :: ctx,
    shouldCheckForMigration :: Bool,
    -- The maximum number of workers to use for concurrent work on a single request.
    -- E.g. sync can parallelize signing/verifying JWTs or run multiple transactions against
    -- PG. If this goes too high we can end up tanking Share.
    maxParallelismPerDownloadRequest :: Int,
    maxParallelismPerUploadRequest :: Int,
    -- OpenTelemetry tracing
    tracer :: Trace.Tracer
  }
  deriving (Functor, Generic)

instance (HasTags ctx) => HasTags (Env ctx) where
  getTags = getTags . ctx
  addTags newTags env = env {ctx = addTags newTags (ctx env)}
