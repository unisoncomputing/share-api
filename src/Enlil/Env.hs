module Enlil.Env
  ( Env (..),
  )
where

import Database.Redis qualified as R
import Enlil.JWT qualified as JWT
import Enlil.Prelude
import Enlil.Utils.Logging.Types qualified as Logging
import Enlil.Utils.Servant.Cookies qualified as Cookies
import Hasql.Pool qualified as Hasql
import Network.URI (URI)
import Servant qualified as S
import System.Log.FastLogger (FormattedTime, LogStr)
import System.Log.Raven.Types (SentryService)
import Unison.Codebase.Runtime (Runtime)
import Unison.Symbol (Symbol)

data Env reqCtx = Env
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
    serverPort :: Int,
    redisConfig :: R.ConnectInfo,
    postgresConfig :: Text,
    githubClientID :: Text,
    githubClientSecret :: Text,
    jwtSettings :: JWT.JWTSettings,
    cookieSettings :: Cookies.CookieSettings,
    sessionCookieKey :: Text,
    sandboxedRuntime :: Runtime Symbol,
    zendeskAuth :: S.BasicAuthData,
    sentryService :: SentryService,
    -- The commit hash of the currently running version of Enlil
    commitHash :: Text,
    requestCtx :: reqCtx,
    shouldCheckForMigration :: Bool,
    -- The maximum number of workers to use for concurrent work on a single request.
    -- E.g. sync can parallelize signing/verifying JWTs or run multiple transactions against
    -- PG. If this goes too high we can end up tanking Share.
    maxParallelismPerDownloadRequest :: Int,
    maxParallelismPerUploadRequest :: Int
  }
