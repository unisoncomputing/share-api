module Share.Monitoring where

import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.Stack qualified as Stack
import Network.URI (URI)
import Share.Env.Types
import Share.Env.Types qualified as Env
import Share.Prelude
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging
import Share.Utils.Logging qualified as Logging
import System.Log.Raven qualified as Sentry
import System.Log.Raven.Types qualified as Sentry

-- | Logs the error with a call stack, but doesn't abort the request or render an error to the client.
reportError :: (MonadIO m, HasCallStack, Loggable e, MonadLogger m) => Env ctx -> (HM.HashMap String String) -> HM.HashMap String Aeson.Value -> Text -> e -> m ()
reportError (Env.Env {Env.apiOrigin = host, Env.sentryService = sentryService, commitHash}) tags extraTags errID e = do
  let errLog = withTag ("error-id", errID) $ toLog e
  logMsg (withSeverity Error $ errLog)
  let addSentryData sr =
        sr
          { Sentry.srEnvironment = Just (show Deployment.deployment),
            Sentry.srCulprit = Just (Text.unpack errID),
            Sentry.srLevel = Sentry.Error,
            Sentry.srTags =
              tags
                & HM.insert "host" (show @URI host)
                & HM.insert "errorID" (Text.unpack errID)
                & HM.insert "commitHash" (Text.unpack commitHash),
            Sentry.srExtra =
              extraTags
                <> (HM.fromList . fmap (bimap Text.unpack Aeson.String) . Map.toList $ (Logging.tags errLog))
                <> HM.singleton "callstack" (Aeson.String . Text.pack $ Stack.prettyCallStack Stack.callStack)
          }
  liftIO $ Sentry.register sentryService "errors" Sentry.Error (Text.unpack $ Logging.msg errLog) addSentryData
