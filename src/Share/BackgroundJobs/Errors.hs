module Share.BackgroundJobs.Errors () where

import Control.Exception (SomeException)
import Data.Typeable qualified as Typeable
import Share.BackgroundJobs.Monad
import Share.Monitoring qualified as Monitoring
import Share.Prelude
import Share.Utils.Logging (Loggable)
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (ToServerError)
import UnliftIO qualified

-- | Log any exceptions which occur within the worker.
-- Catch and return synchronous exceptions, but just log and rethrow async exceptions.
reportException :: SomeException -> Background a -> Background (Either SomeException a)
reportException e bg = flip UnliftIO.withException reportError do
  UnliftIO.tryAny m >>= \case
    Left (UnliftIO.SomeException err) -> reportError $ UncaughtException err
    Right a -> pure a

-- | Logs the error with a call stack, but doesn't abort the request or render an error to the client.
reportError :: (HasCallStack, Loggable e) => e -> Background ()
reportError e = do
  asks (workerName . req)
  let errLog = Logging.toLog e
  Monitoring.reportError env coreTags extraTags errID e
  logMsg (withSeverity Error $ errLog)
