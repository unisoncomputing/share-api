{-# LANGUAGE GADTs #-}

module Share.BackgroundJobs.Errors
  ( reportException,
    reportError,
    throwSomeServerError,
  )
where

import Control.Exception (SomeException)
import Control.Monad.Except
import Data.HashMap.Lazy qualified as HM
import Data.Typeable qualified as Typeable
import Share.BackgroundJobs.Monad
import Share.Env qualified as Env
import Share.Monitoring qualified as Monitoring
import Share.Prelude
import Share.Utils.Logging (Loggable)
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (InternalServerError (..), withCallstack)
import Data.Map qualified as Map
import UnliftIO qualified
import Data.Text qualified as Text
import Data.Aeson qualified as Aeson

data SomeBackgroundError where
  SomeBackgroundError :: (Typeable e, Loggable e) => e -> SomeBackgroundError

instance Loggable SomeBackgroundError where
  toLog (SomeBackgroundError e) = Logging.toLog e

throwSomeServerError :: (Loggable e, MonadError SomeBackgroundError m, HasCallStack, Typeable e) => e -> m a
throwSomeServerError = throwError . SomeBackgroundError . withCallstack

-- | Log any exceptions which occur within the worker.
-- Catch and return synchronous exceptions, but just log and rethrow async exceptions.
reportException :: Background a -> Background (Either SomeException a)
reportException bg = flip (UnliftIO.withException @_ @SomeException) (reportError . InternalServerError "uncaught-async-exception") do
  UnliftIO.tryAny bg >>= \case
    Left e@(UnliftIO.SomeException err)
      | Just (SomeBackgroundError bgErr) <- Typeable.cast err -> do
          reportError bgErr
          pure $ Left e
      | otherwise -> do
          reportError $ InternalServerError "unknown-exception" err
          pure $ Left e
    Right a -> pure $ Right a

-- | Logs the error with a call stack, but doesn't abort the request or render an error to the client.
reportError :: (HasCallStack, Loggable e) => e -> Background ()
reportError e = do
  env <- ask
  let BackgroundCtx {workerName, loggingTags} = Env.ctx env
  let coreTags = HM.fromList [("workerName", into @String workerName)]
  let extraTags =
        loggingTags
        & Map.toList
        & (fmap \(k, v) -> (Text.unpack k, Aeson.toJSON v))
        & HM.fromList
  let errID = "background-job:" <> workerName
  let errLog = Logging.toLog e
  Monitoring.reportError env coreTags extraTags errID e
  Logging.logMsg (Logging.withSeverity Logging.Error $ errLog)
