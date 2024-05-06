{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Share.Utils.Logging
  ( -- * Message builders
    Loggable (..),
    withCallstackIfUnset,
    withCurrentCallstackIfUnset,
    withTag,
    withSeverity,
    showLog,
    textLog,
    logDebugText,
    logInfoText,
    logUserFaultText,
    logErrorText,
    ioLogger,
    logFmtFormatter,

    -- * Logger Monads
    Logger,
    LoggerT (..),
    MonadLogger (..),
    runLoggerT,
    runLoggerTEnv,

    -- * Other
    module X,
  )
where

import Control.Monad.Reader
import Data.Char qualified as Char
import Data.Coerce
import Data.Kind
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Share.Env qualified as Env
import Share.OAuth.Errors (OAuth2Error)
import Share.OAuth.Types (RedirectReceiverErr)
import Share.Prelude
import Share.Utils.Deployment (deployment)
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging.Types as X
import GHC.Stack (CallStack, callStack, prettyCallStack)
import Servant.Client qualified as Servant
import System.Log.FastLogger qualified as FL
import Unison.Server.Backend qualified as Backend
import Unison.Sync.Types qualified as Sync
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Monoid qualified as Monoid
import UnliftIO qualified as IO
import Prelude hiding (log)

type Logger = FL.LogStr -> IO ()

newtype LoggerT m a = LoggerT (ReaderT (Logger, IO FL.FormattedTime, Severity, Map Text Text) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

type Representational m = (forall a b. (Coercible a b) => Coercible (m a) (m b) :: Constraint)

deriving instance (MonadUnliftIO m, Representational m) => MonadUnliftIO (LoggerT m)

instance (MonadIO m) => MonadLogger (LoggerT m) where
  logMsg msg = LoggerT $ do
    (logger, getTime, minSeverity, tags') <- ask
    when (severity msg >= minSeverity) . liftIO $ do
      timestamp <- getTime
      logger . formatter timestamp $ msg {tags = tags msg `Map.union` tags'}
    where
      formatter = if Deployment.onLocal then localLogFmtLogger else logFmtFormatter

runLoggerT :: Severity -> Logger -> Map Text Text -> IO FL.FormattedTime -> LoggerT m a -> m a
runLoggerT minSeverity l reqTags ft (LoggerT m) = runReaderT m (l, ft, minSeverity, reqTags)

runLoggerTEnv :: Env.Env reqCtx -> Map Text Text -> LoggerT m a -> m a
runLoggerTEnv (Env.Env {Env.timeCache, Env.logger, Env.minLogSeverity}) tags m =
  runLoggerT minLogSeverity logger tags timeCache $ m

class Loggable msg where
  toLog :: msg -> LogMsg

-- Helpful for cases where errors are Void
instance Loggable Void where
  toLog = absurd

instance Loggable Servant.ClientError where
  toLog = withSeverity Error . showLog

instance Loggable Backend.BackendError where
  toLog = withSeverity UserFault . showLog

instance Loggable RedirectReceiverErr where
  toLog = withSeverity UserFault . textLog . tShow

instance Loggable OAuth2Error where
  toLog = withSeverity UserFault . showLog

class (Monad m) => MonadLogger m where
  logMsg :: LogMsg -> m ()

instance (MonadLogger m) => MonadLogger (ReaderT r m) where
  logMsg = lift . logMsg

textLog :: Text -> LogMsg
textLog msg =
  LogMsg
    { severity = Info,
      callstack = Nothing,
      msg,
      tags = mempty
    }

showLog :: (Show a) => a -> LogMsg
showLog = textLog . tShow

withSeverity :: Severity -> LogMsg -> LogMsg
withSeverity newSeverity m = m {severity = newSeverity}

withCurrentCallstackIfUnset :: (HasCallStack) => LogMsg -> LogMsg
withCurrentCallstackIfUnset = withCallstackIfUnset callStack

withCallstackIfUnset :: CallStack -> LogMsg -> LogMsg
withCallstackIfUnset theCallstack m = m {callstack = callstack m <|> Just theCallstack}

withTag :: (Text, Text) -> LogMsg -> LogMsg
withTag (k, v) m = m {tags = Map.singleton k v <> tags m}

logDebugText :: (MonadLogger m) => Text -> m ()
logDebugText msg = logMsg . withSeverity Debug $ textLog msg

logInfoText :: (MonadLogger m) => Text -> m ()
logInfoText msg = logMsg . withSeverity Info $ textLog msg

logUserFaultText :: (MonadLogger m) => Text -> m ()
logUserFaultText msg = logMsg . withSeverity UserFault $ textLog msg

logErrorText :: (HasCallStack, MonadLogger m) => Text -> m ()
logErrorText msg = logMsg . withCurrentCallstackIfUnset . withSeverity Error $ textLog msg

-- | Formats a log message. Picks the formatter based on the environment.
logFmtFormatter :: FL.FormattedTime -> LogMsg -> FL.LogStr
logFmtFormatter =
  if Deployment.onLocal
    then localLogFmtLogger
    else deploymentLogFmtFormatter

-- | Formats a LogMsg in the popular logfmt format.
--
-- >>> deploymentLogFmtFormatter (LogMsg{severity=Error, timestamp="26/Oct/2022:21:04:38", callstack=Nothing, msg="Something Happened!", tags=Map.fromList [("userID", "123456")]})
-- "time=\"26/Oct/2022:21:04:38\" level=\"Error\" userID=\"123456\" msg=\"Something Happened!\""
deploymentLogFmtFormatter :: FL.FormattedTime -> LogMsg -> FL.LogStr
deploymentLogFmtFormatter timestamp (LogMsg {severity, callstack, msg, tags}) =
  toLogFmt $
    [ ("time", Text.decodeUtf8 timestamp),
      ("level", Text.pack . show $ severity),
      ("deployment", Text.pack . show $ deployment)
    ]
      <> Map.toList tags
      <> Monoid.whenM (not $ Text.null msg) [("msg", msg)]
      <> case callstack of
        Nothing -> []
        Just cs -> [("callstack", Text.pack $ prettyCallStack cs)]
  where
    keyify :: Text -> Text
    keyify = Text.map \case
      c
        | Char.isAlphaNum c || c `elem` ['_', '-'] -> c
        | otherwise -> '_'
    toLogFmt :: [(Text, Text)] -> FL.LogStr
    toLogFmt xs = intercalateMap " " FL.toLogStr do
      (k, v) <- xs
      -- Showing the text value will properly escape all contained quotes and wrap
      -- the resulting expression in quotes as is required by logfmt
      pure $ (keyify k) <> "=" <> (Text.pack $ show v)

-- | When we're on local we focus on readability of errors rather than the ability to ingest into a log
-- collector.
--
-- >>> localLogFmtLogger (LogMsg{severity=Error, timestamp="26/Oct/2022:21:04:38", callstack=Nothing, msg="Something Happened!", tags=Map.fromList [("userID", "123456")]})
localLogFmtLogger :: FL.FormattedTime -> LogMsg -> FL.LogStr
localLogFmtLogger _timestamp (LogMsg {severity, callstack, msg, tags}) =
  FL.toLogStr . Text.unlines $
    [ Text.pack . show $ severity,
      ( Map.toList (Map.delete "error-msg" tags) -- The error-msg is duplicated in the message, don't need to show both.
          <&> (\(k, v) -> k <> ": " <> v)
      )
        & Text.intercalate ", "
    ]
      <> Monoid.whenM (not $ Text.null msg) [msg]
      <> case callstack of
        Nothing -> []
        Just cs -> [Text.pack $ prettyCallStack cs]

ioLogger :: (MonadIO m) => IO.Handle -> (LogMsg -> Text) -> LogMsg -> m ()
ioLogger handle formatter msg = liftIO do
  Text.hPutStrLn handle (formatter msg)

-- Instances from unison

instance Loggable Sync.EntityValidationError where
  toLog = withSeverity Error . showLog
