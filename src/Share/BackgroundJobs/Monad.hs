-- | Background worker monad
module Share.BackgroundJobs.Monad
  ( Background,
    BackgroundCtx (..),
    withWorkerName,
    runBackground,
    withTag,
    withTags,
  )
where

import Data.Map qualified as Map
import Share.App
import Share.Env
import Share.Env qualified as Env
import Share.Prelude
import Share.Utils.Logging qualified as Logging

data BackgroundCtx = BackgroundCtx
  { workerName :: Text,
    loggingTags :: Map Text Text
  }

type Background = AppM BackgroundCtx

localBackgroundCtx :: (MonadReader (Env BackgroundCtx) m) => (BackgroundCtx -> BackgroundCtx) -> m a -> m a
localBackgroundCtx f = local \env -> env {ctx = f (ctx env)}

withWorkerName :: Text -> Background a -> Background a
withWorkerName name = localBackgroundCtx \ctx -> ctx {workerName = name}

withTag :: Text -> Text -> Background a -> Background a
withTag key value = withTags [(key, value)]

withTags :: [(Text, Text)] -> Background a -> Background a
withTags tags = localBackgroundCtx \ctx -> ctx {loggingTags = Map.union (Map.fromList tags) (loggingTags ctx)}

instance Logging.MonadLogger Background where
  logMsg msg = do
    log <- asks Env.logger
    BackgroundCtx {workerName, loggingTags} <- asks ctx
    let currentTags = Map.singleton "workerName" workerName <> loggingTags
    msg <- pure $ msg {Logging.tags = Logging.tags msg `Map.union` currentTags}
    minSeverity <- asks Env.minLogSeverity
    when (Logging.severity msg >= minSeverity) $ do
      timestamp <- asks timeCache >>= liftIO
      liftIO . log . Logging.logFmtFormatter timestamp $ msg

runBackground :: Env () -> Text -> Background a -> IO a
runBackground env workerName bg =
  runAppM env {ctx = BackgroundCtx {workerName, loggingTags = mempty}} bg
