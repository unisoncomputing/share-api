{-# LANGUAGE DataKinds #-}

-- | Background worker monad
module Share.BackgroundJobs.Monad
  ( Background,
    BackgroundCtx (..),
    withWorkerName,
    runBackground,
  )
where

import Share.App
import Share.Env
import Share.Prelude
import Share.Utils.Tags (HasTags (..))

data BackgroundCtx = BackgroundCtx
  { workerName :: Text,
    loggingTags :: Map Text Text
  }
  deriving (Generic)

type Background = AppM BackgroundCtx

instance HasTags BackgroundCtx where
  getTags ctx = pure $ loggingTags ctx
  addTags tags ctx = ctx {loggingTags = loggingTags ctx <> tags}

localBackgroundCtx :: (MonadReader (Env BackgroundCtx) m) => (BackgroundCtx -> BackgroundCtx) -> m a -> m a
localBackgroundCtx f = local \env -> env {ctx = f (ctx env)}

withWorkerName :: Text -> Background a -> Background a
withWorkerName name = localBackgroundCtx \ctx -> ctx {workerName = name}

runBackground :: Env () -> Text -> Background a -> IO a
runBackground env workerName bg =
  runAppM env {ctx = BackgroundCtx {workerName, loggingTags = mempty}} bg
