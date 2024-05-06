-- | Background worker monad
module Share.BackgroundJobs.Monad
  ( Background,
    BackgroundCtx (..),
    withWorkerName,
  )
where

import Share.App
import Share.Env
import Share.Prelude

data BackgroundCtx = BackgroundCtx
  { workerName :: Text
  }

type Background = AppM BackgroundCtx

localBackgroundCtx :: (MonadReader (Env BackgroundCtx) m) => (BackgroundCtx -> BackgroundCtx) -> m a -> m a
localBackgroundCtx f = local \env -> env {ctx = f (ctx env)}

withWorkerName :: Text -> Background a -> Background a
withWorkerName name = localBackgroundCtx \ctx -> ctx {workerName = name}
