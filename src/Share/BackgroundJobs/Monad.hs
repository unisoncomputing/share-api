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
import Share.Prelude

data BackgroundCtx = BackgroundCtx
  { workerName :: Text,
    loggingTags :: Map Text Text
  }

instance HasTags BackgroundCtx where
  getTags BackgroundCtx {loggingTags, workerName} = pure $ Map.singleton "workerName" workerName <> loggingTags
  updateTags f BackgroundCtx {loggingTags, workerName} = do
    let newTags = f loggingTags
    pure $ BackgroundCtx {workerName, loggingTags = newTags}

type Background = AppM BackgroundCtx

localBackgroundCtx :: (MonadReader (Env BackgroundCtx) m) => (BackgroundCtx -> BackgroundCtx) -> m a -> m a
localBackgroundCtx f = local \env -> env {ctx = f (ctx env)}

withWorkerName :: Text -> Background a -> Background a
withWorkerName name = localBackgroundCtx \ctx -> ctx {workerName = name}

withTag :: Text -> Text -> Background a -> Background a
withTag key value = withTags [(key, value)]

withTags :: [(Text, Text)] -> Background a -> Background a
withTags tags = localBackgroundCtx \ctx -> ctx {loggingTags = Map.union (Map.fromList tags) (loggingTags ctx)}

runBackground :: Env () -> Text -> Background a -> IO a
runBackground env workerName bg =
  runAppM env {ctx = BackgroundCtx {workerName, loggingTags = mempty}} bg
