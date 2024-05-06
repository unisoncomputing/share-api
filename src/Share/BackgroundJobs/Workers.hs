module Share.BackgroundJobs.Workers (newWorker) where

import Data.HashMap.Lazy qualified as HM
import Data.Map qualified as Map
import Ki.Unlifted qualified as Ki
import Share.App
import Share.BackgroundJobs.Monad (Background, BackgroundCtx (..), withWorkerName, workerName)
import Share.Env qualified as Env
import Share.Monitoring qualified as Monitoring
import Share.Prelude
import Share.Web.Errors (InternalServerError (..))
import UnliftIO qualified

-- | Creates a worker that runs forever.
-- Any exceptions will be caught and logged, then the worker will be restarted.
newWorker :: Ki.Scope -> Text -> Background Void -> Background ()
newWorker scope workerName worker = withWorkerName workerName do
  Ki.fork_ scope do
    -- Log any async exceptions, but don't try to catch them.
    flip UnliftIO.withException logException . forever $ do
      UnliftIO.tryAny worker >>= \case
        Left e -> logException e
        Right void -> absurd void
  where
    logException :: UnliftIO.SomeException -> Background ()
    logException e = do
      env <- ask
      let coreTags = HM.fromList [("workerName", into @String workerName)]
      let extraTags = HM.fromList []
      let errID = "background-job:" <> workerName
      Monitoring.reportError env coreTags extraTags errID (InternalServerError e)
