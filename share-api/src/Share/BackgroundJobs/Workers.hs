module Share.BackgroundJobs.Workers (newWorker) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Errors (reportException)
import Share.BackgroundJobs.Monad (Background, withWorkerName)
import Share.Prelude

-- | Creates a worker that runs forever.
-- Any exceptions will be caught and logged, then the worker will be restarted.
newWorker :: Ki.Scope -> Text -> Background Void -> Background ()
newWorker scope workerName worker = withWorkerName workerName do
  Ki.fork_ scope do
    -- Run the worker forever, catching and logging any syncronous exceptions, but then restarting.
    forever $ reportException worker
