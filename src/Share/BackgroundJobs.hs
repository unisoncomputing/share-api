module Share.BackgroundJobs (startWorkers) where

import Data.Void (Void)
import Ki.Unlifted qualified as Ki
import Share.App
import Share.BackgroundJobs.Monad (Background)
import Share.Prelude

-- | Kicks off all background workers.
startWorkers :: Ki.Scope -> Background ()
startWorkers scope = do
  _
