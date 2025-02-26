module Share.BackgroundJobs (startWorkers) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync qualified as DefnSearch

-- | Kicks off all background workers.
startWorkers :: Ki.Scope -> Background ()
startWorkers scope = do
  DefnSearch.worker scope

-- Temporary disable background diff jobs until the new diffing logic is done.
-- ContributionDiffs.worker scope
-- SerializedEntitiesMigration.worker scope
