module Share.BackgroundJobs (startWorkers) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.CausalDiffs qualified as ContributionDiffs
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync qualified as DefnSearch
import Share.BackgroundJobs.Webhooks.Worker qualified as Webhooks

-- | Kicks off all background workers.
startWorkers :: Ki.Scope -> Background ()
startWorkers scope = do
  DefnSearch.worker scope
  ContributionDiffs.worker scope
  Webhooks.worker scope

-- Temporary disable background diff jobs until the new diffing logic is done.
-- SerializedEntitiesMigration.worker scope
