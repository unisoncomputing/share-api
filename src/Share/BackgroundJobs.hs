module Share.BackgroundJobs (startWorkers) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.ContributionDiffs qualified as ContributionDiffs
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync qualified as DefnSearch
import Share.BackgroundJobs.SerialisedEntitiesMigration.Worker qualified as SerialisedEntitiesMigration

-- | Kicks off all background workers.
startWorkers :: Ki.Scope -> Background ()
startWorkers scope = do
  DefnSearch.worker scope
  ContributionDiffs.worker scope
  SerialisedEntitiesMigration.worker scope
