module Share.BackgroundJobs.Diffs.Queries
  ( submitContributionsToBeDiffed,
    claimContributionToDiff,
  )
where

import Data.Set (Set)
import Share.IDs
import Share.Postgres

submitContributionsToBeDiffed :: (QueryM m) => Set ContributionId -> m ()
submitContributionsToBeDiffed _contributions = do
  -- TODO: need to revive this.
  pure ()

-- execute_
--   [sql|
--   WITH new_contributions(contribution_id) AS (
--     SELECT * FROM ^{singleColumnTable (toList contributions)}
--   )
--   INSERT INTO contribution_diff_queue (contribution_id)
--     SELECT nc.contribution_id FROM new_contributions nc
--     ON CONFLICT DO NOTHING
--   |]
-- Notif.notifyChannel Notif.ContributionDiffChannel

-- | Claim the oldest contribution in the queue to be diffed.
claimContributionToDiff :: Transaction e (Maybe ContributionId)
claimContributionToDiff = do
  query1Col
    [sql|
    WITH chosen_contribution(contribution_id) AS (
      SELECT q.contribution_id
      FROM contribution_diff_queue q
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM contribution_diff_queue
      USING chosen_contribution
      WHERE contribution_diff_queue.contribution_id = chosen_contribution.contribution_id
    RETURNING chosen_contribution.contribution_id
    |]
