module Share.BackgroundJobs.Diffs.Queries
  ( submitContributionsToBeDiffed,
    claimContributionToDiff,
    updateRetries,
  )
where

import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Set (Set)
import Share.IDs
import Share.Postgres

updateRetries :: ContributionId -> Int32 -> Transaction e ()
updateRetries contributionId retries = do
  execute_
    [sql|
    UPDATE contribution_diff_queue
    SET retries_left = #{retries}
    WHERE contribution_id = #{contributionId}
    |]

submitContributionsToBeDiffed :: (QueryM m) => Set ContributionId -> Int32 -> m ()
submitContributionsToBeDiffed contributions retries = do
  execute_
    [sql|
    WITH new_contributions(contribution_id) AS (
      SELECT * FROM ^{singleColumnTable (toList contributions)}
    )
    INSERT INTO contribution_diff_queue (contribution_id, retries_left)
      SELECT nc.contribution_id, #{retries}
      FROM new_contributions nc
      ON CONFLICT DO NOTHING
    |]

-- | Claim the oldest contribution in the queue to be diffed.
claimContributionToDiff :: Transaction e (Maybe (ContributionId, Int32))
claimContributionToDiff = do
  query1Row
    [sql|
    WITH chosen_contribution(contribution_id, retries_left) AS (
      SELECT q.contribution_id, c.retries_left
      FROM contribution_diff_queue q
      WHERE q.retries_left > 0
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM contribution_diff_queue
      USING chosen_contribution
      WHERE contribution_diff_queue.contribution_id = chosen_contribution.contribution_id
    RETURNING chosen_contribution.contribution_id, chosen_contribution.retries_left
    |]
