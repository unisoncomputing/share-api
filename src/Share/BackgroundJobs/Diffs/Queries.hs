module Share.BackgroundJobs.Diffs.Queries
  ( submitContributionToBeDiffed,
    claimContributionToDiff,
  )
where

import Share.IDs
import Share.Postgres

submitContributionToBeDiffed :: (QueryM m) => ContributionId -> m ()
submitContributionToBeDiffed contributionId = do
  execute_
    [sql|
    INSERT INTO contribution_diff_queue (contribution_id)
    VALUES (#{contributionId})
    |]

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
