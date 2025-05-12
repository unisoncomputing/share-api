module Share.BackgroundJobs.Diffs.Queries
  ( submitContributionsToBeDiffed,
    claimCausalDiff,
  )
where

import Share.BackgroundJobs.Diffs.Types
import Share.IDs
import Share.Postgres
import Share.Postgres.Notifications qualified as Notif
import Unison.Prelude

submitContributionsToBeDiffed :: (QueryM m) => Set ContributionId -> m ()
submitContributionsToBeDiffed contributions = do
  execute_
    [sql|
      WITH new_contributions(contribution_id) AS (
        SELECT * FROM ^{singleColumnTable (toList contributions)}
      )
      INSERT INTO causal_diff_queue (from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner)
        SELECT c.target_causal_id, c.source_causal_id, COALESCE(target_branch.contributor_id, target_project.owner_user_id), COALESCE(source_branch.contributor_id, source_project.owner_user_id)
          FROM new_contributions nc
          JOIN contributions c ON c.id = nc.contribution_id
          JOIN project_branches source_branch ON source_branch.id = c.source_branch
          JOIN project_branches target_branch ON target_branch.id = c.target_branch
          JOIN projects source_project ON source_project.id = source_branch.project_id
          JOIN projects target_project ON target_project.id = target_branch.project_id
        ON CONFLICT DO NOTHING
    |]
  Notif.notifyChannel Notif.CausalDiffChannel

-- | Claim the oldest contribution in the queue to be diffed.
claimCausalDiff :: Transaction e (Maybe CausalDiffInfo)
claimCausalDiff = do
  query1Row
    [sql|
      WITH chosen(from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner) AS (
        SELECT q.from_causal_id, q.to_causal_id, q.from_codebase_owner, q.to_codebase_owner
        FROM causal_diff_queue q
        ORDER BY q.created_at ASC
        LIMIT 1
        -- Skip any that are being synced by other workers.
        FOR UPDATE SKIP LOCKED
      )
      DELETE FROM causal_diff_queue
        USING chosen
        WHERE causal_diff_queue.from_causal_id = chosen.from_causal_id
          AND causal_diff_queue.to_causal_id = chosen.to_causal_id
          AND causal_diff_queue.from_codebase_owner = chosen.from_codebase_owner
          AND causal_diff_queue.to_codebase_owner = chosen.to_codebase_owner
      RETURNING chosen.from_causal_id, chosen.to_causal_id, chosen.from_codebase_owner, chosen.to_codebase_owner
    |]
