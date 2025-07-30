module Share.BackgroundJobs.Diffs.Queries
  ( submitContributionsToBeDiffed,
    submitCausalsToBeDiffed,
    claimCausalDiff,
    deleteClaimedCausalDiff,
  )
where

import Share.BackgroundJobs.Diffs.Types
import Share.Codebase (CodebaseEnv)
import Share.Codebase.Types (CodebaseEnv (..))
import Share.IDs
import Share.Postgres
import Share.Postgres.IDs (CausalId)
import Share.Postgres.Notifications qualified as Notif
import Unison.Prelude

-- | Enqueue any arbitrary causal IDs to be diffed.
submitCausalsToBeDiffed :: (QueryM m) => (CodebaseEnv, CausalId) -> (CodebaseEnv, CausalId) -> m ()
submitCausalsToBeDiffed (CodebaseEnv {codebaseOwner = fromCodebase}, fromCausalId) (CodebaseEnv {codebaseOwner = toCodebase}, toCausalId) = do
  execute_
    [sql|
      INSERT INTO causal_diff_queue (from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner)
        SELECT #{fromCausalId}, #{toCausalId}, #{fromCodebase}, #{toCodebase}
        FROM diff_info
          WHERE NOT EXISTS (
            SELECT FROM namespace_diffs nd
              WHERE nd.left_causal_id = diff_info.from_causal_id
              AND nd.right_causal_id = diff_info.to_causal_id
              AND nd.left_codebase_owner_user_id = diff_info.from_codebase_owner
              AND nd.right_codebase_owner_user_id = diff_info.to_codebase_owner
          )
        ON CONFLICT DO NOTHING
    |]
  Notif.notifyChannel Notif.CausalDiffChannel

-- | Enqueue a set of contributions to be diffed.
submitContributionsToBeDiffed :: (QueryM m) => Set ContributionId -> m ()
submitContributionsToBeDiffed contributions = do
  execute_
    [sql|
      WITH new_contributions(contribution_id) AS (
        SELECT * FROM ^{singleColumnTable (toList contributions)}
      ), diff_info(from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner) AS (
        SELECT c.target_causal_id, c.source_causal_id, COALESCE(target_branch.contributor_id, target_project.owner_user_id), COALESCE(source_branch.contributor_id, source_project.owner_user_id)
          FROM new_contributions nc
          JOIN contributions c ON c.id = nc.contribution_id
          JOIN project_branches source_branch ON source_branch.id = c.source_branch
          JOIN project_branches target_branch ON target_branch.id = c.target_branch
          JOIN projects source_project ON source_project.id = source_branch.project_id
          JOIN projects target_project ON target_project.id = target_branch.project_id
      )
      INSERT INTO causal_diff_queue (from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner)
        SELECT from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner
        FROM diff_info
          WHERE NOT EXISTS (
            SELECT FROM namespace_diffs nd
              WHERE nd.left_causal_id = diff_info.from_causal_id
              AND nd.right_causal_id = diff_info.to_causal_id
              AND nd.left_codebase_owner_user_id = diff_info.from_codebase_owner
              AND nd.right_codebase_owner_user_id = diff_info.to_codebase_owner
          )
        ON CONFLICT DO NOTHING
    |]
  Notif.notifyChannel Notif.CausalDiffChannel

-- | Claim the oldest contribution in the queue to be diffed.
claimCausalDiff :: Transaction e (Maybe CausalDiffInfo)
claimCausalDiff = do
  query1Row
    [sql|
      SELECT from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner
      FROM causal_diff_queue
      ORDER BY created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    |]

deleteClaimedCausalDiff :: CausalDiffInfo -> Transaction e ()
deleteClaimedCausalDiff CausalDiffInfo {fromCausalId, toCausalId, fromCodebaseOwner, toCodebaseOwner} =
  execute_
    [sql|
      DELETE FROM causal_diff_queue
        WHERE causal_diff_queue.from_causal_id = #{fromCausalId}
          AND causal_diff_queue.to_causal_id = #{toCausalId}
          AND causal_diff_queue.from_codebase_owner = #{fromCodebaseOwner}
          AND causal_diff_queue.to_codebase_owner = #{toCodebaseOwner}
    |]
