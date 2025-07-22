module Share.Web.Share.Contributions.MergeDetection
  ( updateContributionsFromBranchUpdate,
  )
where

import Data.Set qualified as Set
import Share.BackgroundJobs.Diffs.Queries qualified as DiffsQ
import Share.IDs
import Share.Postgres (QueryM)
import Share.Postgres.Contributions.Ops qualified as ContribOps
import Share.Postgres.Contributions.Queries qualified as ContributionQ

-- | Update contribution status to reflect any merges which may have occurred and update the
-- best common ancestors.
updateContributionsFromBranchUpdate :: (QueryM m) => UserId -> BranchId -> m ()
updateContributionsFromBranchUpdate callerUserId branchId = do
  contributionsWithUpdatedBCAs <- ContribOps.performMergesAndBCAUpdatesFromBranchPush callerUserId branchId
  rebasedContributions <- ContributionQ.rebaseContributionsFromMergedBranches contributionsWithUpdatedBCAs
  affectedContributions <- ContributionQ.contributionsRelatedToBranches (Set.singleton branchId)
  DiffsQ.submitContributionsToBeDiffed (Set.fromList affectedContributions <> rebasedContributions)
  pure ()
