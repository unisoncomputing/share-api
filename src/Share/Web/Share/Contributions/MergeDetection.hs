module Share.Web.Share.Contributions.MergeDetection
  ( updateContributionsFromBranchUpdate,
  )
where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionQ

-- | Update contribution status to reflect any merges which may have occurred and update the
-- best common ancestors.
updateContributionsFromBranchUpdate :: UserId -> BranchId -> PG.Transaction e ()
updateContributionsFromBranchUpdate callerUserId branchId = do
  updatedContributions <- ContributionQ.performMergesAndBCAUpdatesFromBranchPush callerUserId branchId
  _rebasedContributions <- ContributionQ.rebaseContributionsFromMergedBranches updatedContributions
  pure ()
