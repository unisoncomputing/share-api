module Share.BackgroundJobs.Diffs.ContributionDiffs (worker) where

import Control.Lens
import Control.Monad.Except
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.Queries qualified as DQ
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background, withTag)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Branch (Branch (..))
import Share.Codebase qualified as Codebase
import Share.Contribution (Contribution (..))
import Share.IDs
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.NamespaceDiffs (NamespaceDiffError (MissingEntityError))
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.Notifications qualified as Notif
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..), ErrorID (..))
import Share.Web.Share.Diffs.Impl qualified as Diffs

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "diffs:contributions" $ forever do
    Notif.waitOnChannel Notif.ContributionDiffChannel (maxPollingIntervalSeconds * 1000000)
    processDiffs authZReceipt >>= \case
      Left (contributionId, e) ->
        withTag "contribution-id" (IDs.toText contributionId) $ do
          reportError e
      Right _ -> pure ()

processDiffs :: AuthZ.AuthZReceipt -> Background (Either (ContributionId, NamespaceDiffError) ())
processDiffs authZReceipt = Metrics.recordContributionDiffDuration . runExceptT $ do
  mayContributionId <- PG.runTransaction DQ.claimContributionToDiff
  for_ mayContributionId (diffContribution authZReceipt)
  case mayContributionId of
    Just contributionId -> do
      Logging.textLog ("Recomputed contribution diff: " <> tShow contributionId)
        & Logging.withTag ("contribution-id", tShow contributionId)
        & Logging.withSeverity Logging.Info
        & Logging.logMsg
      -- Keep processing releases until we run out of them.
      either throwError pure =<< lift (processDiffs authZReceipt)
    Nothing -> pure ()

diffContribution :: AuthZ.AuthZReceipt -> ContributionId -> ExceptT (ContributionId, NamespaceDiffError) Background ()
diffContribution authZReceipt contributionId = withExceptT (contributionId,) . mapExceptT (withTag "contribution-id" (IDs.toText contributionId)) $ do
  ( bestCommonAncestorCausalId,
    project,
    newBranch@Branch {causal = newBranchCausalId},
    oldBranch@Branch {causal = oldBranchCausalId}
    ) <- ExceptT $ PG.tryRunTransaction $ do
    Contribution {bestCommonAncestorCausalId, sourceBranchId = newBranchId, targetBranchId = oldBranchId, projectId} <- ContributionsQ.contributionById contributionId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    project <- Q.projectById projectId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "project:missing") "Project not found")
    newBranch <- Q.branchById newBranchId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "branch:missing") "Source branch not found")
    oldBranch <- Q.branchById oldBranchId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "branch:missing") "Target branch not found")
    pure (bestCommonAncestorCausalId, project, newBranch, oldBranch)
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  -- This method saves the diff so it'll be there when we need it, so we don't need to do anything with it.
  _ <-
    Diffs.diffCausals authZReceipt (oldCodebase, oldBranchCausalId) (newCodebase, newBranchCausalId) bestCommonAncestorCausalId
  pure ()
