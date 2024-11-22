module Share.BackgroundJobs.Diffs.ContributionDiffs (worker) where

import Control.Lens
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.Queries qualified as DQ
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Branch (Branch (..))
import Share.Codebase qualified as Codebase
import Share.Contribution (Contribution (..))
import Share.IDs
import Share.Metrics qualified as Metrics
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Project (Project (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..), ErrorID (..))
import Share.Web.Share.Diffs.Impl qualified as Diffs
import UnliftIO.Concurrent qualified as UnliftIO

pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "diffs:contributions" $ forever do
    processDiffs authZReceipt
    liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000

processDiffs :: AuthZ.AuthZReceipt -> Background ()
processDiffs authZReceipt = do
  mayContributionId <- Metrics.recordContributionDiffDuration $ PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
    mayContributionId <- DQ.claimContributionToDiff
    for_ mayContributionId (diffContribution authZReceipt)
    pure mayContributionId
  case mayContributionId of
    Just contributionId -> do
      Logging.textLog ("Recomputed contribution diff: " <> tShow contributionId)
        & Logging.withTag ("contribution-id", tShow contributionId)
        & Logging.withSeverity Logging.Info
        & Logging.logMsg
      -- Keep processing releases until we run out of them.
      processDiffs authZReceipt
    Nothing -> pure ()

type DiffContributionError = EntityMissing

diffContribution :: AuthZ.AuthZReceipt -> ContributionId -> WebApp ()
diffContribution authZReceipt contributionId = do
  Contribution {sourceBranchId = newBranchId, targetBranchId = oldBranchId, projectId} <- ContributionsQ.contributionById contributionId `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
  project <- Q.projectById projectId `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  newBranch@Branch {causal = newBranchCausalId, branchId = newBranchId} <- Q.branchById newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
  oldBranch@Branch {causal = oldBranchCausalId, branchId = oldBranchId} <- Q.branchById oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  _ <- Diffs.diffCausals authZReceipt (oldCodebase, oldBranchCausalId) (newCodebase, newBranchCausalId)
  pure ()
