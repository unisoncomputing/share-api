module Share.BackgroundJobs.Diffs.ContributionDiffs (worker) where

import Control.Lens
import Control.Monad.Except
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.Queries qualified as DQ
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background, withTag)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Branch (branchCausals_)
import Share.Codebase qualified as Codebase
import Share.Contribution (Contribution (..))
import Share.Env qualified as Env
import Share.IDs
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.Notifications qualified as Notif
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..), ErrorID (..))
import Share.Web.Share.Diffs.Impl qualified as Diffs
import System.Clock qualified as Clock

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  badUnliftCodebaseRuntime <- Codebase.badAskUnliftCodebaseRuntime
  unisonRuntime <- asks Env.sandboxedRuntime
  let makeRuntime :: Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)
      makeRuntime codebase = do
        runtime <- Codebase.codebaseRuntimeTransaction unisonRuntime codebase
        pure (badUnliftCodebaseRuntime runtime)
  newWorker scope "diffs:contributions" $ forever do
    Notif.waitOnChannel Notif.ContributionDiffChannel (maxPollingIntervalSeconds * 1000000)
    processDiffs authZReceipt makeRuntime

-- Process diffs until we run out of them. We claim a diff in a transaction and compute the diff in the same
-- transaction, with a row lock on the contribution id (which is skipped by other workers). There's therefore no chance
-- that we claim a diff but fail to write the result of computing that diff back to the database.
processDiffs :: AuthZ.AuthZReceipt -> (Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)) -> Background ()
processDiffs authZReceipt makeRuntime = do
  let loop :: Background ()
      loop = do
        result <-
          PG.runTransactionMode PG.RepeatableRead PG.ReadWrite do
            DQ.claimContributionToDiff >>= \case
              Nothing -> pure Nothing
              Just contributionId -> do
                startTime <- PG.transactionUnsafeIO (Clock.getTime Clock.Monotonic)
                result <- PG.catchTransaction (maybeComputeAndStoreCausalDiff authZReceipt makeRuntime contributionId)
                pure (Just (contributionId, startTime, result))
        whenJust result \(contributionId, startTime, result) -> do
          withTag "contribution-id" (IDs.toText contributionId) do
            case result of
              Left err -> reportError err
              Right didWork -> do
                when didWork do
                  liftIO (Metrics.recordContributionDiffDuration startTime)
                  Logging.textLog "Computed contribution diff"
                    & Logging.withSeverity Logging.Info
                    & Logging.logMsg
          loop
  loop

-- Check whether a causal diff has already been computed, and if it hasn't, compute and store it. Otherwise, do nothing.
-- Returns whether or not we did any work.
maybeComputeAndStoreCausalDiff ::
  AuthZ.AuthZReceipt ->
  (Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)) ->
  ContributionId ->
  PG.Transaction EntityMissing Bool
maybeComputeAndStoreCausalDiff authZReceipt makeRuntime contributionId = do
  Contribution {bestCommonAncestorCausalId, sourceBranchId = newBranchId, targetBranchId = oldBranchId, projectId} <-
    ContributionsQ.contributionById contributionId
  project <- Q.projectById projectId `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  newBranch <- Q.branchById newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
  oldBranch <- Q.branchById oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  let oldCausal = oldBranch ^. branchCausals_
  let newCausal = newBranch ^. branchCausals_
  ContributionsQ.existsPrecomputedNamespaceDiff (oldCodebase, oldCausal) (newCodebase, newCausal) >>= \case
    True -> pure False
    False -> do
      oldRuntime <- PG.transactionUnsafeIO (makeRuntime oldCodebase)
      newRuntime <- PG.transactionUnsafeIO (makeRuntime newCodebase)
      _ <-
        Diffs.computeAndStoreCausalDiff
          authZReceipt
          (oldCodebase, oldRuntime, oldCausal)
          (newCodebase, newRuntime, newCausal)
          bestCommonAncestorCausalId
      pure True
