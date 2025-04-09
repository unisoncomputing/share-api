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
import Share.Env qualified as Env
import Share.IDs
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.NamespaceDiffs (NamespaceDiffError (MissingEntityError))
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.Notifications qualified as Notif
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Project (Project)
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..), ErrorID (..))
import Share.Web.Share.Diffs.Impl qualified as Diffs
import System.Clock qualified as Clock

-- | Check every 30 seconds if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 30

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  badUnliftCodebaseRuntime <- Codebase.badAskUnliftCodebaseRuntime
  unisonRuntime <- asks Env.sandboxedRuntime
  let makeRuntime :: Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)
      makeRuntime codebase = do
        runtime <- Codebase.codebaseRuntime' unisonRuntime codebase
        pure (badUnliftCodebaseRuntime runtime)
  newWorker scope "diffs:contributions" $ forever do
    Notif.waitOnChannel Notif.ContributionDiffChannel (maxPollingIntervalSeconds * 1000000)
    processDiffs authZReceipt makeRuntime

-- Process diffs until we run out of them. We claim a diff in a transaction, commit it, then proceed to compute the
-- diff. There's therefore a chance we claim a diff and fail to compute it (due to e.g. server restart). The current
-- solution to these "at most once" semantics is to simply re-enqueue a diff job if necessary; e.g. in the view diff
-- endpoint handler.
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
                result <- PG.catchTransaction (diffContribution authZReceipt makeRuntime contributionId)
                pure (Just (contributionId, startTime, result))
        case result of
          Nothing -> pure ()
          Just (contributionId, startTime, result) -> do
            withTag "contribution-id" (IDs.toText contributionId) do
              case result of
                Left err -> reportError err
                Right () -> do
                  liftIO (Metrics.recordContributionDiffDuration startTime)
                  Logging.textLog "Computed contribution diff"
                    & Logging.withSeverity Logging.Info
                    & Logging.logMsg
            loop
  loop

diffContribution ::
  AuthZ.AuthZReceipt ->
  (Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)) ->
  ContributionId ->
  PG.Transaction NamespaceDiffError ()
diffContribution authZReceipt makeRuntime contributionId = do
  (bestCommonAncestorCausalId, project, newBranch, oldBranch) <- getContributionInfo
  let Branch {causal = oldBranchCausalId} = oldBranch
  let Branch {causal = newBranchCausalId} = newBranch
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  oldRuntime <- PG.transactionUnsafeIO (makeRuntime oldCodebase)
  newRuntime <- PG.transactionUnsafeIO (makeRuntime newCodebase)
  _ <-
    Diffs.diffCausals
      authZReceipt
      (oldCodebase, oldRuntime, oldBranchCausalId)
      (newCodebase, newRuntime, newBranchCausalId)
      bestCommonAncestorCausalId
  pure ()
  where
    getContributionInfo :: PG.Transaction NamespaceDiffError (Maybe CausalId, Project, Branch CausalId, Branch CausalId)
    getContributionInfo = do
      Contribution {bestCommonAncestorCausalId, sourceBranchId = newBranchId, targetBranchId = oldBranchId, projectId} <-
        ContributionsQ.contributionById contributionId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "contribution:missing") "Contribution not found")
      project <- Q.projectById projectId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "project:missing") "Project not found")
      newBranch <- Q.branchById newBranchId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "branch:missing") "Source branch not found")
      oldBranch <- Q.branchById oldBranchId `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "branch:missing") "Target branch not found")
      pure (bestCommonAncestorCausalId, project, newBranch, oldBranch)
