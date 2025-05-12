module Share.BackgroundJobs.Diffs.CausalDiffs (worker) where

import Control.Lens
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.Queries qualified as DQ
import Share.BackgroundJobs.Diffs.Types (CausalDiffInfo (..))
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background, withTags)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase qualified as Codebase
import Share.Env qualified as Env
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.Notifications qualified as Notif
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..))
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
  newWorker scope "causal-diffs" $ forever do
    Notif.waitOnChannel Notif.CausalDiffChannel (maxPollingIntervalSeconds * 1000000)
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
            DQ.claimCausalDiff >>= \case
              Nothing -> pure Nothing
              Just causalDiffInfo -> do
                startTime <- PG.transactionUnsafeIO (Clock.getTime Clock.Monotonic)
                result <- PG.catchTransaction (maybeComputeAndStoreCausalDiff authZReceipt makeRuntime causalDiffInfo)
                pure (Just (causalDiffInfo, startTime, result))
        whenJust result \(CausalDiffInfo {fromCausalId, toCausalId, fromCodebaseOwner, toCodebaseOwner}, startTime, result) -> do
          let tags =
                [ ("from-causal-id", IDs.toText fromCausalId),
                  ("to-causal-id", IDs.toText toCausalId),
                  ("from-codebase-owner", IDs.toText fromCodebaseOwner),
                  ("to-codebase-owner", IDs.toText toCodebaseOwner)
                ]
          withTags tags do
            case result of
              Left err -> reportError err
              Right didWork -> do
                when didWork do
                  liftIO (Metrics.recordCausalDiffDuration startTime)
                  Logging.textLog "Computed causal diff"
                    & Logging.withSeverity Logging.Info
                    & Logging.logMsg
          loop
  loop

-- Check whether a causal diff has already been computed, and if it hasn't, compute and store it. Otherwise, do nothing.
-- Returns whether or not we did any work.
maybeComputeAndStoreCausalDiff ::
  AuthZ.AuthZReceipt ->
  (Codebase.CodebaseEnv -> IO (Codebase.CodebaseRuntime IO)) ->
  CausalDiffInfo ->
  PG.Transaction EntityMissing Bool
maybeComputeAndStoreCausalDiff authZReceipt makeRuntime (CausalDiffInfo {fromCausalId, toCausalId, fromCodebaseOwner, toCodebaseOwner}) = do
  bestCommonAncestorCausalId <- CausalQ.bestCommonAncestor fromCausalId toCausalId
  let fromCodebase = Codebase.codebaseEnv authZReceipt $ Codebase.codebaseLocationForUserCodebase fromCodebaseOwner
  let toCodebase = Codebase.codebaseEnv authZReceipt $ Codebase.codebaseLocationForUserCodebase toCodebaseOwner
  ContributionsQ.existsPrecomputedNamespaceDiff (fromCodebase, fromCausalId) (toCodebase, toCausalId) >>= \case
    True -> pure False
    False -> do
      fromRuntime <- PG.transactionUnsafeIO (makeRuntime fromCodebase)
      toRuntime <- PG.transactionUnsafeIO (makeRuntime toCodebase)
      _ <-
        Diffs.computeAndStoreCausalDiff
          authZReceipt
          (fromCodebase, fromRuntime, fromCausalId)
          (toCodebase, toRuntime, toCausalId)
          bestCommonAncestorCausalId
      pure True
