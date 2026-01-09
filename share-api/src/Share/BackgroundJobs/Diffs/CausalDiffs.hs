module Share.BackgroundJobs.Diffs.CausalDiffs (worker) where

import Control.Lens
import Data.Map qualified as Map
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Diffs.Queries qualified as DQ
import Share.BackgroundJobs.Diffs.Types (CausalDiffInfo (..))
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase qualified as Codebase
import Share.Codebase.CodebaseRuntime qualified as CR
import Share.Env.Types qualified as Env
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.Notifications qualified as Notif
import Share.Prelude
import Share.Telemetry qualified as Trace
import Share.Utils.Logging qualified as Logging
import Share.Utils.Tags (MonadTags (..), Tags)
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (..))
import Share.Web.Share.Diffs.Impl qualified as Diffs
import System.Clock qualified as Clock
import UnliftIO qualified

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  unisonRuntime <- asks Env.sandboxedRuntime
  newWorker scope "causal-diffs" $ forever do
    Notif.waitOnChannel Notif.CausalDiffChannel (maxPollingIntervalSeconds * 1000000)
    processDiffs authZReceipt unisonRuntime

-- Process diffs until we run out of them. We claim a diff in a transaction and compute the diff in the same
-- transaction, with a row lock on the contribution id (which is skipped by other workers). There's therefore no chance
-- that we claim a diff but fail to write the result of computing that diff back to the database.
processDiffs :: AuthZ.AuthZReceipt -> CR.UnisonRuntime -> Background ()
processDiffs authZReceipt unisonRuntime = do
  processDiff authZReceipt unisonRuntime >>= \case
    True -> processDiffs authZReceipt unisonRuntime
    False -> pure ()

-- | Process a diff, then return whether or not we did any work.
processDiff :: AuthZ.AuthZReceipt -> CR.UnisonRuntime -> Background Bool
processDiff authZReceipt unisonRuntime = Trace.withSpan "background:causal-diffs:process-diff" mempty $ do
  pendingCausalDiffVar <- liftIO $ UnliftIO.newEmptyMVar
  result <- UnliftIO.tryAny $ PG.runTransactionMode PG.RepeatableRead PG.ReadWrite do
    DQ.claimCausalDiff >>= \case
      Nothing -> pure Nothing
      Just causalDiffInfo -> withTags (causalDiffTags causalDiffInfo) do
        PG.transactionUnsafeIO $ UnliftIO.tryPutMVar pendingCausalDiffVar causalDiffInfo
        startTime <- PG.transactionUnsafeIO (Clock.getTime Clock.Monotonic)
        result <- PG.catchTransaction (maybeComputeAndStoreCausalDiff authZReceipt unisonRuntime causalDiffInfo)
        DQ.deleteClaimedCausalDiff causalDiffInfo
        pure (Just (causalDiffInfo, startTime, result))
  case result of
    -- The transaction failed with an exception.
    -- One possible cause is an unknown builtin.
    -- We should report it and mark it as invalid so we don't keep retrying it.
    Left err -> do
      mCausalDiffInfo <- liftIO $ UnliftIO.tryTakeMVar pendingCausalDiffVar
      case mCausalDiffInfo of
        Nothing -> pure ()
        Just cdi -> withTags (causalDiffTags cdi) do
          reportError err
          PG.runTransaction $ DQ.markCausalDiffInvalid (tShow err) cdi
      -- Continue processing other diffs.
      pure True
    Right Nothing -> pure False
    Right (Just (cdi, startTime, result)) -> do
      let tags = causalDiffTags cdi
      withTags tags do
        case result of
          Left err -> reportError err
          Right didWork -> do
            when didWork do
              liftIO (Metrics.recordCausalDiffDuration startTime)
              Logging.textLog "Computed causal diff"
                & Logging.withSeverity Logging.Info
                & Logging.logMsg
      pure True
  where
    causalDiffTags :: CausalDiffInfo -> Tags
    causalDiffTags CausalDiffInfo {fromCausalId, toCausalId, fromCodebaseOwner, toCodebaseOwner} =
      Map.fromList $
        [ ("from-causal-id", IDs.toText fromCausalId),
          ("to-causal-id", IDs.toText toCausalId),
          ("from-codebase-owner", IDs.toText fromCodebaseOwner),
          ("to-codebase-owner", IDs.toText toCodebaseOwner)
        ]

-- Check whether a causal diff has already been computed, and if it hasn't, compute and store it. Otherwise, do nothing.
-- Returns whether or not we did any work.
maybeComputeAndStoreCausalDiff ::
  AuthZ.AuthZReceipt ->
  CR.UnisonRuntime ->
  CausalDiffInfo ->
  PG.Transaction EntityMissing Bool
maybeComputeAndStoreCausalDiff authZReceipt unisonRuntime (CausalDiffInfo {fromCausalId, toCausalId, fromCodebaseOwner, toCodebaseOwner}) = PG.transactionSpan "maybeComputeAndStoreCausalDiff" mempty $ do
  bestCommonAncestorCausalId <- CausalQ.bestCommonAncestor fromCausalId toCausalId
  let fromCodebase = Codebase.codebaseEnv authZReceipt $ Codebase.codebaseLocationForUserCodebase fromCodebaseOwner
  let toCodebase = Codebase.codebaseEnv authZReceipt $ Codebase.codebaseLocationForUserCodebase toCodebaseOwner
  ContributionsQ.existsPrecomputedNamespaceDiff (fromCodebase, fromCausalId) (toCodebase, toCausalId) >>= \case
    True -> pure False
    False -> CR.withCodebaseRuntime fromCodebase unisonRuntime \fromRuntime -> do
      CR.withCodebaseRuntime toCodebase unisonRuntime \toRuntime -> do
        _ <-
          Diffs.computeAndStoreCausalDiff
            authZReceipt
            (fromCodebase, fromRuntime, fromCausalId)
            (toCodebase, toRuntime, toCausalId)
            -- This is a little strange. At the moment, if we don't have a proper LCA we
            -- revert to a two-way diff. The other option would be to diff both from an empty
            -- branch, but that's not typically what the user meant.
            -- If this assumption no longer holds true in the future, feel free to change it
            -- to be more explicit about which diff we're calculating.
            (Just $ fromMaybe fromCausalId bestCommonAncestorCausalId)
        pure True
