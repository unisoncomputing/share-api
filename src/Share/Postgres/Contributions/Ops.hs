{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Contributions.Ops
  ( createContribution,
    updateContribution,
    performMergesAndBCAUpdatesFromBranchPush,
  )
where

import Data.Set qualified as Set
import Share.Contribution (Contribution (..), ContributionStatus (..))
import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types (ContributionData (..), NotificationEvent (..), NotificationEventData (..))
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContribQ
import Share.Postgres.Projects.Queries qualified as ProjectQ
import Share.Postgres.Projects.Queries qualified as ProjectsQ
import Share.Prelude
import Share.Utils.API (NullableUpdate (..), fromNullableUpdate)

createContribution ::
  -- | Author
  UserId ->
  ProjectId ->
  -- | Title
  Text ->
  -- | Description
  Maybe Text ->
  ContributionStatus ->
  -- | Source Branch
  BranchId ->
  -- | Target Branch
  BranchId ->
  PG.Transaction e (ContributionId, ContributionNumber)
createContribution authorId projectId title description status sourceBranchId targetBranchId = do
  (contributionId, number) <-
    PG.queryExpect1Row
      [PG.sql|
        WITH contrib_number AS (
            SELECT (COALESCE(MAX(contribution_number), 0) + 1) AS new
            FROM contributions contribution
            WHERE contribution.project_id = #{projectId}
        )
        INSERT INTO contributions(
          author_id,
          project_id,
          title,
          description,
          status,
          source_branch,
          target_branch,
          source_causal_id,
          target_causal_id,
          contribution_number,
          best_common_ancestor_causal_id
        )
        SELECT #{authorId}, #{projectId}, #{title}, #{description}, #{status}, #{sourceBranchId}, #{targetBranchId}, source_branch.causal_id, target_branch.causal_id, contrib_number.new, best_common_causal_ancestor(source_branch.causal_id, target_branch.causal_id)
          FROM contrib_number
          JOIN project_branches AS source_branch ON source_branch.id = #{sourceBranchId}
          JOIN project_branches AS target_branch ON target_branch.id = #{targetBranchId}
        RETURNING contributions.id, contributions.contribution_number
      |]
  insertContributionStatusChangeEvent projectId contributionId authorId Nothing status
  (projectData, projectResourceId, projectOwnerUserId) <- ProjectQ.projectNotificationData projectId

  let contributionData =
        ContributionData
          { contributionId,
            fromBranchId = sourceBranchId,
            toBranchId = targetBranchId,
            contributorUserId = authorId
          }
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectContributionCreatedData projectData contributionData,
            eventScope = projectOwnerUserId,
            eventActor = authorId
          }
  NotifQ.recordEvent notifEvent
  pure (contributionId, number)

updateContribution :: UserId -> ContributionId -> Maybe Text -> NullableUpdate Text -> Maybe ContributionStatus -> Maybe BranchId -> Maybe BranchId -> PG.Transaction e Bool
updateContribution callerUserId contributionId newTitle newDescription newStatus newSourceBranchId newTargetBranchId = do
  isJust <$> runMaybeT do
    Contribution {..} <- lift $ ContribQ.contributionById contributionId
    let updatedTitle = fromMaybe title newTitle
    let updatedDescription = fromNullableUpdate description newDescription
    let updatedStatus = fromMaybe status newStatus
    let updatedSourceBranchId = fromMaybe sourceBranchId newSourceBranchId
    let updatedTargetBranchId = fromMaybe targetBranchId newTargetBranchId
    -- Add a status change event
    when (isJust newStatus && newStatus /= Just status) do
      lift $ insertContributionStatusChangeEvent projectId contributionId callerUserId (Just status) updatedStatus
    lift $
      PG.execute_
        [PG.sql|
        UPDATE contributions
        SET
          title = #{updatedTitle},
          description = #{updatedDescription},
          status = #{updatedStatus},
          source_branch = #{updatedSourceBranchId},
          target_branch = #{updatedTargetBranchId}
        WHERE id = #{contributionId}
        |]
    -- We don't want to change the causals for merged or closed contributions because it
    -- messes with the diffs.
    -- But we do want to update them if the source or target branch has changed.
    when
      (updatedStatus == InReview || updatedStatus == Draft)
      do
        lift $
          PG.execute_
            [PG.sql|
          UPDATE contributions
          SET
            source_causal_id = (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = #{updatedSourceBranchId}),
            target_causal_id = (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = #{updatedTargetBranchId}),
            best_common_ancestor_causal_id = best_common_causal_ancestor(
              (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = #{updatedSourceBranchId}),
              (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = #{updatedTargetBranchId})
            )
          WHERE id = #{contributionId}
          |]

insertContributionStatusChangeEvent :: ProjectId -> ContributionId -> UserId -> Maybe ContributionStatus -> ContributionStatus -> PG.Transaction e ()
insertContributionStatusChangeEvent projectId contributionId actorUserId oldStatus newStatus = do
  PG.execute_
    [PG.sql|
        INSERT INTO contribution_status_events
          (contribution_id, actor, old_status, new_status)
          VALUES (#{contributionId}, #{actorUserId}, #{oldStatus}, #{newStatus})
      |]

  -- Only record a notification event if it's a status change, not a creation
  case oldStatus of
    Nothing -> pure ()
    Just _ -> do
      (projectData, projectResourceId, projectOwnerUserId) <- ProjectsQ.projectNotificationData projectId
      -- Record the status update notification event
      contributionData <- ContribQ.contributionNotificationData contributionId
      let notifEvent =
            NotificationEvent
              { eventId = (),
                eventOccurredAt = (),
                eventResourceId = projectResourceId,
                eventData = ProjectContributionUpdatedData projectData contributionData,
                eventScope = projectOwnerUserId,
                eventActor = actorUserId
              }
      NotifQ.recordEvent notifEvent

-- | Recompute the best common ancestors for all contributions related to the branch, then
-- return the set of contribution IDs which have been marked as merged.
performMergesAndBCAUpdatesFromBranchPush :: UserId -> BranchId -> PG.Transaction e (Set ContributionId)
performMergesAndBCAUpdatesFromBranchPush callerUserId branchId = do
  -- Get the new BCAs for all contributions related to the branch
  contributionsToMarkAsMerged <-
    PG.queryListCol @(ContributionId)
      [PG.sql|
    WITH new_bcas(contribution_id, source_causal_id, target_causal_id, bca_id) AS (
      SELECT contr.id, source_branch.causal_id, target_branch.causal_id, best_common_causal_ancestor(source_branch.causal_id, target_branch.causal_id) FROM contributions contr
        JOIN project_branches AS source_branch ON source_branch.id = contr.source_branch
        JOIN project_branches AS target_branch ON target_branch.id = contr.target_branch
        WHERE contr.source_branch = #{branchId} OR contr.target_branch = #{branchId}
          AND status IN (#{Draft}, #{InReview})
    ), contributions_to_mark_as_merged(contribution_id) AS (
      SELECT contribution_id FROM new_bcas
        WHERE new_bcas.bca_id IS NOT NULL
          AND new_bcas.bca_id = new_bcas.source_causal_id
    ), non_merged_bca_updates AS MATERIALIZED (
      UPDATE contributions contr
        SET best_common_ancestor_causal_id = new_bcas.bca_id,
            source_causal_id = new_bcas.source_causal_id,
            target_causal_id = new_bcas.target_causal_id
        FROM new_bcas
        WHERE
          contr.id = new_bcas.contribution_id
          AND contr.id NOT IN (SELECT contribution_id FROM contributions_to_mark_as_merged)
    ) SELECT contribution_id FROM contributions_to_mark_as_merged
      |]
  for_ contributionsToMarkAsMerged \contributionId -> do
    _success <- updateContribution callerUserId contributionId Nothing Unchanged (Just Merged) Nothing Nothing
    pure ()
  pure $ Set.fromList contributionsToMarkAsMerged
