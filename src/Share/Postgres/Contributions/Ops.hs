{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Contributions.Ops (createContribution) where

import Share.Contribution (ContributionStatus (..))
import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types (NotificationEvent (..), NotificationEventData (..), ProjectContributionData (..))
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContribQ
import Share.Prelude

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
          contribution_number,
          best_common_ancestor_causal_id
        )
        SELECT #{authorId}, #{projectId}, #{title}, #{description}, #{status}, #{sourceBranchId}, #{targetBranchId}, contrib_number.new, best_common_causal_ancestor(source_branch.causal_id, target_branch.causal_id)
          FROM contrib_number
          JOIN project_branches AS source_branch ON source_branch.id = #{sourceBranchId}
          JOIN project_branches AS target_branch ON target_branch.id = #{targetBranchId}
        RETURNING contributions.id, contributions.contribution_number
      |]
  ContribQ.insertContributionStatusChangeEvent contributionId authorId Nothing status
  let contributionEventData =
        ProjectContributionData
          { projectId,
            contributionId,
            fromBranchId = sourceBranchId,
            toBranchId = targetBranchId,
            contributorUserId = authorId
          }
  (projectResourceId, projectOwnerUserId) <-
    PG.queryExpect1Row
      [PG.sql|
    SELECT p.resource_id, p.owner_user_id FROM projects p
    WHERE p.id = #{projectId}
    |]

  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectContributionCreatedData contributionEventData,
            eventScope = projectOwnerUserId,
            eventActor = authorId
          }
  NotifQ.recordEvent notifEvent
  pure (contributionId, number)
