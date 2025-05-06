-- | This module contains queries related to sending notification webhooks.
module Share.BackgroundJobs.Webhooks.Queries
  ( queueWebhook,
    getUnsentWebhook,
    recordFailedDeliveryAttempt,
    markWebhookAsDelivered,
    hydrateEventData,
  )
where

import Share.BackgroundJobs.Webhooks.Types
import Share.Contribution (Contribution (..))
import Share.IDs
import Share.Notifications.Types
import Share.Postgres
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Notifications qualified as Notif
import Share.Postgres.Users.Queries qualified as UsersQ

queueWebhook :: (QueryM m) => NotificationEventId -> NotificationWebhookId -> m ()
queueWebhook eventId webhookId = do
  execute_
    [sql|
    INSERT INTO notification_webhook_queue (event_id, webhook_id)
    VALUES (#{eventId}, #{webhookId})
    |]
  Notif.notifyChannel Notif.WebhooksChannel

-- | Claim the oldest unsent webhook with some delivery attempts left.
getUnsentWebhook :: Transaction e (Maybe (NotificationEventId, NotificationWebhookId))
getUnsentWebhook = do
  query1Row @(NotificationEventId, NotificationWebhookId)
    [sql|
      SELECT q.event_id, q.webhook_id
      FROM notification_webhook_queue q
      WHERE NOT delivered AND delivery_attempts_remaining > 0
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    |]

recordFailedDeliveryAttempt :: (QueryM m) => NotificationEventId -> NotificationWebhookId -> m ()
recordFailedDeliveryAttempt eventId webhookId = do
  execute_
    [sql|
    UPDATE notification_webhook_queue
    SET delivery_attempts_remaining = delivery_attempts_remaining - 1
    WHERE event_id = #{eventId} AND webhook_id = #{webhookId}
    |]

markWebhookAsDelivered :: (QueryM m) => NotificationEventId -> NotificationWebhookId -> m ()
markWebhookAsDelivered eventId webhookId = do
  execute_
    [sql|
    UPDATE notification_webhook_queue
    SET delivered = TRUE
    WHERE event_id = #{eventId} AND webhook_id = #{webhookId}
    |]

hydrateEventData :: forall m. (QueryM m) => NotificationEventData -> m WebhookPayloadData
hydrateEventData = \case
  ProjectBranchUpdatedData
    (ProjectBranchData {projectId, branchId}) -> do
      ProjectBranchUpdatedPayload <$> hydrateProjectBranchPayload projectId branchId
  ProjectContributionCreatedData
    (ProjectContributionData {projectId, contributionId, fromBranchId, toBranchId, contributorUserId}) -> do
      ProjectContributionCreatedPayload <$> hydrateContributionInfo contributionId projectId fromBranchId toBranchId contributorUserId
  where
    hydrateContributionInfo :: ContributionId -> ProjectId -> BranchId -> BranchId -> UserId -> m ProjectContributionPayload
    hydrateContributionInfo contributionId projectId fromBranchId toBranchId authorUserId = do
      author <- UsersQ.userDisplayInfoOf id authorUserId
      projectInfo <- hydrateProjectPayload projectId
      mergeTargetBranch <- hydrateBranchPayload fromBranchId
      mergeSourceBranch <- hydrateBranchPayload toBranchId
      Contribution {title, description, status} <- ContributionQ.contributionById contributionId
      pure $
        ProjectContributionPayload
          { projectInfo,
            mergeSourceBranch,
            mergeTargetBranch,
            author,
            title,
            description,
            contributionId,
            status
          }
    hydrateProjectBranchPayload projectId branchId = do
      branchInfo <- hydrateBranchPayload branchId
      projectInfo <- hydrateProjectPayload projectId
      pure $ ProjectBranchPayload {projectInfo, branchInfo}
    hydrateBranchPayload branchId = do
      ( branchName,
        branchContributorUserId,
        branchContributorHandle
        ) <-
        queryExpect1Row
          [sql|
          SELECT b.name, contributor.id, contributor.handle
          FROM project_branches b
          LEFT JOIN users contributor ON b.contributor_id = contributor.id
          WHERE b.id = #{branchId}
          |]
      let branchShortHand = BranchShortHand {contributorHandle = branchContributorHandle, branchName}
      pure $
        BranchPayload
          { branchId,
            branchName,
            branchShortHand,
            branchContributorUserId,
            branchContributorHandle
          }
    hydrateProjectPayload projectId = do
      ( projectSlug,
        projectOwnerHandle,
        projectOwnerUserId
        ) <-
        queryExpect1Row
          [sql|
          SELECT p.slug, owner.handle, p.owner_user_id
          FROM projects p
          JOIN users owner ON p.owner_user_id = owner.id
          WHERE p.id = #{projectId}
          |]
      let projectShortHand = ProjectShortHand {userHandle = projectOwnerHandle, projectSlug}
      pure $
        ProjectPayload
          { projectId,
            projectSlug,
            projectShortHand,
            projectOwnerHandle,
            projectOwnerUserId
          }
