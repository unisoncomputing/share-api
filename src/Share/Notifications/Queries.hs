module Share.Notifications.Queries
  ( recordEvent,
    expectEvent,
    listNotificationHubEntries,
    updateNotificationHubEntries,
    addSubscriptionDeliveryMethods,
    removeSubscriptionDeliveryMethods,
    listEmailDeliveryMethods,
    listWebhooks,
    createEmailDeliveryMethod,
    createWebhookDeliveryMethod,
    deleteEmailDeliveryMethod,
    deleteWebhookDeliveryMethod,
    updateEmailDeliveryMethod,
    listNotificationSubscriptions,
    createNotificationSubscription,
    deleteNotificationSubscription,
    updateNotificationSubscription,
    getNotificationSubscription,
    hydrateEventData,
  )
where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Ord (clamp)
import Data.Set.NonEmpty (NESet)
import Data.Time (UTCTime)
import Share.Contribution
import Share.IDs
import Share.Notifications.Types
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Web.Share.DisplayInfo.Queries qualified as DisplayInfoQ
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

recordEvent :: (QueryA m) => NewNotificationEvent -> m ()
recordEvent (NotificationEvent {eventScope, eventData, eventResourceId, eventActor}) = do
  execute_
    [sql|
      INSERT INTO notification_events (topic, scope_user_id, actor_user_id, resource_id, data)
      VALUES (#{eventTopic eventData}::notification_topic, #{eventScope}, #{eventActor}, #{eventResourceId}, #{eventData})
    |]

expectEvent :: (QueryM m) => NotificationEventId -> m PGNotificationEvent
expectEvent eventId = do
  queryExpect1Row @PGNotificationEvent
    [sql|
      SELECT id, occurred_at, scope_user_id, actor_user_id, resource_id, topic, data
        FROM notification_events
      WHERE id = #{eventId}
    |]

listNotificationHubEntries :: UserId -> Maybe Int -> Maybe UTCTime -> Maybe (NESet NotificationStatus) -> Transaction e [NotificationHubEntry UnifiedDisplayInfo HydratedEventPayload]
listNotificationHubEntries notificationUserId mayLimit afterTime statusFilter = do
  let limit = clamp (0, 1000) . fromIntegral @Int @Int32 . fromMaybe 50 $ mayLimit
  let statusFilterList = Foldable.toList <$> statusFilter
  dbNotifications <-
    queryListRows @(NotificationHubEntry UserId NotificationEventData)
      [sql|
      SELECT hub.id, hub.status, event.id, event.occurred_at, event.scope_user_id, event.actor_user_id, event.resource_id, event.topic, event.data
        FROM notification_hub_entries hub
        JOIN notification_events event ON hub.event_id = event.id
      WHERE hub.user_id = #{notificationUserId}
            AND (#{statusFilterList} IS NULL OR hub.status = ANY(#{statusFilterList}::notification_status[]))
            AND (#{afterTime} IS NULL OR event.occurred_at > #{afterTime})
      ORDER BY hub.created_at DESC
      LIMIT #{limit}
    |]
  hydrated <- PG.pipelined $ forOf (traversed . traversed) dbNotifications hydrateEventData
  hydrated & DisplayInfoQ.unifiedDisplayInfoForUserOf (traversed . hubEntryUserInfo_)

updateNotificationHubEntries :: (QueryA m) => NESet NotificationHubEntryId -> NotificationStatus -> m ()
updateNotificationHubEntries hubEntryIds status = do
  execute_
    [sql|
      WITH to_update(notification_id) AS (
        SELECT * FROM ^{singleColumnTable $ Foldable.toList hubEntryIds}
      )
      UPDATE notification_hub_entries
      SET status = #{status}::notification_status
      WHERE id IN (SELECT notification_id FROM to_update)
    |]

-- | Note: If a given delivery method belongs to a different subscriber user, it will simply be ignored,
-- this should never happen in non-malicious workflows so it's fine to ignore it.
addSubscriptionDeliveryMethods :: UserId -> NotificationSubscriptionId -> NESet DeliveryMethodId -> Transaction e ()
addSubscriptionDeliveryMethods subscriberUserId subscriptionId deliveryMethods = do
  let (emailIds, webhookIds) =
        deliveryMethods & foldMap \case
          EmailDeliveryMethodId emailId -> ([emailId], mempty)
          WebhookDeliveryMethodId webhookId -> (mempty, [webhookId])
  execute_
    [sql|
      WITH email_ids(email_id) AS (
        SELECT * FROM ^{singleColumnTable emailIds}
      )
      INSERT INTO notification_by_email (subscription_id, email_id)
      SELECT #{subscriptionId}, ei.email_id
        FROM email_ids ei
        JOIN notification_emails ne ON ei.email_id = ne.id
      WHERE ne.subscriber_user_id = #{subscriberUserId}
      ON CONFLICT DO NOTHING
    |]
  execute_
    [sql|
      WITH webhook_ids(webhook_id) AS (
        SELECT * FROM ^{singleColumnTable webhookIds}
      )
      INSERT INTO notification_by_webhook (subscription_id, webhook_id)
      SELECT #{subscriptionId}, wi.webhook_id
        FROM webhook_ids wi
        JOIN notification_webhooks nw ON wi.webhook_id = nw.id
      WHERE nw.subscriber_user_id = #{subscriberUserId}
      ON CONFLICT DO NOTHING
    |]

removeSubscriptionDeliveryMethods :: UserId -> NotificationSubscriptionId -> NESet DeliveryMethodId -> Transaction e ()
removeSubscriptionDeliveryMethods subscriberUserId subscriptionId deliveryMethods = do
  let (emailIds, webhookIds) =
        deliveryMethods & foldMap \case
          EmailDeliveryMethodId emailId -> ([emailId], mempty)
          WebhookDeliveryMethodId webhookId -> (mempty, [webhookId])
  execute_
    [sql|
      DELETE FROM notification_by_email
      WHERE subscription_id = #{subscriptionId}
        AND email_id IN (SELECT * FROM ^{singleColumnTable emailIds})
        AND EXISTS (
          SELECT 1
            FROM notification_emails ne
          WHERE ne.id = email_id
                AND ne.subscriber_user_id = #{subscriberUserId}
        )
    |]
  execute_
    [sql|
      DELETE FROM notification_by_webhook
      WHERE subscription_id = #{subscriptionId}
        AND webhook_id IN (SELECT * FROM ^{singleColumnTable webhookIds})
        AND EXISTS (
          SELECT 1
            FROM notification_webhooks nw
          WHERE nw.id = webhook_id
                AND nw.subscriber_user_id = #{subscriberUserId}
        )
    |]

listEmailDeliveryMethods :: (QueryA m) => UserId -> Maybe NotificationSubscriptionId -> m [NotificationEmailDeliveryConfig]
listEmailDeliveryMethods userId maySubscriptionId = do
  queryListRows
    [sql|
      SELECT ne.id, ne.email
        FROM notification_emails ne
      WHERE ne.subscriber_user_id = #{userId}
        AND (#{maySubscriptionId} IS NULL
              OR EXISTS(
                   SELECT FROM notification_by_email nbe
                     WHERE nbe.subscription_id = #{maySubscriptionId}
                           AND nbe.email_id = ne.id
                       )
            )
      ORDER BY ne.email
    |]

listWebhooks :: (QueryA m) => UserId -> Maybe NotificationSubscriptionId -> m [NotificationWebhookId]
listWebhooks userId maySubscriptionId = do
  queryListCol
    [sql|
      SELECT nw.id
        FROM notification_webhooks nw
      WHERE nw.subscriber_user_id = #{userId}
        AND (#{maySubscriptionId} IS NULL
              OR EXISTS(
                   SELECT FROM notification_by_webhook nbw
                     WHERE nbw.subscription_id = #{maySubscriptionId}
                           AND nbw.webhook_id = nw.id
                       )
            )
        ORDER BY nw.created_at
    |]

createEmailDeliveryMethod :: UserId -> Email -> Transaction e NotificationEmailDeliveryMethodId
createEmailDeliveryMethod userId email = do
  existingEmailDeliveryMethodId <-
    query1Col
      [sql|
      SELECT id
        FROM notification_emails
      WHERE subscriber_user_id = #{userId}
            AND email = #{email}
    |]
  case existingEmailDeliveryMethodId of
    Just emailDeliveryMethodId -> pure emailDeliveryMethodId
    Nothing -> do
      queryExpect1Col
        [sql|
          INSERT INTO notification_emails (subscriber_user_id, email)
          VALUES (#{userId}, #{email})
          RETURNING id
        |]

updateEmailDeliveryMethod :: UserId -> NotificationEmailDeliveryMethodId -> Email -> Transaction e ()
updateEmailDeliveryMethod notificationUserId emailDeliveryMethodId email = do
  execute_
    [sql|
      UPDATE notification_emails
      SET email = #{email}
      WHERE id = #{emailDeliveryMethodId}
        AND subscriber_user_id = #{notificationUserId}
    |]

deleteEmailDeliveryMethod :: UserId -> NotificationEmailDeliveryMethodId -> Transaction e ()
deleteEmailDeliveryMethod notificationUserId emailDeliveryMethodId = do
  execute_
    [sql|
      DELETE FROM notification_emails
      WHERE id = #{emailDeliveryMethodId}
        AND subscriber_user_id = #{notificationUserId}
    |]

createWebhookDeliveryMethod :: UserId -> Transaction e NotificationWebhookId
createWebhookDeliveryMethod userId = do
  queryExpect1Col
    [sql|
          INSERT INTO notification_webhooks (subscriber_user_id)
          VALUES (#{userId})
          RETURNING id
        |]

deleteWebhookDeliveryMethod :: UserId -> NotificationWebhookId -> Transaction e ()
deleteWebhookDeliveryMethod notificationUserId webhookDeliveryMethodId = do
  execute_
    [sql|
      DELETE FROM notification_webhooks
      WHERE id = #{webhookDeliveryMethodId}
        AND subscriber_user_id = #{notificationUserId}
    |]

listNotificationSubscriptions :: UserId -> Transaction e [NotificationSubscription NotificationSubscriptionId]
listNotificationSubscriptions subscriberUserId = do
  queryListRows
    [sql|
      SELECT ns.id, ns.scope_user_id, ns.topics, ns.filter
        FROM notification_subscriptions ns
      WHERE ns.subscriber_user_id = #{subscriberUserId}
      ORDER BY ns.created_at DESC
    |]

createNotificationSubscription :: UserId -> UserId -> NESet NotificationTopic -> Maybe SubscriptionFilter -> Transaction e NotificationSubscriptionId
createNotificationSubscription subscriberUserId subscriptionScope subscriptionTopics subscriptionFilter = do
  queryExpect1Col
    [sql|
      INSERT INTO notification_subscriptions (subscriber_user_id, scope_user_id, topics, filter)
      VALUES (#{subscriberUserId}, #{subscriptionScope}, #{Foldable.toList subscriptionTopics}::notification_topic[], #{subscriptionFilter})
      RETURNING id
    |]

deleteNotificationSubscription :: UserId -> NotificationSubscriptionId -> Transaction e ()
deleteNotificationSubscription subscriberUserId subscriptionId = do
  execute_
    [sql|
      DELETE FROM notification_subscriptions
      WHERE id = #{subscriptionId}
        AND subscriber_user_id = #{subscriberUserId}
    |]

updateNotificationSubscription :: UserId -> NotificationSubscriptionId -> Maybe (NESet NotificationTopic) -> Maybe SubscriptionFilter -> Transaction e ()
updateNotificationSubscription subscriberUserId subscriptionId subscriptionTopics subscriptionFilter = do
  execute_
    [sql|
      UPDATE notification_subscriptions
      SET topics = COALESCE(#{Foldable.toList <$> subscriptionTopics}::notification_topic[], topics),
          filter = COALESCE(#{subscriptionFilter}, filter)
      WHERE id = #{subscriptionId}
        AND subscriber_user_id = #{subscriberUserId}
    |]

getNotificationSubscription :: UserId -> NotificationSubscriptionId -> Transaction e (NotificationSubscription NotificationSubscriptionId)
getNotificationSubscription subscriberUserId subscriptionId = do
  queryExpect1Row
    [sql|
      SELECT ns.id, ns.scope_user_id, ns.topics, ns.filter
        FROM notification_subscriptions ns
      WHERE ns.id = #{subscriptionId}
        AND ns.subscriber_user_id = #{subscriberUserId}
    |]

-- | Events are complex, so for now we hydrate them one at a time using a simple traverse
-- (preferably pipelined).
--
-- If need be we can write a batch job in plpgsql to hydrate them all at once.
hydrateEventData :: forall m. (QueryA m) => NotificationEventData -> m HydratedEventPayload
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
      contribution <- ContributionQ.contributionById contributionId
      pure $
        ProjectContributionPayload
          { projectInfo,
            mergeSourceBranch,
            mergeTargetBranch,
            author,
            title = contribution.title,
            description = contribution.description,
            contributionId,
            status = contribution.status
          }
    hydrateProjectBranchPayload projectId branchId = do
      branchInfo <- hydrateBranchPayload branchId
      projectInfo <- hydrateProjectPayload projectId
      pure $ ProjectBranchPayload {projectInfo, branchInfo}
    hydrateBranchPayload :: BranchId -> m BranchPayload
    hydrateBranchPayload branchId = do
      queryExpect1Row
        [sql|
          SELECT b.name, contributor.id, contributor.handle
          FROM project_branches b
          LEFT JOIN users contributor ON b.contributor_id = contributor.id
          WHERE b.id = #{branchId}
          |]
        <&> \( branchName,
               branchContributorUserId,
               branchContributorHandle
               ) ->
            let branchShortHand = BranchShortHand {contributorHandle = branchContributorHandle, branchName}
             in BranchPayload
                  { branchId,
                    branchName,
                    branchShortHand,
                    branchContributorUserId,
                    branchContributorHandle
                  }
    hydrateProjectPayload :: ProjectId -> m ProjectPayload
    hydrateProjectPayload projectId = do
      queryExpect1Row
        [sql|
          SELECT p.slug, owner.handle, p.owner_user_id
          FROM projects p
          JOIN users owner ON p.owner_user_id = owner.id
          WHERE p.id = #{projectId}
          |]
        <&> \( projectSlug,
               projectOwnerHandle,
               projectOwnerUserId
               ) ->
            let projectShortHand = ProjectShortHand {userHandle = projectOwnerHandle, projectSlug}
             in ProjectPayload
                  { projectId,
                    projectSlug,
                    projectShortHand,
                    projectOwnerHandle,
                    projectOwnerUserId
                  }
