{-# LANGUAGE TypeOperators #-}

module Share.Notifications.Queries
  ( recordEvent,
    expectEvent,
    listNotificationHubEntryPayloads,
    updateNotificationHubEntries,
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
    expectNotificationSubscription,
    hydrateEventPayload,
    hasUnreadNotifications,
    updateWatchProjectSubscription,
    isUserSubscribedToWatchProject,
    listProjectWebhooks,
    webhooksForSubscription,
    expectProjectWebhook,
  )
where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Ord (clamp)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Share.Contribution
import Share.IDs
import Share.Notifications.API (GetHubEntriesCursor)
import Share.Notifications.Types
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Releases.Queries qualified as ReleasesQ
import Share.Postgres.Tickets.Queries qualified as TicketQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Release (Release (..))
import Share.Ticket
import Share.Utils.API (Cursor (..), CursorDirection (..), Paged (..), guardPaged, pagedOn)
import Share.Web.Share.DisplayInfo.Queries qualified as DisplayInfoQ
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)

recordEvent :: (QueryA m) => NewNotificationEvent -> m ()
recordEvent (NotificationEvent {eventScope, eventData, eventResourceId, eventProjectId, eventActor}) = do
  execute_
    [sql|
      INSERT INTO notification_events (topic, scope_user_id, actor_user_id, resource_id, project_id, data)
      VALUES (#{eventTopic eventData}::notification_topic, #{eventScope}, #{eventActor}, #{eventResourceId}, #{eventProjectId}, #{eventData})
    |]

expectEvent :: (QueryM m) => NotificationEventId -> m PGNotificationEvent
expectEvent eventId = do
  queryExpect1Row @PGNotificationEvent
    [sql|
      SELECT id, occurred_at, scope_user_id, actor_user_id, resource_id, project_id, topic, data
        FROM notification_events
      WHERE id = #{eventId}
    |]

listNotificationHubEntryPayloads :: UserId -> Maybe Int -> Maybe (Cursor GetHubEntriesCursor) -> Maybe (NESet NotificationStatus) -> Transaction e (Paged GetHubEntriesCursor (NotificationHubEntry UnifiedDisplayInfo HydratedEventPayload))
listNotificationHubEntryPayloads notificationUserId mayLimit mayCursor statusFilter = do
  let limit = clamp (0, 100) . fromIntegral @Int @Int32 . fromMaybe 50 $ mayLimit
  let mkCursorFilter = \case
        Nothing -> mempty
        Just (Cursor (beforeTime, entryId) Previous) -> [PG.sql| AND (hub.created_at, hub.id) > (#{beforeTime}, #{entryId})|]
        Just (Cursor (afterTime, entryId) Next) -> [PG.sql| AND (hub.created_at, hub.id) < (#{afterTime}, #{entryId})|]
  dbNotifications <- query limit (mkCursorFilter mayCursor)
  hydratedPayloads <- PG.pipelined $ forOf (traversed . traversed) dbNotifications hydrateEventPayload
  results <- hydratedPayloads & DisplayInfoQ.unifiedDisplayInfoForUserOf (traversed . hubEntryUserInfo_)
  let paged@(Paged {prevCursor, nextCursor}) =
        results
          & pagedOn (\(NotificationHubEntry {hubEntryId, hubEntryCreatedAt}) -> (hubEntryCreatedAt, hubEntryId))
  hasPrevPage <- not . null <$> query 1 (mkCursorFilter prevCursor)
  hasNextPage <- not . null <$> query 1 (mkCursorFilter nextCursor)
  pure $ guardPaged hasPrevPage hasNextPage paged
  where
    statusFilterList :: Maybe [NotificationStatus]
    statusFilterList = Foldable.toList <$> statusFilter
    query ::
      (QueryA m) =>
      Int32 ->
      PG.Sql ->
      m [NotificationHubEntry UserId NotificationEventData]
    query limit cursorFilter =
      queryListRows @(NotificationHubEntry UserId NotificationEventData)
        [sql|
          SELECT hub.id, hub.status, hub.created_at, event.id, event.occurred_at, event.scope_user_id, event.actor_user_id, event.resource_id, event.topic, event.data
            FROM notification_hub_entries hub
            JOIN notification_events event ON hub.event_id = event.id
          WHERE hub.user_id = #{notificationUserId}
                AND (#{statusFilterList} IS NULL OR hub.status = ANY(#{statusFilterList}::notification_status[]))
                -- By default omit notifications that are from the user themself.
                AND event.actor_user_id <> #{notificationUserId}
                ^{cursorFilter}
          ORDER BY hub.created_at DESC
          LIMIT #{limit}
        |]

hasUnreadNotifications :: UserId -> Transaction e Bool
hasUnreadNotifications notificationUserId = do
  queryExpect1Col
    [sql|
      SELECT EXISTS(
        SELECT
          FROM notification_hub_entries hub
          JOIN notification_events event ON hub.event_id = event.id
        WHERE hub.user_id = #{notificationUserId}
              AND hub.status = #{Unread}::notification_status
              AND event.actor_user_id <> #{notificationUserId}
      )
    |]

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

createWebhookDeliveryMethod :: Text -> NotificationSubscriptionId -> Transaction e NotificationWebhookId
createWebhookDeliveryMethod name subscriptionId = do
  queryExpect1Col
    [sql|
          INSERT INTO notification_webhooks (name, subscription_id)
          VALUES (#{name}, #{subscriptionId})
          RETURNING id
        |]

deleteWebhookDeliveryMethod :: NotificationWebhookId -> Transaction e ()
deleteWebhookDeliveryMethod webhookDeliveryMethodId = do
  execute_
    [sql|
      DELETE FROM notification_webhooks
      WHERE id = #{webhookDeliveryMethodId}
    |]

listNotificationSubscriptions :: UserId -> Transaction e [NotificationSubscription NotificationSubscriptionId]
listNotificationSubscriptions subscriberUserId = do
  queryListRows
    [sql|
      SELECT
        ns.id,
        ns.scope_user_id,
        ns.scope_project_id,
        ns.subscriber_user_id,
        ns.subscriber_project_id,
        ns.topics,
        ns.topic_groups,
        ns.filter,
        ns.created_at,
        ns.updated_at
        FROM notification_subscriptions ns
      WHERE ns.subscriber_user_id = #{subscriberUserId}
      ORDER BY ns.created_at DESC
    |]

createNotificationSubscription :: SubscriptionOwner -> UserId -> Maybe ProjectId -> Set NotificationTopic -> Set NotificationTopicGroup -> Maybe SubscriptionFilter -> Transaction e NotificationSubscriptionId
createNotificationSubscription owner subscriptionScope subscriptionProjectId subscriptionTopics subscriptionTopicGroups subscriptionFilter = do
  let (subscriberUserId, subscriberProjectId) = case owner of
        ProjectSubscriptionOwner projectId -> (Nothing, Just projectId)
        UserSubscriptionOwner userId -> (Just userId, Nothing)
  queryExpect1Col
    [sql|
      INSERT INTO notification_subscriptions (subscriber_user_id, subscriber_project_id, scope_user_id, scope_project_id, topics, topic_groups, filter)
      VALUES (#{subscriberUserId}, #{subscriberProjectId}, #{subscriptionScope}, #{subscriptionProjectId}, #{Foldable.toList subscriptionTopics}::notification_topic[], #{Foldable.toList subscriptionTopicGroups}::notification_topic_group[], #{subscriptionFilter})
      RETURNING id
    |]

deleteNotificationSubscription :: SubscriptionOwner -> NotificationSubscriptionId -> Transaction e ()
deleteNotificationSubscription owner subscriptionId = do
  let ownerFilter = case owner of
        UserSubscriptionOwner subscriberUserId -> [sql| subscriber_user_id = #{subscriberUserId}|]
        ProjectSubscriptionOwner projectOwnerUserId -> [sql| subscriber_project_id = #{projectOwnerUserId} |]
  execute_
    [sql|
      DELETE FROM notification_subscriptions
      WHERE id = #{subscriptionId}
        AND ^{ownerFilter}
    |]

updateNotificationSubscription :: SubscriptionOwner -> NotificationSubscriptionId -> Maybe (Set NotificationTopic) -> Maybe (Set NotificationTopicGroup) -> Maybe SubscriptionFilter -> Transaction e ()
updateNotificationSubscription owner subscriptionId subscriptionTopics subscriptionTopicGroups subscriptionFilter = do
  let ownerFilter = case owner of
        UserSubscriptionOwner subscriberUserId -> [sql| subscriber_user_id = #{subscriberUserId}|]
        ProjectSubscriptionOwner projectOwnerUserId -> [sql| subscriber_project_id = #{projectOwnerUserId} |]
  execute_
    [sql|
      UPDATE notification_subscriptions
      SET topics = COALESCE(#{Foldable.toList <$> subscriptionTopics}::notification_topic[], topics),
          topic_groups = COALESCE(#{Foldable.toList <$> subscriptionTopicGroups}::notification_topic_group[], topic_groups),
          filter = COALESCE(#{subscriptionFilter}, filter)
      WHERE id = #{subscriptionId}
        AND ^{ownerFilter}
    |]

expectNotificationSubscription :: SubscriptionOwner -> NotificationSubscriptionId -> Transaction e (NotificationSubscription NotificationSubscriptionId)
expectNotificationSubscription subscriptionOwner subscriptionId = do
  let ownerFilter = case subscriptionOwner of
        UserSubscriptionOwner subscriberUserId -> [sql| ns.subscriber_user_id = #{subscriberUserId}|]
        ProjectSubscriptionOwner projectOwnerUserId -> [sql| ns.subscriber_project_id = #{projectOwnerUserId} |]
  queryExpect1Row
    [sql|
      SELECT
        ns.id,
        ns.scope_user_id,
        ns.scope_project_id,
        ns.subscriber_user_id,
        ns.subscriber_project_id,
        ns.topics,
        ns.topic_groups,
        ns.filter,
        ns.created_at,
        ns.updated_at
        FROM notification_subscriptions ns
      WHERE ns.id = #{subscriptionId}
        AND ^{ownerFilter}
    |]

-- | Events are complex, so for now we hydrate them one at a time using a simple traverse
-- (preferably pipelined).
--
-- If need be we can write a batch job in plpgsql to hydrate them all at once.
hydrateEventPayload :: forall m. (QueryA m) => NotificationEventData -> m HydratedEventPayload
hydrateEventPayload = \case
  ProjectBranchUpdatedData
    (ProjectData {projectId})
    (BranchData {branchId}) -> do
      (\projectPayload mkBranchPayload -> HydratedProjectBranchUpdatedPayload projectPayload (mkBranchPayload projectPayload))
        <$> hydrateProjectPayload projectId
        <*> hydrateBranchPayload branchId
  ProjectContributionCreatedData
    (ProjectData {projectId})
    (ContributionData {contributionId, fromBranchId, toBranchId, contributorUserId}) -> do
      (\projectPayload mkContributionPayload -> HydratedProjectContributionCreatedPayload projectPayload (mkContributionPayload projectPayload))
        <$> hydrateProjectPayload projectId
        <*> hydrateContributionInfo contributionId fromBranchId toBranchId contributorUserId
  ProjectContributionStatusUpdatedData
    (ProjectData {projectId})
    (ContributionData {contributionId, fromBranchId, toBranchId, contributorUserId})
    (StatusUpdateData {oldStatus, newStatus}) -> do
      let statusPayload = StatusUpdatePayload {oldStatus, newStatus}
      (\projectPayload mkContributionPayload -> HydratedProjectContributionStatusUpdatedPayload projectPayload (mkContributionPayload projectPayload) statusPayload)
        <$> hydrateProjectPayload projectId
        <*> hydrateContributionInfo contributionId fromBranchId toBranchId contributorUserId
  ProjectContributionCommentData
    (ProjectData {projectId})
    (ContributionData {contributionId, fromBranchId, toBranchId, contributorUserId})
    (CommentData {commentId, commentAuthorUserId}) -> do
      (\projectPayload mkContributionPayload -> HydratedProjectContributionCommentPayload projectPayload (mkContributionPayload projectPayload))
        <$> hydrateProjectPayload projectId
        <*> hydrateContributionInfo contributionId fromBranchId toBranchId contributorUserId
        <*> hydrateCommentPayload commentId commentAuthorUserId
  ProjectTicketCreatedData
    (ProjectData {projectId})
    (TicketData {ticketId, ticketAuthorUserId}) -> do
      HydratedProjectTicketCreatedPayload
        <$> hydrateProjectPayload projectId
        <*> hydrateTicketInfo ticketId ticketAuthorUserId
  ProjectTicketStatusUpdatedData
    (ProjectData {projectId})
    (TicketData {ticketId, ticketAuthorUserId})
    (StatusUpdateData {oldStatus, newStatus}) -> do
      let statusPayload = StatusUpdatePayload {oldStatus, newStatus}
      HydratedProjectTicketStatusUpdatedPayload
        <$> hydrateProjectPayload projectId
        <*> hydrateTicketInfo ticketId ticketAuthorUserId
        <*> pure statusPayload
  ProjectTicketCommentData
    (ProjectData {projectId})
    (TicketData {ticketId, ticketAuthorUserId})
    (CommentData {commentId, commentAuthorUserId}) -> do
      HydratedProjectTicketCommentPayload
        <$> hydrateProjectPayload projectId
        <*> hydrateTicketInfo ticketId ticketAuthorUserId
        <*> hydrateCommentPayload commentId commentAuthorUserId
  ProjectReleaseCreatedData
    (ProjectData {projectId})
    (ReleaseData {releaseId}) -> do
      projectInfo <- hydrateProjectPayload projectId
      releasePayload <- hydrateReleasePayload releaseId
      pure $ HydratedProjectReleaseCreatedPayload projectInfo releasePayload
  where
    hydrateReleasePayload :: ReleaseId -> m ReleasePayload
    hydrateReleasePayload releaseId = do
      release <- ReleasesQ.releaseById releaseId
      pure $
        ReleasePayload
          { releaseId,
            releaseVersion = release.version,
            releaseCreatedAt = release.createdAt
          }
    hydrateTicketInfo :: TicketId -> UserId -> m TicketPayload
    hydrateTicketInfo ticketId authorUserId = do
      author <- UsersQ.userDisplayInfoOf id authorUserId
      ticket <- TicketQ.ticketById ticketId
      pure $
        TicketPayload
          { ticketId,
            ticketNumber = ticket.number,
            ticketTitle = ticket.title,
            ticketDescription = ticket.description,
            ticketStatus = ticket.status,
            ticketAuthor = author,
            ticketCreatedAt = ticket.createdAt
          }
    hydrateContributionInfo :: ContributionId -> BranchId -> BranchId -> UserId -> m (ProjectPayload -> ContributionPayload)
    hydrateContributionInfo contributionId fromBranchId toBranchId authorUserId = do
      author <- UsersQ.userDisplayInfoOf id authorUserId
      targetBranch <- hydrateBranchPayload fromBranchId
      sourceBranch <- hydrateBranchPayload toBranchId
      contribution <- ContributionQ.contributionById contributionId
      pure $ \projectInfo ->
        ContributionPayload
          { contributionId,
            contributionNumber = contribution.number,
            contributionTitle = contribution.title,
            contributionDescription = contribution.description,
            contributionStatus = contribution.status,
            contributionAuthor = author,
            contributionSourceBranch = sourceBranch projectInfo,
            contributionTargetBranch = targetBranch projectInfo,
            contributionCreatedAt = contribution.createdAt
          }
    hydrateBranchPayload ::
      BranchId ->
      -- We return a func so we can convince GHC this whole thing is Applicative
      m (ProjectPayload -> BranchPayload)
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
               )
             projectInfo ->
            let branchShortHand = BranchShortHand {contributorHandle = branchContributorHandle, branchName}
                projectBranchShortHand = ProjectBranchShortHand {userHandle = projectInfo.projectOwnerHandle, projectSlug = projectInfo.projectSlug, contributorHandle = branchContributorHandle, branchName}
             in BranchPayload
                  { branchId,
                    branchName,
                    branchShortHand,
                    projectBranchShortHand,
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
    hydrateCommentPayload :: CommentId -> UserId -> m CommentPayload
    hydrateCommentPayload commentId commentAuthorUserId = do
      let construct (commentId, commentContent, commentCreatedAt) commentAuthor =
            CommentPayload
              { commentId,
                commentContent,
                commentCreatedAt,
                commentAuthor
              }
      construct
        <$> ( queryExpect1Row
                [sql|
                SELECT cc.comment_id, cc.content, cc.created_at, cc.updated_at
                FROM comment_content cc
                JOIN users author ON cc.author_id = author.id
                WHERE cc.comment_id = #{commentId}
              |]
            )
        <*> (UsersQ.userDisplayInfoOf id commentAuthorUserId)

-- | Subscribe or unsubscribe to watching a project
updateWatchProjectSubscription :: UserId -> ProjectId -> Bool -> Transaction e (Maybe NotificationSubscriptionId)
updateWatchProjectSubscription userId projId shouldBeSubscribed = do
  let filter = SubscriptionFilter $ Aeson.object []
  existing <- isUserSubscribedToWatchProject userId projId
  case existing of
    Just existingId
      | not shouldBeSubscribed -> do
          execute_
            [sql|
              DELETE FROM notification_subscriptions
              WHERE id = #{existingId}
                AND subscriber_user_id = #{userId}
            |]
          pure Nothing
      | otherwise -> pure (Just existingId)
    Nothing | shouldBeSubscribed -> do
      -- Create a new subscription
      projectOwnerUserId <-
        queryExpect1Col
          [sql|
          SELECT p.owner_user_id
            FROM projects p
          WHERE p.id = #{projId}
        |]
      Just <$> createNotificationSubscription (UserSubscriptionOwner userId) projectOwnerUserId (Just projId) mempty (Set.singleton WatchProject) (Just filter)
    _ -> pure Nothing

isUserSubscribedToWatchProject :: UserId -> ProjectId -> Transaction e (Maybe NotificationSubscriptionId)
isUserSubscribedToWatchProject userId projId = do
  let filter = SubscriptionFilter $ Aeson.object []
  query1Col @NotificationSubscriptionId
    [sql|
      SELECT ns.id FROM notification_subscriptions ns
                   JOIN projects p ON p.id = #{projId}
        WHERE ns.subscriber_user_id = #{userId}
          AND ns.scope_user_id = p.owner_user_id
          AND ns.topic_groups = ARRAY[#{WatchProject}::notification_topic_group]
          AND ns.project_id = #{projId}
          AND ns.filter = #{filter}::jsonb
          LIMIT 1
    |]

-- | We provide a wrapper layer on top of notification subscriptions and webhooks
-- to make the frontend experience a bit more intuitive.
listProjectWebhooks :: UserId -> ProjectId -> PG.Transaction e [(NotificationWebhookId, Text, NotificationSubscription NotificationSubscriptionId)]
listProjectWebhooks caller projectId = do
  PG.queryListRows @((NotificationWebhookId, Text) PG.:. NotificationSubscription NotificationSubscriptionId)
    [PG.sql|
      -- Only get one webhook per subscription, there _shouldn't_ be multiple webhooks per
      -- subscription if everything is working correctly.
      SELECT DISTINCT ON (ns.id)
        nw.id,
        nw.name,
        -- We ignore topic groups here for now and only allow the user to configure
        -- individual topic events.
        ns.id,
        ns.scope_user_id,
        ns.scope_project_id,
        ns.subscriber_user_id,
        ns.subscriber_project_id,
        ns.topics,
        ns.topic_groups,
        ns.filter,
        ns.created_at,
        ns.updated_at
      FROM notification_subscriptions ns
      JOIN notification_by_webhook nbw ON ns.id = nbw.subscription_id
      JOIN notification_webhooks nw ON nbw.webhook_id = nw.id
      WHERE ns.project_id = #{projectId}
        AND ns.subscriber_user_id = #{caller}
    |]
    <&> fmap (\((webhookId, webhookName) PG.:. subscription) -> (webhookId, webhookName, subscription))

-- | Gets the (first) webhook associated with a project webhook notification.
expectProjectWebhook :: ProjectId -> NotificationSubscriptionId -> PG.Transaction e (NotificationWebhookId, Text)
expectProjectWebhook projectId subscriptionId = do
  PG.queryExpect1Row @(NotificationWebhookId, Text)
    [PG.sql|
      SELECT nw.id, nw.name
        FROM notification_webhooks nw
      WHERE nw.subscription_id = #{subscriptionId}
        AND nw.subscriber_project_id = #{projectId}
      LIMIT 1
    |]

webhooksForSubscription :: NotificationSubscriptionId -> Transaction e [NotificationWebhookId]
webhooksForSubscription subscriptionId = do
  PG.queryListCol
    [PG.sql|
      SELECT nw.id
        FROM notification_webhooks nw
      WHERE nw.subscription_id = #{subscriptionId}
    |]
