module Share.Notifications.Queries
  ( recordEvent,
    listNotificationHubEntries,
    updateNotificationHubEntry,
    listNotificationDeliveryMethods,
    listEmailDeliveryMethods,
    listWebhookDeliveryMethods,
    createEmailDeliveryMethod,
    createWebhookDeliveryMethod,
    deleteEmailDeliveryMethod,
    deleteWebhookDeliveryMethod,
    updateEmailDeliveryMethod,
    updateWebhookDeliveryMethod,
    listNotificationSubscriptions,
    createNotificationSubscription,
    deleteNotificationSubscription,
    updateNotificationSubscription,
    getNotificationSubscription,
  )
where

import Data.Foldable qualified as Foldable
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Data.Set.NonEmpty (NESet)
import Data.Time (UTCTime)
import Share.IDs
import Share.Notifications.Types
import Share.Postgres
import Share.Utils.URI (URIParam)

recordEvent :: (QueryA m) => NewNotificationEvent -> m ()
recordEvent (NotificationEvent {eventScope, eventData}) = do
  execute_
    [sql|
      INSERT INTO notification_events (topic, scope_user_id, data)
      VALUES (#{eventTopic eventData}, #{eventScope}, #{eventData})
    |]

listNotificationHubEntries :: (QueryA m) => UserId -> Maybe Int -> Maybe UTCTime -> Maybe (NESet NotificationStatus) -> m [NotificationHubEntry]
listNotificationHubEntries notificationUserId mayLimit afterTime statusFilter = do
  let limit = clamp (0, 1000) . fromIntegral @Int @Int32 . fromMaybe 50 $ mayLimit
  let statusFilterList = Foldable.toList <$> statusFilter
  queryListRows
    [sql|
      SELECT hub.id, hub.status, event.id, event.occurred_at, event.scope_user_id, event.topic, event.data
        FROM notification_hub_entries hub
        JOIN notification_events event ON hub.event_id = event.id
      WHERE hub.user_id = #{notificationUserId}
            AND (#{statusFilterList} IS NULL OR hub.status = ANY(#{statusFilterList}::notification_status[]))
            AND (#{afterTime} IS NULL OR event.occurred_at > #{afterTime})
      ORDER BY hub.created_at DESC
      LIMIT #{limit}
    |]

updateNotificationHubEntry :: (QueryA m) => NotificationHubEntryId -> NotificationStatus -> m ()
updateNotificationHubEntry hubEntryId status = do
  execute_
    [sql|
      UPDATE notification_hub_entries
      SET status = #{status}
      WHERE id = #{hubEntryId}
    |]

listNotificationDeliveryMethods :: UserId -> Transaction e [NotificationDeliveryMethod]
listNotificationDeliveryMethods userId = pipelined do
  emailDeliveryMethods <- listEmailDeliveryMethods userId
  webhookDeliveryMethods <- listWebhookDeliveryMethods userId
  pure $ (EmailDeliveryMethod <$> emailDeliveryMethods) <> (WebhookDeliveryMethod <$> webhookDeliveryMethods)

listEmailDeliveryMethods :: (QueryA m) => UserId -> m [NotificationEmailDeliveryConfig]
listEmailDeliveryMethods userId = do
  queryListRows
    [sql|
      SELECT ne.id, ne.email
        FROM notification_emails ne
      WHERE ne.subscriber_user_id = #{userId}
      ORDER BY ne.email
    |]

listWebhookDeliveryMethods :: (QueryA m) => UserId -> m [NotificationWebhookDeliveryConfig]
listWebhookDeliveryMethods userId = do
  queryListRows
    [sql|
      SELECT nw.id, nw.url
        FROM notification_webhooks nw
      WHERE nw.subscriber_user_id = #{userId}
      ORDER BY nw.url
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

createWebhookDeliveryMethod :: UserId -> URIParam -> Transaction e NotificationWebhookId
createWebhookDeliveryMethod userId url = do
  existingWebhookDeliveryMethodId <-
    query1Col
      [sql|
      SELECT id
        FROM notification_webhooks
      WHERE subscriber_user_id = #{userId}
            AND url = #{url}
    |]
  case existingWebhookDeliveryMethodId of
    Just webhookDeliveryMethodId -> pure webhookDeliveryMethodId
    Nothing -> do
      queryExpect1Col
        [sql|
          INSERT INTO notification_webhooks (subscriber_user_id, url)
          VALUES (#{userId}, #{url})
          RETURNING id
        |]

updateWebhookDeliveryMethod :: UserId -> NotificationWebhookId -> URIParam -> Transaction e ()
updateWebhookDeliveryMethod notificationUser webhookDeliveryMethodId url = do
  execute_
    [sql|
      UPDATE notification_webhooks
      SET url = #{url}
      WHERE id = #{webhookDeliveryMethodId}
        AND subscriber_user_id = #{notificationUser}
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
listNotificationSubscriptions notificationUserId = do
  queryListRows
    [sql|
      SELECT ns.id, ns.scope_user_id, ns.topics, ns.filter
        FROM notification_subscriptions ns
      WHERE ns.subscriber_user_id = #{notificationUserId}
      ORDER BY ns.created_at DESC
    |]

createNotificationSubscription :: UserId -> UserId -> NESet NotificationTopic -> Maybe SubscriptionFilter -> Transaction e NotificationSubscriptionId
createNotificationSubscription notificationUserId subscriptionScope subscriptionTopics subscriptionFilter = do
  queryExpect1Col
    [sql|
      INSERT INTO notification_subscriptions (subscriber_user_id, scope_user_id, topics, filter)
      VALUES (#{notificationUserId}, #{subscriptionScope}, #{Foldable.toList subscriptionTopics}, #{subscriptionFilter})
      RETURNING id
    |]

deleteNotificationSubscription :: UserId -> NotificationSubscriptionId -> Transaction e ()
deleteNotificationSubscription notificationUserId subscriptionId = do
  execute_
    [sql|
      DELETE FROM notification_subscriptions
      WHERE id = #{subscriptionId}
        AND subscriber_user_id = #{notificationUserId}
    |]

updateNotificationSubscription :: UserId -> NotificationSubscriptionId -> Maybe (NESet NotificationTopic) -> Maybe SubscriptionFilter -> Transaction e ()
updateNotificationSubscription notificationUserId subscriptionId subscriptionTopics subscriptionFilter = do
  execute_
    [sql|
      UPDATE notification_subscriptions
      SET topics = COALESCE(#{Foldable.toList <$> subscriptionTopics}, topics),
          filter = COALESCE(#{subscriptionFilter}, filter)
      WHERE id = #{subscriptionId}
        AND subscriber_user_id = #{notificationUserId}
    |]

getNotificationSubscription :: UserId -> NotificationSubscriptionId -> Transaction e (NotificationSubscription NotificationSubscriptionId)
getNotificationSubscription notificationUserId subscriptionId = do
  queryExpect1Row
    [sql|
      SELECT ns.id, ns.scope_user_id, ns.topics, ns.filter
        FROM notification_subscriptions ns
      WHERE ns.id = #{subscriptionId}
        AND ns.subscriber_user_id = #{notificationUserId}
    |]
