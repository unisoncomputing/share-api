module Share.Notifications.Queries
  ( recordEvent,
    listNotificationHubEntries,
    updateNotificationHubEntry,
    listNotificationDeliveryMethods,
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

createEmailDeliveryMethod :: UserId -> Text -> Transaction e NotificationEmailDeliveryMethodId
