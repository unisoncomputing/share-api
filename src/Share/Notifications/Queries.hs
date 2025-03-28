module Share.Notifications.Queries
  ( recordEvent,
    listNotificationHubEntries,
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
