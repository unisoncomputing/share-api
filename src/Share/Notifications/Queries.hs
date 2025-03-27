module Share.Notifications.Queries
  ( recordEvent,
  )
where

import Share.Notifications.Types
import Share.Postgres

recordEvent :: (QueryA m) => NewNotificationEvent -> m ()
recordEvent (NotificationEvent {eventScope, eventData}) = do
  execute_
    [sql|
      INSERT INTO notification_events (topic, scope_user_id, data)
      VALUES (#{eventTopic eventData}, #{eventScope}, #{eventData})
    |]
