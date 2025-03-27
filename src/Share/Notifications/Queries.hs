module Share.Notifications.Queries
  ( recordEvent,
  )
where

import Share.Notifications.Types (NewNotificationEvent)
import Share.Postgres

recordEvent :: (QueryA m) => NewNotificationEvent -> m ()
recordEvent (NotificationEvent {eventData}) = do
  execute_
    [sql|

    |]

subscriptionsForEvent :: (QueryA m) => NewNotificationEvent -> m [Subscription]
subscriptionsForEvent (NotificationEvent {}) = do
  queryListCol
    [sql| 

    |]
