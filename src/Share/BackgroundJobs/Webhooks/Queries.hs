{-# LANGUAGE ApplicativeDo #-}

-- | This module contains queries related to sending notification webhooks.
module Share.BackgroundJobs.Webhooks.Queries
  ( queueWebhook,
    getUnsentWebhook,
    recordFailedDeliveryAttempt,
    markWebhookAsDelivered,
  )
where

import Share.IDs
import Share.Postgres
import Share.Postgres.Notifications qualified as Notif

queueWebhook :: (QueryM m) => NotificationEventId -> NotificationWebhookId -> m ()
queueWebhook eventId webhookId = do
  execute_
    [sql|
    INSERT INTO notification_webhook_queue (event_id, webhook_id)
    VALUES (#{eventId}, #{webhookId})
    |]
  Notif.notifyChannel Notif.WebhooksChannel

-- | Claim the oldest unsent webhook with some delivery attempts left.
getUnsentWebhook :: (QueryA m) => m (Maybe (NotificationEventId, NotificationWebhookId))
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
