-- Tweaks to webhooks now that they're largely stored in encrypted storage.

-- Remake this index without the 'url'
DROP INDEX notification_webhooks_by_user;
CREATE INDEX notification_webhooks_by_user ON notification_webhooks(subscriber_user_id);

-- The url column moved to encrypted storage in vault.
ALTER TABLE notification_webhooks
  DROP COLUMN url;


-- Update the trigger function so it notifies the webhooks workers if there's a new webhook to deliver.
CREATE OR REPLACE FUNCTION trigger_notification_event_subscriptions()
RETURNS TRIGGER AS $$
DECLARE
  the_subscription_id UUID;
  the_event_id UUID;
  the_subscriber UUID;
  rows_affected INTEGER;
BEGIN
  SELECT NEW.id INTO the_event_id;
  FOR the_subscription_id, the_subscriber IN
    (SELECT ns.id, ns.subscriber_user_id FROM notification_subscriptions ns
      WHERE ns.scope_user_id = NEW.scope_user_id
        AND NEW.topic = ANY(ns.topics)
        AND (ns.filter IS NULL OR NEW.data @> ns.filter)
        AND
        -- A subscriber can be notified if the event is in their scope or if they have permission to the resource.
        -- The latter is usually a superset of the former, but the former is trivial to compute so it can help
        -- performance to include it.
          (NEW.scope_user_id = ns.subscriber_user_id
            OR user_has_permission(ns.subscriber_user_id, NEW.resource_id, topic_permission(NEW.topic))
          )
    )
  LOOP
    -- Log that this event triggered this subscription.
    INSERT INTO notification_providence_log (event_id, subscription_id)
      VALUES (the_event_id, the_subscription_id);

    -- Add to the relevant queues.
    -- Each delivery method _may_ be triggered by multiple subscriptions,
    -- we need ON CONFLICT DO NOTHING.
    INSERT INTO notification_webhook_queue (event_id, webhook_id)
      SELECT the_event_id, nbw.webhook_id
      FROM notification_by_webhook nbw
      WHERE nbw.subscription_id = the_subscription_id
      ON CONFLICT DO NOTHING
      RETURNING webhook_id;

      -- Notify listeners on the channel IF any webhooks were added to the queue

      IF (SELECT COUNT(*) FROM notification_webhook_queue WHERE event_id = the_event_id) > 0 THEN
        PERFORM pg_notify('notification_webhook_queue', the_event_id::TEXT);
      END IF;

    INSERT INTO notification_email_queue (event_id, email_id)
      SELECT the_event_id AS event_id, nbe.email_id
      FROM notification_by_email nbe
      WHERE nbe.subscription_id = the_subscription_id
      ON CONFLICT DO NOTHING;

    -- If there are any new webhooks to process, trigger workers via LISTEN/NOTIFY
    GET DIAGNOSTICS rows_affected = ROW_COUNT;
    IF rows_affected > 0 THEN
        NOTIFY 'webhooks'
    END IF;

    -- Also add the notification to the hub.
    -- It's possible it was already added by another subscription for this user,
    -- in which case we just carry on.
    INSERT INTO notification_hub_entries (event_id, user_id)
      VALUES (the_event_id, the_subscriber)
      ON CONFLICT DO NOTHING;
  END LOOP;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
