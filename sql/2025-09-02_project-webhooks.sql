-- This migration codifies the project ID filter on notification subscriptions into a real field rather than a 
-- generic JSONB filter. You can still set it to NULL for org-wide subscriptions.

ALTER TABLE notification_subscriptions
  -- The project which this filter is scopped to.
  -- If provided, the project_id must be belong to the scope_user_id's user/org.
  ADD COLUMN project_id UUID NULL REFERENCES projects(id) ON DELETE CASCADE;

CREATE INDEX notification_subscriptions_by_user_and_project ON notification_subscriptions(subscriber_user_id, project_id, created_at DESC);

ALTER TABLE notification_events 
  ADD COLUMN project_id UUID NULL REFERENCES projects(id) ON DELETE CASCADE;

CREATE INDEX notification_events_scope_user_and_project ON notification_events(scope_user_id, project_id, occurred_at DESC);

-- Migrate existing filters to the new column, and also remove 
-- the projectId from the JSONB filter.
UPDATE notification_subscriptions
  SET project_id = (filter->>'projectId')::UUID,
      filter = filter - 'projectId'
  WHERE filter ? 'projectId';

UPDATE notification_events
  SET project_id = (data->>'projectId')::UUID
  WHERE data ? 'projectId';

-- Now update the trigger so we use the new projectid columns
CREATE FUNCTION trigger_notification_event_subscriptions()
RETURNS TRIGGER AS $$
DECLARE
  the_subscription_id UUID;
  the_event_id UUID;
  the_subscriber UUID;
BEGIN
  SELECT NEW.id INTO the_event_id;
  FOR the_subscription_id, the_subscriber IN
    (SELECT ns.id, ns.subscriber_user_id FROM notification_subscriptions ns
      WHERE ns.scope_user_id = NEW.scope_user_id
        AND NEW.topic = ANY(ns.topics)
        AND (ns.project_id IS NULL OR ns.project_id = NEW.project_id)
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
      ON CONFLICT DO NOTHING;

    INSERT INTO notification_email_queue (event_id, email_id)
      SELECT the_event_id AS event_id, nbe.email_id
      FROM notification_by_email nbe
      WHERE nbe.subscription_id = the_subscription_id
      ON CONFLICT DO NOTHING;

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
