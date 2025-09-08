-- This changes webhooks such that each webhook and email delivery method is assigned to exactly one subscription, rather than the
-- previous many-to-many relationship.

-- Add a new topic group which represents _all_ activity in a project.
ALTER TYPE notification_topic_group ADD VALUE IF NOT EXISTS 'all_project_topics';

INSERT INTO notification_topic_group_topics (topic_group, topic)
  VALUES ('all_project_topics', 'project:branch:updated'),
         ('all_project_topics', 'project:contribution:created'),
         ('all_project_topics', 'project:contribution:updated'),
         ('all_project_topics', 'project:contribution:comment'),
         ('all_project_topics', 'project:ticket:created'),
         ('all_project_topics', 'project:ticket:updated'),
         ('all_project_topics', 'project:ticket:comment'),
         ('all_project_topics', 'project:release:created');

-- WEBHOOKS
ALTER TABLE notification_webhooks
  ADD COLUMN subscription_id UUID NULL REFERENCES notification_subscriptions(id) ON DELETE CASCADE;

CREATE INDEX notification_webhooks_by_subscription ON notification_webhooks(subscription_id);

UPDATE notification_webhooks nw
  SET subscription_id = (SELECT nbw.subscription_id FROM notification_by_webhook nbw WHERE nbw.webhook_id = nw.id LIMIT 1);

ALTER TABLE notification_webhooks
  ALTER COLUMN subscription_id SET NOT NULL;

-- EMAILS
ALTER TABLE notification_emails
  ADD COLUMN subscription_id UUID NULL REFERENCES notification_subscriptions(id) ON DELETE CASCADE;

CREATE INDEX notification_emails_by_subscription ON notification_emails(subscription_id);

UPDATE notification_emails ne
  SET subscription_id = (SELECT nbe.subscription_id FROM notification_by_email nbe WHERE nbe.email_id = ne.id LIMIT 1);


ALTER TABLE notification_emails
  ALTER COLUMN subscription_id SET NOT NULL;



-- This migration codifies the project ID filter on notification subscriptions into a real field rather than a
-- generic JSONB filter. You can still set it to NULL for org-wide subscriptions.
ALTER TABLE notification_subscriptions
  -- The project which this filter is scoped to.
  -- If provided, the project_id must be belong to the scope_user_id's user/org, or match the subscriber_project_id.
  ADD COLUMN scope_project_id UUID NULL REFERENCES projects(id) ON DELETE CASCADE,
  -- A subscription can belong to a project itself.
  ADD COLUMN subscriber_project_id UUID NULL REFERENCES projects(id) ON DELETE CASCADE,
  -- Project subscriptions won't have a subscriber_user_id, just a subscriber_project_id.
  -- So allow subscriber_user_id to be nullable
  ALTER COLUMN subscriber_user_id DROP NOT NULL,
  -- Add a constraint that either subscriber_user_id or subscriber_project_id must be set, but not both.
  ADD CONSTRAINT notification_subscriptions_user_or_project CHECK (
    (subscriber_user_id IS NOT NULL AND subscriber_project_id IS NULL)
    OR (subscriber_user_id IS NULL AND subscriber_project_id IS NOT NULL)
  ),
  -- Add a constraint that if the subscriber is a project, the scope_project_id must match the subscriber_project_id.
  ADD CONSTRAINT notification_subscriptions_scope_project_matches CHECK (
    (subscriber_project_id IS NULL)
    OR (scope_project_id IS NOT DISTINCT FROM subscriber_project_id)
  );

-- Index to find for a user scoped to a specific project.
CREATE INDEX notification_subscriptions_by_user_and_project ON notification_subscriptions(subscriber_user_id, scope_project_id, created_at DESC);

ALTER TABLE notification_events
  ADD COLUMN project_id UUID NULL REFERENCES projects(id) ON DELETE CASCADE;

CREATE INDEX notification_events_scope_user_and_project ON notification_events(scope_user_id, project_id, occurred_at DESC);

-- There may be some existing subscriptions and events with a projectId for a project which no longer exists.
-- It's safe to delete them.
DELETE FROM notification_subscriptions
  WHERE filter ? 'projectId'
    AND NOT EXISTS (SELECT FROM projects p WHERE p.id = regexp_replace(filter->>'projectId', '^P-', '')::UUID)
  ;

DELETE FROM notification_events
  WHERE data ? 'projectId'
    AND NOT EXISTS (SELECT FROM projects p WHERE p.id = regexp_replace(data->>'projectId', '^P-', '')::UUID)
  ;

-- Migrate existing filters to the new column, and also remove
-- the projectId from the JSONB filter.
UPDATE notification_subscriptions
  SET scope_project_id = regexp_replace(filter->>'projectId', '^P-', '')::UUID,
      filter = filter - 'projectId'
  WHERE filter ? 'projectId'
    AND EXISTS (SELECT FROM projects p WHERE p.id = regexp_replace(filter->>'projectId', '^P-', '')::UUID)
  ;

UPDATE notification_events
  SET project_id = regexp_replace(data->>'projectId', '^P-', '')::UUID
  WHERE data ? 'projectId'
  AND EXISTS (SELECT FROM projects p WHERE p.id = regexp_replace(data->>'projectId', '^P-', '')::UUID);

-- Rework the trigger to use the new topic groups.
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
        AND ( NEW.topic = ANY(ns.topics)
          OR EXISTS(
              SELECT FROM notification_topic_group_topics ntgt
              WHERE ntgt.topic_group = ANY(ns.topic_groups)
                AND ntgt.topic = NEW.topic
             )
        )
        AND (ns.filter IS NULL OR NEW.data @> ns.filter)
        AND
        -- A subscriber can be notified if the event is in their scope or if they have permission to the resource, 
        -- or if the subscription is owned by the project which the event is scoped to.
        -- The latter is usually a superset of the former, but the former is trivial to compute so it can help
        -- performance to include it.
          (NEW.scope_user_id = ns.subscriber_user_id
            OR user_has_permission(ns.subscriber_user_id, NEW.resource_id, topic_permission(NEW.topic))
            OR (ns.subscriber_project_id IS NOT NULL AND ns.subscriber_project_id = NEW.project_id)
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
      SELECT the_event_id, nw.id
      FROM notification_webhooks nw
      WHERE nw.subscription_id = the_subscription_id
      ON CONFLICT DO NOTHING;

      -- If there are any new webhooks to process, trigger workers via LISTEN/NOTIFY
      GET DIAGNOSTICS rows_affected = ROW_COUNT;
      IF rows_affected > 0 THEN
        NOTIFY webhooks;
      END IF;

    INSERT INTO notification_email_queue (event_id, email_id)
      SELECT the_event_id AS event_id, ne.id
      FROM notification_emails ne
      WHERE ne.subscription_id = the_subscription_id
      ON CONFLICT DO NOTHING;

    -- If there are any new webhooks to process, trigger workers via LISTEN/NOTIFY
    GET DIAGNOSTICS rows_affected = ROW_COUNT;
    IF rows_affected > 0 THEN
        NOTIFY emails;
    END IF;

    IF the_subscriber IS NOT NULL THEN
      -- Also add the notification to the hub.
      -- It's possible it was already added by another subscription for this user,
      -- in which case we just carry on.
      INSERT INTO notification_hub_entries (event_id, user_id)
        VALUES (the_event_id, the_subscriber)
        ON CONFLICT DO NOTHING;
    END IF;
  END LOOP;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


ALTER TABLE notification_webhooks
  DROP COLUMN subscriber_user_id;

ALTER TABLE notification_emails
  DROP COLUMN subscriber_user_id;

-- Now we can drop the old tables
DROP TABLE notification_by_webhook;
DROP TABLE notification_by_email;
