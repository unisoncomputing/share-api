CREATE TYPE notification_topic_group AS ENUM (
  'watch_project'
);

-- Mapping for which topics are in each group.
CREATE TABLE notification_topic_group_topics (
  topic_group notification_topic_group NOT NULL,
  topic notification_topic NOT NULL,
  PRIMARY KEY (topic_group, topic)
);

CREATE INDEX idx_notification_topic_group_topics_topic ON notification_topic_group_topics (topic) INCLUDE (topic_group);

INSERT INTO notification_topic_group_topics (topic_group, topic)
VALUES
  ('watch_project', 'project:contribution:created')
;

-- Allow subscribing to a group of notifications, which may change over time.
ALTER TABLE notification_subscriptions
  ADD COLUMN topic_groups notification_topic_group[] NOT NULL DEFAULT '{}';


-- Rework the trigger to use the new topic groups.
CREATE OR REPLACE FUNCTION trigger_notification_event_subscriptions()
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
        -- Check whether any subscription is subscribed to the topic directly or through a topic group.
        AND ( NEW.topic = ANY(ns.topics)
          OR EXISTS(
              SELECT FROM notification_topic_group_topics ntgt
              WHERE ntgt.topic_group = ANY(ns.topic_groups)
                AND ntgt.topic = NEW.topic
             )
        )
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
