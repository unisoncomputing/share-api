-- The groundwork for a generalized notification system, which can be used to notify users of various events.
--
-- The basis of the system is that:
--   users have *subscriptions*,
--   which apply filters to *topics*,
--   which contain *events*,
--   that then generate *notifications*,
--   which are sent via *delivery methods*

-- E.g.
-- A user *subscribes* to receive *emails* for the "New contributions" *topic* with a filter for the `foo` project.
--
-- Later, A user creates a contribution on that project,
-- That *event* is handled by all relevant *subscriptions*,
-- Which generates a *notification* for the user,
-- which is then sent via the *email* *delivery method* and marked completed.
--
-- A note on permissions:
-- We allow creating all kinds of notification subscriptions, even for things the calling user
-- doesn't have access to, but the notification system will only actually create notifications if the caller has access to
-- the resource of a given event for the permission associated to that topic via the
-- 'topic_permission' SQL function.
--
-- This means that if the permissions associated to a given resource change, the notification system will correctly
-- adapt which notifications its sending over time. And we avoid the nebulous task of determining which possible
-- resources any given subscription _might_ be associated with. This allows subscriptions to be more general as well,
-- since they can be wide-sweeping wildcard subscriptions which are not constrained to a specific resource.

CREATE TYPE notification_topic AS ENUM (
    'project:branch:updated',
    'project:contribution:created'
);

-- Returns the list of permissions a user must have for an event's resource in order to be notified.
CREATE FUNCTION topic_permission(topic notification_topic)
RETURNS permission 
PARALLEL SAFE
IMMUTABLE
AS $$
BEGIN
  CASE topic
    WHEN 'project:branch:updated' THEN 
      RETURN 'project:view'::permission;
    WHEN 'project:contribution:created' THEN 
      RETURN 'project:view'::permission;
    ELSE
      RAISE EXCEPTION 'topic_permissions: topic % must declare its necessary permissions', topic;
  END CASE;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE notification_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    occurred_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- The resource associated with the event, to check if subscribers have permission to be notified.
    resource_id UUID NOT NULL REFERENCES resources(id) ON DELETE CASCADE,

    topic notification_topic NOT NULL,
    -- The effective scope of this event. The user_id of the relevant user or org.
    scope_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    data JSONB NOT NULL
);

CREATE INDEX notification_events_topic ON notification_events(topic, occurred_at DESC);
CREATE INDEX notification_events_scope_user ON notification_events(scope_user_id, occurred_at DESC);

CREATE TABLE notification_subscriptions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- user_id of the subscriber.
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The scope of this subscription.
    scope_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    -- The topics this subscription is for.
    topics notification_topic[] NOT NULL CHECK (array_length(topics, 1) > 0),
    -- Any additional filtering for this subscription, e.g. which projects we care about, etc.
    -- Specified as an object with key-value pairs which must ALL be present on the event in order to trigger
    -- the notification.
    filter JSONB NULL
);

CREATE TRIGGER notification_subscriptions_updated_at
  BEFORE UPDATE ON notification_subscriptions
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

-- GIN index for finding subscriptions by topic
CREATE INDEX notification_subscriptions_by_topic ON notification_subscriptions USING GIN (topics, scope_user_id);

CREATE INDEX notification_subscriptions_by_user ON notification_subscriptions(subscriber_user_id, created_at DESC);

-- Which notifications were triggered by which subscription for each event.
CREATE TABLE notification_providence_log (
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  subscription_id UUID REFERENCES notification_subscriptions(id) ON DELETE CASCADE,
  PRIMARY KEY (event_id, subscription_id)
);

CREATE TABLE notification_webhooks (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Who owns (and can edit) this delivery method
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The URL to send the webhook to.
    url TEXT NOT NULL CHECK (url <> '')
);

CREATE INDEX notification_webhooks_by_user ON notification_webhooks(subscriber_user_id, url);

CREATE TRIGGER notification_webhooks_updated_at
  BEFORE UPDATE ON notification_webhooks
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

CREATE TABLE notification_by_webhook (
  subscription_id UUID REFERENCES notification_subscriptions(id) ON DELETE CASCADE,
  webhook_id UUID REFERENCES notification_webhooks(id) ON DELETE CASCADE,
  PRIMARY KEY (subscription_id, webhook_id)
);

CREATE INDEX notification_by_webhook_by_webhook_id ON notification_by_webhook(webhook_id);

CREATE TABLE notification_emails (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    -- Who owns (and can edit) this delivery method
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The email address to send the email to.
    email TEXT NOT NULL CHECK (email <> ''),

    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX notification_emails_by_user ON notification_emails(subscriber_user_id, email);

CREATE TRIGGER notification_emails_updated_at
  BEFORE UPDATE ON notification_emails
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

CREATE TABLE notification_by_email (
  subscription_id UUID REFERENCES notification_subscriptions(id) ON DELETE CASCADE,
  email_id UUID REFERENCES notification_emails(id) ON DELETE CASCADE,
  PRIMARY KEY (subscription_id, email_id)
);

CREATE INDEX notification_by_email_by_email_id ON notification_by_email(email_id);

CREATE TABLE notification_webhook_queue (
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  webhook_id UUID REFERENCES notification_webhooks(id) ON DELETE CASCADE,
  delivery_attempts_remaining INTEGER NOT NULL DEFAULT 3,
  delivered BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY (event_id, webhook_id)
);

-- Allow efficiently grabbing the oldest undelivered webhooks we're still trying to deliver.
CREATE INDEX notification_webhook_queue_undelivered ON notification_webhook_queue(created_at ASC)
  WHERE NOT delivered AND delivery_attempts_remaining > 0;

CREATE TABLE notification_email_queue (
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  email_id UUID REFERENCES notification_emails(id) ON DELETE CASCADE,
  delivery_attempts_remaining INTEGER NOT NULL DEFAULT 3,
  delivered BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY (event_id, email_id)
);

-- Allow efficiently grabbing the oldest undelivered emails we're still trying to deliver.
CREATE INDEX notification_email_queue_undelivered ON notification_email_queue(created_at ASC)
  WHERE NOT delivered AND delivery_attempts_remaining > 0;

CREATE TYPE notification_status AS ENUM (
    'unread',
    'read',
    'archived'
);

CREATE TABLE notification_hub_entries (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  status notification_status NOT NULL DEFAULT 'unread',

  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE UNIQUE INDEX notification_hub_entries_event_user ON notification_hub_entries(user_id, event_id);
CREATE INDEX notification_hub_entries_by_user_chronological ON notification_hub_entries(user_id, created_at DESC)
  WHERE status <> 'archived';

CREATE TRIGGER notification_hub_entries_updated_at
  BEFORE UPDATE ON notification_hub_entries
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

-- Add a trigger to automatically add to notification queues for relevant subscriptions.
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

CREATE TRIGGER notification_event_subscriptions
  AFTER INSERT ON notification_events
  FOR EACH ROW
  EXECUTE FUNCTION trigger_notification_event_subscriptions();



-- Add new permissions to existing roles
UPDATE roles r
SET permissions = r.permissions || '{"notifications_hub_entries:view", "notifications_hub_entries:update", "notification_delivery_methods:create", "notification_delivery_methods:update", "notification_subscriptions:create", "notification_subscriptions:update"}'
  WHERE r.ref IN ('org_admin'::role_ref, 'org_owner'::role_ref, 'org_default'::role_ref, 'org_maintainer'::role_ref);
