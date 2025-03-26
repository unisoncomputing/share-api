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

CREATE TYPE notification_topic AS ENUM (
    'contribution:created',
    'branch:updated'
);

CREATE TABLE notification_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    topic notification_topic NOT NULL,
    -- The effective scope of this event. The user_id of the relevant user or org.
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    data JSONB NOT NULL
);

CREATE INDEX notification_events_topic ON notification_events(topic, created_at DESC);

CREATE TABLE notification_subscriptions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- user_id of the subscriber.
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The scope of this subscription.
    scope_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    -- The topic this subscription is for.
    topic notification_topic NOT NULL,
    -- Any additional filtering for this subscription, e.g. which projects we care about, etc.
    -- Specified as an object with key-value pairs which must ALL be present on the event in order to trigger
    -- the notification.
    filter JSONB NOT NULL
);

CREATE INDEX notification_subscriptions_topic ON notification_subscriptions(topic, user_id_scope);

CREATE TABLE notification_webhooks (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    -- Who owns (and can edit) this delivery method
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The URL to send the webhook to.
    url TEXT NOT NULL CHECK (url <> '')
);

CREATE TABLE notification_by_webhook (
  subscription_id UUID REFERENCES notification_subscriptions(id) ON DELETE CASCADE,
  webhook_id UUID REFERENCES notification_webhooks(id) ON DELETE CASCADE,
  PRIMARY KEY (subscription_id, webhook_id)
);

CREATE TABLE notification_emails (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    -- Who owns (and can edit) this delivery method
    subscriber_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    -- The email address to send the email to.
    email TEXT NOT NULL CHECK (email <> '')
);

CREATE TABLE notification_by_email (
  subscription_id UUID REFERENCES notification_subscriptions(id) ON DELETE CASCADE,
  email_id UUID REFERENCES notification_email_delivery(id) ON DELETE CASCADE,
  PRIMARY KEY (subscription_id, email_id)
);

CREATE TABLE notification_webhook_queue (
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  webhook_id UUID REFERENCES notification_webhooks(id) ON DELETE CASCADE,
  delivery_attempts_remaining INTEGER NOT NULL DEFAULT 3,
  delivered BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY (event_id, webhook_id)
);

-- Allow efficiently grabbing the oldest undelivered webhooks we're still trying to deliver.
CREATE INDEX notification_webhook_queue_delivered
  WHERE delivered = FALSE AND delivery_attempts_remaining > 0
  ORDER BY created_at ASC;


CREATE TABLE notification_email_queue (
  event_id UUID REFERENCES notification_events(id) ON DELETE CASCADE,
  email_id UUID REFERENCES notification_emails(id) ON DELETE CASCADE,
  delivery_attempts INTEGER NOT NULL DEFAULT 3,
  delivered BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY (event_id, email_id)
);

-- Allow efficiently grabbing the oldest undelivered emails we're still trying to deliver.
CREATE INDEX notification_email_queue_delivered
  WHERE delivered = FALSE AND delivery_attempts_remaining > 0
  ORDER BY created_at ASC;

