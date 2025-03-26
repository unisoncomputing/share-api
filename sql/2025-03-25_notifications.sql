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
    data JSONB NOT NULL
);

CREATE INDEX notification_events_topic ON notification_events(topic, created_at DESC);

CREATE TABLE notification_subscriptions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    topic notification_topic NOT NULL,
);

