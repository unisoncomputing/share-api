ALTER TYPE notification_topic ADD VALUE 'project:contribution:updated';
ALTER TYPE notification_topic ADD VALUE 'project:ticket:created';
ALTER TYPE notification_topic ADD VALUE 'project:ticket:updated';
ALTER TYPE notification_topic ADD VALUE 'project:contribution:comment';
ALTER TYPE notification_topic ADD VALUE 'project:ticket:comment';

INSERT INTO notification_topic_group_topics (topic_group, topic)
VALUES
  ('watch_project', 'project:contribution:updated'),
  ('watch_project', 'project:ticket:created'),
  ('watch_project', 'project:ticket:updated'),
  ('watch_project', 'project:contribution:comment'),
  ('watch_project', 'project:ticket:comment')
;


-- Returns the permission a user must have for an event's resource in order to be notified.
CREATE OR REPLACE FUNCTION topic_permission(topic notification_topic)
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
    WHEN 'project:contribution:updated' THEN
      RETURN 'project:view'::permission;
    WHEN 'project:contribution:comment' THEN
      RETURN 'project:view'::permission;
    WHEN 'project:ticket:created' THEN
      RETURN 'project:view'::permission;
    WHEN 'project:ticket:updated' THEN
      RETURN 'project:view'::permission;
    WHEN 'project:ticket:comment' THEN
      RETURN 'project:view'::permission;
    ELSE
      RAISE EXCEPTION 'topic_permissions: topic % must declare its necessary permissions', topic;
  END CASE;
END;
$$ LANGUAGE plpgsql;

