-- Migration to add watch_project subscriptions for all existing users and projects.

INSERT INTO notification_subscriptions (subscriber_user_id, scope_user_id, topics, topic_groups, filter)
  SELECT u.id, 
         p.owner_user_id, 
         '{}', 
         ARRAY['watch_project'::notification_topic_group]::notification_topic_group[],
         jsonb_build_object('projectId', p.id)
    FROM users u
    JOIN projects p ON u.id = p.owner_user_id
  WHERE NOT EXISTS (
  SELECT FROM notification_subscriptions ns
    WHERE ns.subscriber_user_id = u.id
      AND ns.scope_user_id = p.owner_user_id
      AND ns.topics = '{}'
      AND ns.topic_groups = ARRAY['watch_project'::notification_topic_group]
      AND ns.filter = jsonb_build_object('projectId', p.id)
    )
;
