ALTER TABLE notification_events
  ADD COLUMN actor_user_id UUID NULL REFERENCES users(id) ON DELETE SET NULL
;
