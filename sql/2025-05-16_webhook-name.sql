-- Add a name to the webhooks table
ALTER TABLE notification_webhooks
  ADD COLUMN name TEXT NULL CONSTRAINT notification_webhooks_name_not_empty CHECK (name <> ''::text)
;

UPDATE notification_webhooks
  SET name = 'Webhook'
  WHERE name IS NULL;

ALTER TABLE notification_webhooks
  ALTER COLUMN name SET NOT NULL
;
