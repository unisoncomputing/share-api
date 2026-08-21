-- Webhook URIs used to live in Vault (see 2025-04-28_webhooks.sql, which dropped the original
-- 'url' column). We're migrating off of Vault, so they move back into Postgres.
--
-- The URI lives in its own table rather than back on 'notification_webhooks' so that the
-- sensitive part of a webhook's config is easy to audit and so the common queries over
-- webhooks don't select it by accident.
--
-- NOTE: this migration only creates the table; the existing URIs still need to be copied out of
-- Vault and inserted here.
CREATE TABLE notification_webhook_uris (
    -- Each webhook has exactly one URI, so we can key the table on the webhook itself.
    webhook_id UUID PRIMARY KEY NOT NULL REFERENCES notification_webhooks(id) ON DELETE CASCADE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),

    -- The URL to send the webhook to.
    uri TEXT NOT NULL CONSTRAINT notification_webhook_uris_uri_not_empty CHECK (uri <> '')
);

CREATE TRIGGER notification_webhook_uris_updated_at
  BEFORE UPDATE ON notification_webhook_uris
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);
