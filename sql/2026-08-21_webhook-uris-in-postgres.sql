-- Webhook URIs used to live in Vault; the original 'url' column was dropped in
-- 2025-04-28_webhooks.sql when they moved there. We're migrating off of Vault, so the URI comes
-- back onto the webhook itself.
--
-- The column starts out nullable because the existing URIs still have to be copied out of Vault.
-- Once that backfill has run, a follow-up migration should set it NOT NULL.
ALTER TABLE notification_webhooks
  ADD COLUMN uri TEXT NULL CONSTRAINT notification_webhooks_uri_not_empty CHECK (uri <> '');
