ALTER TABLE users
  -- Add is_org column
  ADD COLUMN is_org boolean NOT NULL DEFAULT false,
  -- Add check that primary_email is not null unless is_org is true
  ADD CONSTRAINT primary_email_not_null_unless_org CHECK (
    is_org OR primary_email IS NOT NULL
  ),
  -- Make primary_email nullable
  ALTER COLUMN primary_email DROP NOT NULL;

-- Update the is_org column for existing users
WITH org_users(user_id) AS (
  SELECT DISTINCT o.user_id
  FROM orgs o
) UPDATE users
  SET is_org = true
  WHERE id IN (SELECT ou.user_id FROM org_users ou);

-- Add a 'creator_user_id' to orgs just to track where they came from.
-- This is distinct from the owners in the auth roles.
ALTER TABLE orgs
  ADD COLUMN creator_user_id uuid NULL REFERENCES users (id) ON DELETE SET NULL
;

ALTER TABLE orgs
  ADD COLUMN is_commercial boolean NOT NULL DEFAULT false
;
