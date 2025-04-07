-- Add some missing permissions for org owners.
UPDATE roles r
SET permissions = r.permissions || '{"org:edit", "project:create"}'
  WHERE r.ref = 'org_owner'::role_ref;
