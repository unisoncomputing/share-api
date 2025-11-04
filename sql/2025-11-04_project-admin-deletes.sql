-- Adds the 'project:delete" permission to project admins.

UPDATE roles
  SET permissions = array_append(permissions, 'project:delete'::permission)
  WHERE ref = 'project_admin'
  -- Unless they already have it
  AND NOT permissions @> (ARRAY['project:delete']::permission[])
;
