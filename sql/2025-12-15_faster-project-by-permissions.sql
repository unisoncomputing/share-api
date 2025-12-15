-- The previous user_has_project_permission function is called on _every_ project when doing global omnisearch,
-- which is too slow.

-- Create a view which serves as join table for finding projects for which the user has a given permission, it's much faster
-- than running a permission check for every private project when we need to discover the list of all projects a user
-- has access to.
-- 
-- This special-cases the 'project:view' permission to be even faster, it's the most common case.
CREATE FUNCTION projects_by_user_permission(arg_user_id UUID, arg_permission permission)
-- Returns a subset of the projects table
RETURNS SETOF projects AS $$
  -- Get all public projects and projects owned by the user,
  -- as well as all public projects.
  SELECT p.*
    FROM projects p
    WHERE p.owner_user_id = arg_user_id 
      OR (arg_permission = 'project:view' AND NOT p.private)
UNION
  SELECT
    p.*
    FROM org_members om
      JOIN projects p 
        ON om.organization_user_id = p.owner_user_id
      JOIN roles r ON om.role_id = r.id
      WHERE om.member_user_id = arg_user_id
        AND arg_permission = ANY(r.permissions)
        -- All public projects are already included above if the permission is 'project:view'
        AND (p.private OR arg_permission <> 'project:view')
  UNION
  -- Include projects the user is a direct maintainer of
  SELECT
    p.*
    FROM users u
      JOIN role_memberships rm ON u.subject_id = rm.subject_id
      JOIN roles r ON rm.role_id = r.id
      JOIN projects p ON rm.resource_id = p.resource_id
      WHERE u.id = arg_user_id
        AND arg_permission = ANY(r.permissions)
        -- All public projects are already included above if the permission is 'project:view'
        AND (p.private OR arg_permission <> 'project:view')
$$ LANGUAGE sql STABLE PARALLEL SAFE;

-- A better index for this query.
CREATE INDEX idx_projects_by_owner_and_privacy
  ON projects (private, owner_user_id);
