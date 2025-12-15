-- The previous user_has_project_permission function is called on _every_ project when doing global omnisearch,
-- which is too slow.

-- Create a view which serves as join table for finding projects we should consider in search, it's much faster
-- than running a permission check for every private project.
CREATE FUNCTION projects_searchable_by_user(arg_user_id UUID)
-- Returns a subset of the projects table
RETURNS SETOF projects AS $$
  -- Get all public projects and projects owned by the user,
  -- as well as all public projects.
  SELECT p.*
    FROM projects p
    WHERE p.owner_user_id = arg_user_id 
      OR NOT p.private
UNION
  SELECT
    p.*
    FROM org_members om
      JOIN projects p 
        ON om.organization_user_id = p.owner_user_id
      JOIN roles r ON om.role_id = r.id
      WHERE om.member_user_id = arg_user_id
        AND 'project:view' = ANY(r.permissions)
        -- All public projects are already included above
        AND p.private
  UNION
  -- Include projects the user is a direct maintainer of
  SELECT
    p.*
    FROM users u
      JOIN role_memberships rm ON u.subject_id = rm.subject_id
      JOIN roles r ON rm.role_id = r.id
      JOIN projects p ON rm.resource_id = p.resource_id
      WHERE u.id = arg_user_id
        AND 'project:view' = ANY(r.permissions)
        AND p.private;
$$ LANGUAGE sql STABLE PARALLEL SAFE;

-- A better index for this query.
-- CREATE INDEX idx_projects_by_owner_and_privacy
--   ON projects (private, owner_user_id);

CREATE INDEX idx_public_projects_by_owner
  ON projects (owner_user_id)
  WHERE NOT private;

CREATE INDEX idx_private_projects_by_owner
  ON projects (owner_user_id)
  WHERE private;
