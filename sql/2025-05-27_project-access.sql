-- Function to check whether caller has access to a given project permission
-- If the user_id is NULL, it returns TRUE for public projects
CREATE FUNCTION user_has_project_permission(
  user_id UUID,
  project_id UUID,
  permission permission
) 
RETURNS BOOLEAN
STABLE
PARALLEL SAFE
AS $$
  SELECT EXISTS (
    SELECT
    FROM user_resource_permissions urp
    JOIN projects p ON urp.resource_id = p.resource_id
    WHERE (urp.user_id IS NULL OR urp.user_id = $1)
      AND p.id = $2
      AND urp.permission = $3
  );
$$ LANGUAGE SQL;

-- DEPLOY NEW APP CODE HERE

-- Remove old project access management system
-- We need to replace these with the new permissions system.
DROP VIEW accessible_private_projects;
DROP TABLE project_maintainers;

