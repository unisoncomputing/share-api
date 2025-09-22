-- Some resources grant permissions publicly regardless of user.
CREATE OR REPLACE VIEW public_resource_permissions(resource_id, permission) AS (
  SELECT p.resource_id, permission
  FROM projects p
  , roles r
  , UNNEST(r.permissions) AS permission
  WHERE NOT p.private
        AND r.ref = 'project_public_access'
);


CREATE OR REPLACE VIEW subject_resource_permissions(subject_id, resource_id, permission) AS (
  WITH base_permissions(subject_id, resource_id, permission) AS (
    -- base permissions
    SELECT rm.subject_id, rm.resource_id, permission
    FROM role_memberships rm
    JOIN roles r ON rm.role_id = r.id
    , UNNEST(r.permissions) AS permission
  ) SELECT * FROM base_permissions
  UNION
  -- Inherit permissions from parent resources
  SELECT bp.subject_id, rh.resource_id, bp.permission
    FROM base_permissions bp
    JOIN resource_hierarchy rh ON bp.resource_id = rh.parent_resource_id
  UNION
  -- Include public resource permissions
  SELECT NULL, prp.resource_id, permission
    FROM public_resource_permissions prp
);

CREATE OR REPLACE VIEW user_resource_permissions(user_id, resource_id, permission) AS (
  SELECT sbu.user_id, srp.resource_id, permission
  FROM subjects_by_user sbu
  JOIN subject_resource_permissions srp 
    ON sbu.subject_id = srp.subject_id
  UNION
  -- Include public resource permissions explicitly, 
  -- since the above joins on subject_id which is NULL for public perms
  SELECT NULL, prp.resource_id, permission
    FROM public_resource_permissions prp
);

-- work this table into the permissions system
CREATE OR REPLACE FUNCTION user_has_permission(user_id UUID, resource_id UUID, permission permission)
RETURNS BOOLEAN
STABLE
PARALLEL SAFE
AS $$
  SELECT EXISTS (
    SELECT
    FROM user_resource_permissions urp
    WHERE (urp.user_id IS NULL OR urp.user_id = $1) AND urp.resource_id = $2 AND urp.permission = $3
  );
$$ LANGUAGE SQL;
