-- Org membership is now associated with a specific role within the org, this simplifies things, 
-- makes the data more consistent,  no need to rely on triggers, and makes it much easier to display in the UI.

ALTER TABLE org_members
    ADD COLUMN role_id UUID REFERENCES roles(id) NULL;

-- set all existing org members to be maintiners
UPDATE org_members 
  SET role_id = (SELECT id FROM roles WHERE name = 'org_maintainer' LIMIT 1);

ALTER TABLE org_members
    ALTER COLUMN role_id SET NOT NULL;

-- Split out view containing all the direct subject<->resource permissions.
--
-- This view expands the roles into their individual permissions
-- but does not consider resource hierarchy or group memberships
CREATE OR REPLACE VIEW direct_resource_permissions(subject_id, resource_id, permission) AS (
    SELECT rm.subject_id, rm.resource_id, permission
    FROM role_memberships rm
    JOIN roles r ON rm.role_id = r.id
    , UNNEST(r.permissions) AS permission
  UNION
    -- Include permissions from org membership roles
    SELECT u.subject_id, org.resource_id, permission
      FROM org_members om
      JOIN roles r ON om.role_id = r.id
      JOIN orgs org ON om.org_id = org.id
      , UNNEST(r.permissions) AS permission
  UNION
  -- Include public resource permissions
  SELECT NULL, prp.resource_id, permission
    FROM public_resource_permissions prp
)


-- This view builds on top of direct_resource_permissions to include inherited permissions
CREATE OR REPLACE VIEW subject_resource_permissions(subject_id, resource_id, permission) AS (
  SELECT drp.subject_id, drp.resource_id, drp.permission 
    FROM direct_resource_permissions drp
  UNION
  -- Inherit permissions from parent resources
  SELECT drp.subject_id, drp.resource_id, bp.permission
    FROM direct_resource_permissions drp
    JOIN resource_hierarchy rh ON bp.resource_id = rh.parent_resource_id
);

