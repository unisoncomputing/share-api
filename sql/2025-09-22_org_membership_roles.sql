-- Org membership is now associated with a specific role within the org, this simplifies things,
-- makes the data more consistent,  no need to rely on triggers, and makes it much easier to display in the UI.

-- Update the role permissions, some roles had an incorrect 'org:create_project' rather than 'project:create' permission
-- UPDATE roles
--   SET permissions = ARRAY['org:view', 'org:manage', 'org:admin', 'team:view', 'team:manage', 'project:view', 'project:create', 'project:manage', 'project:contribute', 'project:delete', 'project:maintain']
--   WHERE ref = 'org_admin'
--   ;

-- UPDATE roles
--   SET permissions = ARRAY['org:view', 'org:manage', 'org:admin', 'org:delete', 'org:change_owner', 'team:view', 'team:manage', 'project:view', 'project:create', 'project:manage', 'project:contribute', 'project:delete', 'project:maintain']
--   WHERE ref = 'org_owner'
--   ;

-- -- After migrating, this will no longer be used.
-- UPDATE roles
--   SET permissions = ARRAY['org:view', 'org:edit', 'team:view', 'project:view', 'project:create', 'project:maintain', 'project:contribute']
--   WHERE ref = 'org_default'
--   ;

ALTER TABLE org_members
    ADD COLUMN role_id UUID REFERENCES roles(id) NULL;

-- set all existing org members to be maintiners
UPDATE org_members
  SET role_id = (SELECT id FROM roles WHERE ref = 'org_maintainer' LIMIT 1);

-- Elevate the current org owners to have the org_owner role
UPDATE org_members
  SET role_id = (SELECT id FROM roles WHERE ref = 'org_owner' LIMIT 1)
  WHERE EXISTS (
    SELECT
    FROM orgs org
    JOIN users u ON org_members.member_user_id = u.id
    JOIN role_memberships rm ON rm.subject_id = u.subject_id AND rm.resource_id = org.resource_id
    JOIN roles r ON rm.role_id = r.id
    WHERE org.id = org_members.org_id
      AND r.ref = 'org_owner'
  );

ALTER TABLE org_members
    ALTER COLUMN role_id SET NOT NULL;

-- Now add a check that each org always has an owner.
CREATE OR REPLACE FUNCTION check_orgs_have_an_owner()
RETURNS trigger AS $$
BEGIN
    IF NOT EXISTS (
      SELECT
        FROM org_members om
          WHERE om.org_id = OLD.org_id
            AND om.role_id = (SELECT id FROM roles WHERE ref = 'org_owner' LIMIT 1)
    ) THEN
        RAISE EXCEPTION 'Each organization must have at least one owner.';
    END IF;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_org_owners_trigger
    AFTER UPDATE OR DELETE ON org_members
    FOR EACH ROW
    EXECUTE FUNCTION check_orgs_have_an_owner();

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
      JOIN users u ON om.member_user_id = u.id
      JOIN roles r ON om.role_id = r.id
      JOIN orgs org ON om.org_id = org.id
      , UNNEST(r.permissions) AS permission
  UNION
  -- Include public resource permissions
  SELECT NULL, prp.resource_id, permission
    FROM public_resource_permissions prp
);


-- This view builds on top of direct_resource_permissions to include inherited permissions
CREATE OR REPLACE VIEW subject_resource_permissions(subject_id, resource_id, permission) AS (
  SELECT drp.subject_id, drp.resource_id, drp.permission
    FROM direct_resource_permissions drp
  UNION
  -- Inherit permissions from parent resources
  SELECT drp.subject_id, rh.resource_id, drp.permission
    FROM direct_resource_permissions drp
    JOIN resource_hierarchy rh ON drp.resource_id = rh.parent_resource_id
);


DELETE FROM role_memberships rm
  USING roles r
  WHERE
    rm.role_id = r.id
    AND r.ref::text IN ('org_viewer', 'org_maintainer', 'org_contributor', 'org_admin', 'org_owner', 'org_default');
