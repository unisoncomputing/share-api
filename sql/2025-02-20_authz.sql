-- Adds a new somewhat generic Authorization (authz) system to the database.
-- This system is designed to be pretty extensible, allowing new resource or subject kinds to be added, and new permissions
-- to be added to roles, etc.
--
-- Glossary of the terms involved:
--
-- * User: A standard Share user. Currently organizations are represented in the system as users and this is unlikely
--     to change any time soon.
-- * Organization: An organization can own many projects and contain many members and teams.
--     They serve primarily as a management tool, and can set default permissions and settings for all their projects
--     and members. The management of an organization is handled by an "organization" resource.
-- * Team: A team is a group of users, teams can be assigned roles which may grant them permissions to resources. Teams
--     are owned by an organization, but don't inherit the organization's default permissions.
--
-- We map these domain terms onto the following permissions concepts:
--
-- * subject: A subject is anything which may have permission to do something, e.g. a user, a team, a service account,
--     an organization, etc. It is the base unit to which roles are assigned.
-- * group: A generic term for a collection of subjects and/or other groups. Permissions may be allocated to _groups_ instead of
--     _subjects_ directly to make it easier to manage permissions for large numbers of subjects and simplify the mental
--     model. Group membership can ADD new permissions to a subject, but it cannot REMOVE permissions from a subject.
--     The default permissions of an organization are inherited by their users using groups for example.
-- * resource: The object/thing that is being accessed by the subject. This could be a project, search, code, etc.
-- * action/permission: The verb that the subject is trying to perform on the resource. This could be read, write, delete, etc.
-- * role: A collection of permissions that can be assigned to a subject x resource pair.


-- Create a domain type with a CHECK constraint
CREATE DOMAIN action AS TEXT
CHECK (
    VALUE ~ '^[a-zA-Z0-9_-]+:[a-zA-Z0-9_-]+$'
);

-- Create the new AuthZ tables we need

CREATE TABLE subjects (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Resources are the things that can be accessed.
-- This table should be managed by database triggers to ensure it's always up to date.
CREATE TABLE resources (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- Currently each resource may have zero or one parent resource.
  -- We could break this out to a join table if we ever need multiple parents.
  parent_resource_id UUID NULL REFERENCES resources (id)
    ON DELETE CASCADE
    CHECK (id <> parent_resource_id),

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
);
CREATE INDEX resources_parent_resource_id ON resources (parent_resource_id) INCLUDE (id);

-- Assign (subject, resources) pairs actions via roles.
CREATE TABLE role_memberships (
  subject_id UUID NOT NULL REFERENCES subjects (id) ON DELETE CASCADE,
  resource_id UUID NOT NULL REFERENCES resources (id) ON DELETE CASCADE,
  role_id UUID NOT NULL REFERENCES roles (id) ON DELETE CASCADE,

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  -- Allows looking up all roles assigned to a subject.
  PRIMARY KEY (subject_id, resource_id, role_id)
);

-- Allow looking up roles assigned to a resource.
CREATE INDEX role_memberships_resource_id ON role_memberships (resource_id) INCLUDE (subject_id, role_id);

-- Roles are currently managed by Unison admins only, but we could add a resource for managing custom roles in the
-- future.
CREATE TABLE roles (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- The list of actions _may_ be empty, not that it's terribly useful
  actions action[] NOT NULL
);

-- GIN index over actions using text_pattern_ops to allow for prefix searches.
CREATE INDEX roles_actions ON roles USING GIN (actions text_pattern_ops);

-- New Org table

CREATE TABLE orgs (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- There's no subject_id on the org, since the org is a user, and the user has a subject_id.
  org_user_id UUID UNIQUE NOT NULL REFERENCES users (id),
  -- Resource for managing permissions on this organization itself.
  resource_id UUID UNIQUE NOT NULL REFERENCES resources (id)

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Trigger to create an org resource when an org is created.
CREATE FUNCTION trigger_create_org()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a new resource for this org, assigning the resource_id to the new org.
  INSERT INTO resources DEFAULT VALUES
    RETURNING resources.id INTO NEW.resource_id;
  RETURN NEW;
END;

CREATE TRIGGER orgs_create_resource
  BEFORE INSERT ON orgs
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_org();

-- Backfill orgs from the org_members table, should cause the trigger we just created to fire as well.
INSERT INTO orgs(org_user_id) 
  SELECT DISTINCT om.organization_user_id 
  FROM org_members om
  ON CONFLICT DO NOTHING;

-- New teams tables

CREATE TABLE teams (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  org_id UUID NOT NULL REFERENCES orgs (id),

  subject_id UUID UNIQUE NOT NULL REFERENCES subjects (id),
  -- Resource for managing permissions on this team itself.
  resource_id UUID UNIQUE NOT NULL REFERENCES resources (id)

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX teams_org_id ON teams (org_id);

-- Trigger to create a team resource and team subject when a team is created.
CREATE FUNCTION trigger_create_team()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a new resource for this team, assigning the resource_id to the new team.
  INSERT INTO resources(parent_resource_id)
    SELECT org.resource_id
    FROM orgs org
    WHERE org.id = NEW.org_id
    RETURNING resources.id INTO NEW.resource_id;

  -- Insert a new subject for this team, assigning the subject_id to the new team.
  INSERT INTO subjects DEFAULT VALUES
    RETURNING subjects.id INTO NEW.subject_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER teams_create_resource
  BEFORE INSERT ON teams
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_team();


CREATE TABLE team_members (
  team_id UUID NOT NULL REFERENCES teams (id),
  member_user_id UUID NOT NULL REFERENCES users (id),

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  PRIMARY KEY (team_id, member_user_id)
);
CREATE INDEX team_members_member_user_id ON team_members (member_user_id) INCLUDE (team_id);

-- Projects

-- Add the new project resource column
ALTER TABLE projects ADD COLUMN resource_id UUID UNIQUE NULL REFERENCES resources (id);

-- Add a trigger to create a resource for each project.
CREATE FUNCTION create_project()
DECLARE
  the_parent_resource UUID;
RETURNS TRIGGER AS $$
BEGIN
  -- Note: this will be NULL for non-org projects, which is fine :)
  SELECT org.resource_id INTO the_parent_resource
    FROM orgs org
      WHERE org.org_user_id = NEW.owner_user_id;

  -- Insert a new resource for this project, returning the new resource_id.
  INSERT INTO resources(parent_resource_id)
    VALUES (the_parent_resource)
    RETURNING resources.id INTO NEW.resource_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER projects_create_resource
  BEFORE INSERT ON projects
  FOR EACH ROW
  EXECUTE FUNCTION create_project();

-- Backfill resources for existing projects, this is the procedural form of the CTE expression above:
DO $$
DECLARE
  project_id UUID;
  new_resource_id UUID;
BEGIN
  FOR project_id IN SELECT id FROM projects WHERE resource_id IS NULL LOOP
    INSERT INTO resources(parent_resource_id)
      SELECT org.resource_id
      FROM projects p
      LEFT JOIN orgs org ON org.org_user_id = p.owner_user_id
      WHERE p.id = project_id
      RETURNING resources.id INTO new_resource_id;
    UPDATE projects SET resource_id = new_resource_id WHERE projects.id = project_id;
  END LOOP;
END;


-- Make the resource_id column NOT NULL now that we've backfilled it.
ALTER TABLE projects ALTER COLUMN resource_id SET NOT NULL;

-- Users
ALTER TABLE users ADD COLUMN subject_id UUID UNIQUE NULL REFERENCES subjects (id);

-- Backfill subjects for existing users.
DO $$
DECLARE
  user_id UUID;
  new_subject_id bigint;
BEGIN
  FOR user_id IN SELECT id FROM users WHERE subject_id IS NULL LOOP
    INSERT INTO subjects DEFAULT VALUES RETURNING id INTO new_subject_id;
    UPDATE users SET subject_id = new_subject_id WHERE id = user_id;
  END LOOP;
END;
$$;

-- Make the subject_id column NOT NULL now that we've backfilled it.
ALTER TABLE users ALTER COLUMN subject_id SET NOT NULL;

-------------------------------------------------------------------------------

-- View for expressing auth group membership.
CREATE VIEW group_members(group_subject_id, member_subject_id) AS (
  -- Org groups
  SELECT org_user.subject_id, member.subject_id
  FROM orgs org
  JOIN users org_user ON org.org_user_id = org_user.id
  JOIN org_members om ON om.organization_user_id = org_user.id
  JOIN users member ON om.member_user_id = member.id
  UNION
  -- Team groups
  SELECT team.subject_id, member.subject_id
  FROM teams team
  JOIN team_members tm ON tm.team_id = team.id
  JOIN users member ON tm.member_user_id = member.id
  -- We intentionally don't add teams as members of their orgs, since
  -- orgs may add users from outside of their org to a team.
);

-- Get the subjects a user is considered a member of.
CREATE VIEW subjects_by_user(user_id UUID, subject_id UUID) AS (
  -- Get the user's own subject
  SELECT u.id, u.subject_id
  FROM users u
  -- Add in any groups the user is a member of
  UNION
  SELECT u.id, gm.group_subject_id
  FROM users u
  JOIN group_members gm ON u.subject_id = gm.member_subject_id
);

CREATE VIEW user_resource_permissions(user_id, resource_id, action) AS (
  SELECT sbu.user_id, rm.resource_id, action
  FROM subjects_by_user sbu
  JOIN role_memberships rm ON sbu.subject_id = rm.subject_id
  JOIN roles r ON rm.role_id = r.id
  , UNNEST(r.actions) AS action
);

CREATE FUNCTION user_has_permission(user_id UUID, resource_id UUID, action action)
LANGUAGE SQL
RETURNS BOOLEAN
STABLE
STRICT
PARALLEL SAFE
AS $$
  SELECT EXISTS (
    SELECT
    FROM user_resource_permissions
    WHERE user_id = $1 AND resource_id = $2 AND action = $3
  );
$$;
