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


-- All actions are scoped to a specific resource type, this is important since
-- permissable actions can be inherited via the resource hierarchy.
--
-- E.g. 'project:read' is a different permission than 'org:read'.
CREATE DOMAIN action AS TEXT
CHECK (
    VALUE ~ '^[a-zA-Z0-9_-]+:[a-zA-Z0-9_-]+$'
);

-- Create the new AuthZ tables we need

-- Create an enum for subject kinds.
CREATE TYPE subject_kind AS ENUM ('user', 'team', 'org');

CREATE TABLE subjects (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- This can be helpful for knowing which table to look for the originating row of a subject.
  kind subject_kind NOT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX subjects_kind ON subjects (kind) INCLUDE (id);

CREATE TYPE resource_kind AS ENUM ('project', 'org', 'team');

-- Resources are the things that can be accessed.
-- This table should be managed by database triggers to ensure it's always up to date.
CREATE TABLE resources (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- This can be helpful for knowing which table to look for the originating row of a resource.
  kind resource_kind NOT NULL,

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX resources_kind ON resources (kind) INCLUDE (id);

-- Enum for internal 'blessed' role refs.
CREATE TYPE role_ref AS ENUM (
  'org_viewer',
  'org_contributor',
  'org_admin',
  'org_owner',
  'org_default',
  'team_admin',
  'project_viewer',
  'project_contributor',
  'project_owner',
  'project_admin'
);

-- Roles are currently managed by Unison admins only, but we could add a resource for managing custom roles in the
-- future.
CREATE TABLE roles (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name TEXT NOT NULL,
  -- An internal identifier for the role, this is used to look up the role in the code.
  -- It must be NULL for user-created roles.
  ref role_ref UNIQUE NULL,
  -- The list of actions _may_ be empty, not that it's terribly useful
  actions action[] NOT NULL
);
CREATE INDEX roles_actions ON roles USING GIN (actions);

INSERT INTO roles (ref, name, actions)
  VALUES
    ('org_viewer'::role_ref,
     'Organization Viewer',
     ARRAY['org:view', 'team:view', 'project:view']
    ),
    ('org_contributor'::role_ref,
     'Organization Contributor',
     ARRAY['org:view', 'team:view', 'project:view', 'project:contribute']
    ),
    ('org_admin'::role_ref,
     'Organization Admin',
     ARRAY['org:view', 'org:manage', 'org:admin', 'org:create_project', 'team:view', 'team:manage', 'project:view', 'project:manage', 'project:contribute']
    ),
    ('org_owner'::role_ref,
     'Organization Owner',
     ARRAY['org:view', 'org:manage', 'org:admin', 'org:create_project', 'org:delete', 'org:change_owner', 'team:view', 'team:manage', 'project:view', 'project:manage', 'project:contribute']
    ),
    ('org_default'::role_ref,
     'Organization Default', -- The same as the contributor role, but keeping it separate allows us to see which orgs have diverged from the default or not.
     ARRAY['org:view', 'org:edit', 'team:view', 'project:view', 'project:create', 'project:contribute']
    ),
    ('team_admin'::role_ref,
     'Team Admin',
     ARRAY['team:view', 'team:manage']
    ),
    ('project_viewer'::role_ref,
     'Project Viewer',
     ARRAY['project:view']
     ),
    ('project_contributor'::role_ref,
     'Project Contributor',
     ARRAY['project:view', 'project:contribute']
    ),
    ('project_admin'::role_ref,
     'Project Admin',
     ARRAY['project:view', 'project:manage', 'project:contribute']
    ),
    ('project_owner'::role_ref,
     'Project Owner',
     ARRAY['project:view', 'project:manage', 'project:contribute', 'project:delete']
    );


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


-- Users
ALTER TABLE users ADD COLUMN subject_id UUID UNIQUE NULL REFERENCES subjects (id);

-- Backfill subjects for existing users.
DO $$
DECLARE
  user_id UUID;
  new_subject_id UUID;
BEGIN
  FOR user_id IN SELECT id FROM users WHERE subject_id IS NULL LOOP
    INSERT INTO subjects(kind)
      VALUES ('user')
      RETURNING id INTO new_subject_id;
    UPDATE users
      SET subject_id = new_subject_id
      WHERE id = user_id;
  END LOOP;
END;
$$;

-- Make the subject_id column NOT NULL now that we've backfilled it.
ALTER TABLE users ALTER COLUMN subject_id SET NOT NULL;

-- Trigger to create a subject for each new user.
CREATE FUNCTION trigger_create_user_subject()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a new subject for this user, returning the new subject_id.
  INSERT INTO subjects(kind)
    VALUES ('user')
    RETURNING id INTO NEW.subject_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER users_create_subject
  BEFORE INSERT ON users
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_user_subject();

-- New Org table

CREATE TABLE orgs (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- There's no subject_id on the org, since the org is a user, and the user has a subject_id.
  user_id UUID UNIQUE NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  -- Subject representing the org itself.
  -- Note that orgs also have a subject on their associated user, but since you can't log in as a subject that
  -- should be unused in practice.

  subject_id UUID UNIQUE NOT NULL REFERENCES subjects (id),
  -- Resource for managing permissions on this organization itself.
  resource_id UUID UNIQUE NOT NULL REFERENCES resources (id),

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Trigger to create an org resource when an org is created.
CREATE FUNCTION trigger_create_org_resource()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a subject for this org, assigning the subject_id to the new org.
  INSERT INTO subjects(kind)
    VALUES ('org')
    RETURNING id INTO NEW.subject_id;

  -- Insert a new resource for this org, assigning the resource_id to the new org.
  INSERT INTO resources(kind)
    VALUES ('org')
    RETURNING id INTO NEW.resource_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER orgs_create_resource
  BEFORE INSERT ON orgs
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_org_resource();

CREATE FUNCTION set_default_org_permissions()
RETURNS TRIGGER AS $$
DECLARE
  -- Get the default roles for an org.
  org_subject_id UUID;
  org_default_role_id UUID;
BEGIN
  SELECT org.subject_id INTO org_subject_id
    FROM orgs org
    WHERE org.id = NEW.id;
  SELECT role.id
    INTO org_default_role_id
    FROM roles role
    WHERE role.ref = 'org_default';
  -- Assign the default permissions for an org.
  INSERT INTO role_memberships(subject_id, resource_id, role_id)
    SELECT org_subject_id, NEW.resource_id, org_default_role_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER orgs_create_default_permissions
  AFTER INSERT ON orgs
  FOR EACH ROW
  EXECUTE FUNCTION set_default_org_permissions();

-- Backfill orgs from the org_members table, should cause the trigger we just created to fire as well.
INSERT INTO orgs(user_id)
  SELECT DISTINCT om.organization_user_id
  FROM org_members om;

-- Now add the organization id to the org_members table.
ALTER TABLE org_members ADD COLUMN org_id UUID NULL REFERENCES orgs (id) ON DELETE CASCADE;
CREATE INDEX org_members_org_id ON org_members (org_id) INCLUDE (member_user_id);

-- Fill it in
UPDATE org_members
  SET org_id = org.id
  FROM orgs org
  WHERE org_members.organization_user_id = org.user_id;

-- Make the org_id column NOT NULL now that we've backfilled it.
ALTER TABLE org_members ALTER COLUMN org_id SET NOT NULL;

-- New teams tables

CREATE TABLE teams (
  id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  org_id UUID NOT NULL REFERENCES orgs (id),

  subject_id UUID UNIQUE NOT NULL REFERENCES subjects (id),
  -- Resource for managing permissions on this team itself.
  resource_id UUID UNIQUE NOT NULL REFERENCES resources (id),

  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX teams_org_id ON teams (org_id);

-- Trigger to create a team resource and team subject when a team is created.
CREATE FUNCTION trigger_create_team_resource()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a new resource for this team, assigning the resource_id to the new team.
  INSERT INTO resources(kind)
    VALUES ('team')
    RETURNING id INTO NEW.resource_id;

  -- Insert a new subject for this team, assigning the subject_id to the new team.
  INSERT INTO subjects(kind)
    VALUES ('team')
    RETURNING id INTO NEW.subject_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER teams_create_resource
  BEFORE INSERT ON teams
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_team_resource();


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
CREATE FUNCTION trigger_create_project_resource()
RETURNS TRIGGER AS $$
BEGIN
  -- Insert a new resource for this project, returning the new resource_id.
  INSERT INTO resources(kind)
    VALUES ('project')
    RETURNING id INTO NEW.resource_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER projects_create_resource
  BEFORE INSERT ON projects
  FOR EACH ROW
  EXECUTE FUNCTION trigger_create_project_resource();

-- Trigger to assign the project owner role to the project owner.
CREATE FUNCTION trigger_assign_project_owner_role()
RETURNS TRIGGER AS $$
BEGIN
  -- Get the project owner's subject_id.
  INSERT INTO role_memberships(subject_id, resource_id, role_id)
    SELECT owner_user.subject_id, NEW.resource_id, role.id
    FROM projects
    JOIN users owner_user ON projects.owner_user_id = owner_user.id
    JOIN roles role ON role.ref = 'project_owner'
    WHERE projects.id = NEW.id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER projects_assign_owner_role
  AFTER INSERT ON projects
  FOR EACH ROW
  EXECUTE FUNCTION trigger_assign_project_owner_role();

-- Backfill resources for existing projects, this is the procedural form of the CTE expression above:
DO $$
DECLARE
  project_id UUID;
  new_resource_id UUID;
BEGIN
  FOR project_id IN SELECT id FROM projects WHERE resource_id IS NULL LOOP
    INSERT INTO resources(kind)
      VALUES ('project')
      RETURNING id INTO new_resource_id;
    UPDATE projects
      SET resource_id = new_resource_id
      WHERE projects.id = project_id;
  END LOOP;
END;
$$;

-- Backfill project owner roles for existing projects.
DO $$
BEGIN
  INSERT INTO role_memberships(subject_id, resource_id, role_id)
    SELECT owner_user.subject_id, projects.resource_id, role.id
    FROM projects
    JOIN users owner_user ON projects.owner_user_id = owner_user.id
    JOIN roles role ON role.ref = 'project_owner';
END;
$$;

-- Make the resource_id column NOT NULL now that we've backfilled it.
ALTER TABLE projects ALTER COLUMN resource_id SET NOT NULL;


-------------------------------------------------------------------------------

-- View for expressing auth group membership.
CREATE VIEW group_members(group_subject_id, member_subject_id) AS (
  -- Org groups
  SELECT org.subject_id, member.subject_id
  FROM orgs org
  JOIN org_members om ON om.org_id = org.id
  JOIN users member ON om.member_user_id = member.id
  UNION ALL
  -- Team groups
  SELECT team.subject_id, member.subject_id
  FROM teams team
  JOIN team_members tm ON tm.team_id = team.id
  JOIN users member ON tm.member_user_id = member.id
  -- We intentionally don't add teams as members of their orgs, since
  -- orgs may add users from outside of their org to a team.
);

-- View for expressing resources which have a hierarchy
--
-- NOTE: this is currently NOT resolved transitively.
CREATE VIEW resource_hierarchy(resource_id, parent_resource_id) AS (
  -- projects
  SELECT project.resource_id, org.resource_id
  FROM projects project
  JOIN orgs org ON project.owner_user_id = org.user_id
  UNION ALL
  -- teams
  SELECT team.resource_id, org.resource_id
  FROM teams team
  JOIN orgs org ON team.org_id = org.id
);

-- Get the subjects a user is considered a member of.
CREATE VIEW subjects_by_user(user_id, subject_id) AS (
  -- Get the user's own subject
  SELECT u.id, u.subject_id
  FROM users u
  -- Add in any groups the user is a member of
  UNION ALL
  SELECT u.id, gm.group_subject_id
  FROM users u
  JOIN group_members gm ON u.subject_id = gm.member_subject_id
);

CREATE VIEW subject_resource_permissions(subject_id, resource_id, action) AS (
  WITH base_permissions(subject_id, resource_id, action) AS (
    -- base permissions
    SELECT rm.subject_id, rm.resource_id, action
    FROM role_memberships rm
    JOIN roles r ON rm.role_id = r.id
    , UNNEST(r.actions) AS action
  ) SELECT * FROM base_permissions
  UNION
  -- Inherit permissions from parent resources
  SELECT bp.subject_id, rh.resource_id, bp.action
    FROM base_permissions bp
    JOIN resource_hierarchy rh ON bp.resource_id = rh.parent_resource_id
);

CREATE VIEW user_resource_permissions(user_id, resource_id, action) AS (
  SELECT sbu.user_id, srp.resource_id, action
  FROM subjects_by_user sbu
  JOIN subject_resource_permissions srp ON sbu.subject_id = srp.subject_id
);

CREATE FUNCTION user_has_permission(user_id UUID, resource_id UUID, action action)
RETURNS BOOLEAN
STABLE
PARALLEL SAFE
AS $$
  SELECT EXISTS (
    SELECT
    FROM user_resource_permissions
    WHERE user_id = $1 AND resource_id = $2 AND action = $3
  );
$$ LANGUAGE SQL;

CREATE VIEW subjects_by_kind(kind, subject_id, resolved_id) AS (
  -- Users
  SELECT 'user'::subject_kind, u.subject_id, u.id
  FROM users u
  UNION ALL
  -- Orgs
  SELECT 'org'::subject_kind, o.subject_id, o.id
  FROM orgs o
  UNION ALL
  -- Teams
  SELECT 'team'::subject_kind, t.subject_id, t.id
  FROM teams t
);

CREATE VIEW debug_permissions(subject_kind, subject_id, resolved_id, resource_kind, resource_id, role_id, role_ref, actions) AS (
  SELECT
    sbk.kind AS subject_kind,
    sbk.subject_id,
    sbk.resolved_id,
    r.kind AS resource_kind,
    rm.resource_id,
    rm.role_id,
    role.ref AS role_ref,
    role.actions
  FROM subjects_by_kind sbk
  JOIN role_memberships rm ON sbk.subject_id = rm.subject_id
  JOIN roles role ON rm.role_id = role.id
  JOIN resources r ON rm.resource_id = r.id
);
