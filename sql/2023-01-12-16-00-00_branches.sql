CREATE TABLE project_branches (
  id uuid DEFAULT uuid_generate_v4() PRIMARY KEY,

  -- The project this branch belongs to
  project_id uuid NOT NULL REFERENCES projects (id) ON DELETE CASCADE,

  name citext NOT NULL CHECK (name <> ''),

  -- Tracks who created this branch, may be useful for certain list views and queries
  -- We keep branches even if the creator was deleted, unless it's a contributor branch (see below)
  creator_id uuid NULL REFERENCES users (id) ON DELETE SET NULL,

  -- Contributor branches are deleted if their user is deleted.
  contributor_id uuid NULL REFERENCES users (id) ON DELETE CASCADE,

  -- The current HEAD of the branch.
  causal_hash text NOT NULL,

  -- Hint for where this branch was forked from or where to merge into.
  -- Note: this branch may belong to an entirely different project.
  merge_target_branch_id uuid NULL REFERENCES project_branches (id) ON DELETE SET NULL,

  created_at timestamptz NOT NULL DEFAULT NOW(),
  modified_at timestamptz NOT NULL DEFAULT NOW(),
  -- whether (and when) the branch was deleted
  deleted_at timestamptz NULL
);

-- Look up branches by name
-- No two living branches can have the same name within the same project
CREATE UNIQUE INDEX branches_by_project_id_and_name ON project_branches(project_id, name) WHERE deleted_at IS NULL AND contributor_id IS NULL;

-- No two living contributor branches by the same contributor can have the same name within the same project.
CREATE UNIQUE INDEX contributor_branches_by_contributor_id_and_project_id_and_name ON project_branches(contributor_id, project_id, name) WHERE deleted_at IS NULL AND contributor_id IS NOT NULL;

-- Allow looking up live branches by their creator
CREATE INDEX live_branches_by_creator_id ON project_branches(creator_id, created_at DESC) WHERE deleted_at IS NULL;

-- Allow looking up branches within a project sorted by their creation time.
-- This also includes contributor branches.
CREATE INDEX branches_by_project_id ON project_branches(project_id, created_at DESC) WHERE deleted_at IS NULL;

CREATE TABLE project_branch_reflog (
    id uuid DEFAULT uuid_generate_v4() PRIMARY KEY,

    -- The branch this reflog entry belongs to
    branch_id uuid NOT NULL REFERENCES project_branches (id) ON DELETE CASCADE,

    -- A NULL old_causal_hash occurs when a branch is first created.
    old_causal_hash text NULL,

    new_causal_hash text NOT NULL,

    -- The user who made the change
    user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,

    -- What caused the change (fast forward, merge, push, etc.)
    description text NULL,

    created_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE INDEX project_branch_reflog_by_branch_id ON project_branch_reflog(branch_id, created_at DESC);
CREATE INDEX project_branch_reflog_by_user_id ON project_branch_reflog(user_id, created_at DESC);
