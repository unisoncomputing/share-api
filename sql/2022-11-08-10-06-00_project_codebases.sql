CREATE EXTENSION IF NOT EXISTS citext;

CREATE TABLE projects (
  id uuid NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
  -- User or org who owns the project
  owner_user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  -- How the project is referenced by humans and in URLs e.g. 'base'
  slug citext NOT NULL CHECK (length(slug) > 0),
  -- Human readable name
  display_name text NULL CHECK (length(display_name) > 0),
  -- Human readable description
  summary text NULL CHECK (length(summary) > 0),
  -- Tags for filtering projects
  tags citext[] NOT NULL DEFAULT '{}',
  private boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT NOW()
);

-- Assert only one project per slug per user
CREATE UNIQUE INDEX project_user_slug ON projects(owner_user_id, slug);
CREATE INDEX project_tags ON projects USING GIN(tags);
CREATE INDEX project_owner_slug ON projects(owner_user_id, slug);
CREATE INDEX project_slug ON projects(slug);
