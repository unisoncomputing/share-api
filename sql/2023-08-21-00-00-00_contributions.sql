CREATE TABLE contributions (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id uuid NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
  -- The numerical ID of the contribution within the project,
  -- can be used as a key when combined with a project id.
  contribution_number INTEGER NOT NULL,
  title TEXT NOT NULL CHECK (length(title) > 0 AND length(title) < 10000),
  description TEXT NULL CHECK (length(description) > 0 AND length(description) < 1000000),
  status TEXT NOT NULL,
  source_branch uuid  NOT NULL REFERENCES project_branches(id),
  target_branch uuid NOT NULL REFERENCES project_branches(id),
  -- contributions shouldn't merge branches into themselves
  CONSTRAINT not_self_referential CHECK (source_branch <> target_branch),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- Ideally this should be NULL, but in the case where we're required to delete 
  -- a user we probably don't also want to wipe out every contribution they've made.
  author_id uuid NULL REFERENCES users(id) ON DELETE SET NULL,

  contribution_text_document tsvector
    GENERATED ALWAYS AS (
        setweight(to_tsvector('english', title), 'A') ||
        setweight(to_tsvector('english', coalesce(description, '')), 'B')
    ) STORED
);

CREATE TRIGGER contributions_updated_at
  BEFORE UPDATE ON contributions
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

CREATE INDEX contributions_by_project ON contributions(project_id, updated_at DESC);
CREATE INDEX contributions_by_author ON contributions(author_id, updated_at DESC);
CREATE INDEX contributions_by_target_branch ON contributions(target_branch, updated_at DESC);
CREATE UNIQUE INDEX contributions_by_project_and_number ON contributions(project_id, contribution_number, updated_at DESC);

CREATE INDEX contribution_text_search ON contributions USING GIN(contribution_text_document);

CREATE TABLE contribution_status_events (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  contribution_id uuid NOT NULL REFERENCES contributions(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- The user who performed the status change
  actor uuid NULL REFERENCES users(id) ON DELETE SET NULL,
  new_status TEXT NOT NULL,
  -- null on contribution creation
  old_status TEXT NULL
);

CREATE INDEX contribution_status_events_by_contribution ON contribution_status_events(contribution_id, created_at ASC);
