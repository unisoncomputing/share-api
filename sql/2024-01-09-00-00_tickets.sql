CREATE TYPE ticket_status AS ENUM ('open', 'closed');

CREATE TABLE tickets (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  project_id uuid NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
  -- The numerical ID of the ticket within the project,
  -- can be used as a key when combined with a project id.
  ticket_number INTEGER NOT NULL,
  title TEXT NOT NULL CHECK (length(title) > 0 AND length(title) < 10000),
  description TEXT NULL CHECK (length(description) > 0 AND length(description) < 1000000),
  status ticket_status NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- Ideally this shouldn't be NULL, but in the case where we're required to delete
  -- a user we probably don't also want to wipe out every ticket they've made.
  author_id uuid NULL REFERENCES users(id) ON DELETE SET NULL,

  ticket_text_document tsvector
    GENERATED ALWAYS AS (
        setweight(to_tsvector('english', title), 'A') ||
        setweight(to_tsvector('english', coalesce(description, '')), 'B')
    ) STORED
);

-- Allow easily filtering tickets by status.
CREATE INDEX tickets_by_project_and_status ON tickets(project_id, status, updated_at DESC);

CREATE TRIGGER tickets_updated_at
  BEFORE UPDATE ON tickets
  FOR EACH ROW
  EXECUTE PROCEDURE moddatetime (updated_at);

CREATE INDEX tickets_by_project ON tickets(project_id, updated_at DESC);
CREATE INDEX tickets_by_author ON tickets(author_id, updated_at DESC);
CREATE UNIQUE INDEX tickets_by_project_and_number ON tickets(project_id, ticket_number, updated_at DESC);

CREATE INDEX ticket_text_search ON tickets USING GIN(ticket_text_document);

CREATE TABLE ticket_status_events (
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  ticket_id uuid NOT NULL REFERENCES tickets(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- The user who performed the status change
  actor uuid NULL REFERENCES users(id) ON DELETE SET NULL,
  new_status ticket_status NOT NULL,
  -- null on ticket creation
  old_status ticket_status NULL
);

CREATE INDEX ticket_status_events_by_ticket_id ON ticket_status_events(ticket_id, created_at ASC);

-- Generalize comments from JUST contributions to work for tickets as well.
ALTER TABLE contribution_timeline_comments RENAME TO comments;

ALTER TABLE contribution_timeline_comment_revisions RENAME TO comment_revisions;

DROP TRIGGER contribution_updated_at_from_timeline_comments ON comments;

DROP FUNCTION update_contribution_updated_at_using_contribution_id;


-- Each comment can be on either a contribution or a ticket, but not both.
ALTER TABLE comments alter contribution_id DROP NOT NULL,
  ADD COLUMN ticket_id UUID NULL REFERENCES tickets(id) ON DELETE CASCADE,
  ADD CONSTRAINT foreign_reference CHECK (
    (contribution_id IS NOT NULL AND ticket_id IS NULL) OR
    (contribution_id IS NULL AND ticket_id IS NOT NULL)
  );

DROP INDEX contribution_timeline_comments_by_contribution;
DROP INDEX contribution_timeline_comments_by_contribution_deleted;

CREATE INDEX comments_by_contribution ON comments(contribution_id, created_at)
  WHERE deleted_at IS NULL
  AND contribution_id IS NOT NULL;
CREATE INDEX comments_by_contribution_deleted ON comments(contribution_id, created_at)
  WHERE deleted_at IS NOT NULL
  AND contribution_id IS NOT NULL;

CREATE INDEX comments_by_ticket ON comments(ticket_id, created_at)
  WHERE deleted_at IS NULL
  AND ticket_id IS NOT NULL;
CREATE INDEX comments_by_ticket_deleted ON comments(ticket_id, created_at)
  WHERE deleted_at IS NOT NULL
  AND ticket_id IS NOT NULL;


CREATE OR REPLACE FUNCTION set_updated_at_from_comment()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.contribution_id IS NOT NULL THEN
        UPDATE contributions
        SET updated_at = NEW.created_at
        WHERE id = NEW.contribution_id;
    ELSIF NEW.ticket_id IS NOT NULL THEN
        UPDATE tickets
        SET updated_at = NEW.created_at
        WHERE id = NEW.ticket_id;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER set_updated_at_from_comments_trigger
AFTER INSERT ON comments
FOR EACH ROW
EXECUTE FUNCTION set_updated_at_from_comment();
