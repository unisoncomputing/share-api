CREATE TABLE contribution_timeline_comments (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  contribution_id UUID NOT NULL REFERENCES contributions(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  author_id UUID NOT NULL REFERENCES users(id) ON DELETE SET NULL,
  deleted_at TIMESTAMPTZ NULL
);

CREATE INDEX contribution_timeline_comments_by_contribution ON contribution_timeline_comments(contribution_id, created_at)
  WHERE deleted_at IS NULL;
CREATE INDEX contribution_timeline_comments_by_contribution_deleted ON contribution_timeline_comments(contribution_id, created_at)
  WHERE deleted_at IS NOT NULL;

CREATE TABLE contribution_timeline_comment_revisions (
    comment_id UUID NOT NULL REFERENCES contribution_timeline_comments(id) ON DELETE CASCADE,
    revision_number INTEGER NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- Author of the revision may not necessarily be the author of the comment
    author_id UUID NOT NULL REFERENCES users(id) ON DELETE SET NULL,
    content TEXT NOT NULL CHECK (length(content) < 1000000),

    PRIMARY KEY (comment_id, revision_number)
);

CREATE OR REPLACE FUNCTION update_contribution_updated_at_using_contribution_id()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE contributions
    SET updated_at = NEW.created_at
    WHERE id = NEW.contribution_id;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER contribution_updated_at_from_timeline_comments
AFTER INSERT ON contribution_timeline_comments
FOR EACH ROW
EXECUTE FUNCTION update_contribution_updated_at_using_contribution_id();
