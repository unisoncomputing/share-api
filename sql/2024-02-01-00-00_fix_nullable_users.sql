ALTER TABLE loose_code_reflog
  ALTER COLUMN editor_user_id DROP NOT NULL;

ALTER TABLE comments
  ALTER COLUMN author_id DROP NOT NULL;

ALTER TABLE comment_revisions
  ALTER COLUMN author_id DROP NOT NULL;
