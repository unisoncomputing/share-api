-- Change on delete behaviour of project branch reflog to SET NULL
-- so we don't lose the reflog entries when a user is deleted
ALTER TABLE project_branch_reflog
  ALTER COLUMN user_id DROP NOT NULL
, DROP CONSTRAINT project_branch_reflog_user_id_fkey
, ADD CONSTRAINT project_branch_reflog_user_id_fkey
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE SET NULL;
