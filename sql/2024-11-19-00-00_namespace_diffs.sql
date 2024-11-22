-- Adds tables for storing pre-computed namespace diffs

CREATE TABLE namespace_diffs (
    left_namespace_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    right_namespace_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,

    -- Since different codebases can have different variable names and such we also need to sandbox diffs by codebase owner
    left_codebase_owner_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    right_codebase_owner_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    diff JSONB NOT NULL,

    PRIMARY KEY (left_namespace_id, right_namespace_id, left_codebase_owner_user_id, right_codebase_owner_user_id)
);


-- New table for coordinating background job for pre-computing diffs

-- Table of all contributions which have been updated and may need their diffs re-computed
CREATE TABLE contribution_diff_queue (
  contribution_id UUID PRIMARY KEY REFERENCES contributions(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  PRIMARY KEY (contribution_id)
);
