-- Adds tables for storing pre-computed namespace diffs

CREATE TABLE namespace_diffs (
    left_namespace_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    right_namespace_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,

    -- Since different codebases can have different variable names and such we also need to sandbox diffs by codebase owner
    left_codebase_owner_user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    right_codebase_owner_user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE

    diff JSONB NOT NULL,

    PRIMARY KEY (left_namespace_id, right_namespace_id, left_codebase_owner_user_id, right_codebase_owner_user_id)
);
