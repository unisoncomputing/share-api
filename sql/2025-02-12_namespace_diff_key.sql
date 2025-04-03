-- Recreate namespace diffs table, since we're changing a foreign key reference
-- Could truncate and alter columns instead, but it's more work

DROP TABLE namespace_diffs;

CREATE TABLE namespace_diffs (
    left_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
    right_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,

    -- Since different codebases can have different variable names and such we also need to sandbox diffs by codebase owner
    left_codebase_owner_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    right_codebase_owner_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,

    diff JSONB NOT NULL,

    PRIMARY KEY (left_causal_id, right_causal_id, left_codebase_owner_user_id, right_codebase_owner_user_id)
);
