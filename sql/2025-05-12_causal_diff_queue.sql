-- Table for causal diffs we want to compute.
-- Also keyed by the codebase owner of each side of the diff since 
-- the sandboxed terms may affect how the diff looks.
CREATE TABLE causal_diff_queue (
  from_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
  to_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
  from_codebase_owner UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  to_codebase_owner UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  PRIMARY KEY (from_causal_id, to_causal_id, from_codebase_owner, to_codebase_owner)
);
