-- Used when direct-importing into codebases during sync.
CREATE INDEX project_releases_by_squashed_causal_hash_and_created_at ON project_releases (causal_hash_squashed ASC, created_at DESC);
CREATE INDEX project_branches_by_causal_hash_and_created_at ON project_branches (causal_hash ASC, created_at DESC);
