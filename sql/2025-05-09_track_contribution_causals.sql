-- Track the causals on the contribution itself; this is useful because the branches may change _after_ the
-- contribution is merged or closed.
ALTER TABLE contributions 
  ADD COLUMN source_causal_id INTEGER NULL REFERENCES causals (id) ON DELETE NO ACTION,
  ADD COLUMN target_causal_id INTEGER NULL REFERENCES causals (id) ON DELETE NO ACTION
;

-- Backfill the new columns with current causal of each contribution, this will be incorrect for old contributions
-- where we can't infer what the source branch _was_ at when it was merged.
UPDATE contributions
  SET source_causal_id = (
    SELECT pb.causal_id
    FROM project_branches pb
    WHERE pb.id = contributions.source_branch
    LIMIT 1
  ),
  target_causal_id = (
    SELECT pb.causal_id
    FROM project_branches pb
    WHERE pb.id = contributions.target_branch
    LIMIT 1
  )
  WHERE source_causal_id IS NULL
    AND target_causal_id IS NULL
;

-- Make the new columns non-nullable
ALTER TABLE contributions
  ALTER COLUMN source_causal_id SET NOT NULL,
  ALTER COLUMN target_causal_id SET NOT NULL
  ;
