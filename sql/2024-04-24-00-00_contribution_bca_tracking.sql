ALTER TABLE contributions
  -- It's possible this is null if there's no history in common between the two.
  ADD COLUMN best_common_ancestor_causal_id INTEGER NULL REFERENCES causals (id) ON DELETE SET NULL;
