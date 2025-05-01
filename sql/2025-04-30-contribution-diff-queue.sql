-- Drop and re-create the contribution_diff_queue table without a primary key constraint on contribution_id.
-- This allows us to insert redundant rows while a diff is being computed, rather than blocking due to row lock.

DROP TABLE contribution_diff_queue;

CREATE TABLE contribution_diff_queue (
  contribution_id UUID REFERENCES contributions(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Delete all previously-computed namespace diffs, because the diff payload is different now (we explicitly store
-- errors).
TRUNCATE namespace_diffs;
