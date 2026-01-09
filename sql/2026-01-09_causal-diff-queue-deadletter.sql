-- Add nullable error column to causal_diff_queue table
ALTER TABLE causal_diff_queue
  ADD COLUMN error TEXT;

CREATE INDEX IF NOT EXISTS idx_causal_diff_queue_processing_order
  ON causal_diff_queue (created_at ASC)
  WHERE error IS NULL;
