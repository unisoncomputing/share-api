-- Add a retries column to the contribution_diff_queue table
ALTER TABLE contribution_diff_queue ADD COLUMN retries_left INTEGER NOT NULL DEFAULT 3;
