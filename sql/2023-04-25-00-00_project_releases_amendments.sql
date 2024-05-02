ALTER TABLE project_releases RENAME COLUMN causal_hash TO causal_hash_squashed;

ALTER TABLE project_releases
  DROP COLUMN status,
  DROP COLUMN published_at,
  DROP COLUMN published_by,
  ADD COLUMN causal_hash_unsquashed text NOT NULL;

DROP TABLE project_release_reflog;

