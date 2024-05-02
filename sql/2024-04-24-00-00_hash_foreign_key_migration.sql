-- Alter all mentions of causal hashes to use causal ids instead.
-- This guarantees that the causal being referenced actually exists, reduces the need for joins
-- and makes it easier to change the hash algorithm in the future.

--------------------------------------------------------------------------------

DROP VIEW IF EXISTS debug_project_branches;
DROP VIEW IF EXISTS debug_project_releases;

ALTER TABLE project_branches DISABLE TRIGGER USER;

ALTER TABLE project_branches
  ADD COLUMN causal_id INTEGER NULL REFERENCES causals(id);

CREATE INDEX branches_by_causal_id_and_created_at ON project_branches (causal_id ASC, created_at DESC);

UPDATE project_branches
  SET causal_id = (SELECT id FROM causals WHERE project_branches.causal_hash = causals.hash);

ALTER TABLE project_branches
  ALTER COLUMN causal_id SET NOT NULL,
  DROP COLUMN causal_hash;

ALTER TABLE project_releases
  ADD COLUMN causal_id INTEGER NULL REFERENCES causals(id);

ALTER TABLE project_branches ENABLE TRIGGER USER;

--------------------------------------------------------------------------------

ALTER TABLE project_releases DISABLE TRIGGER USER;

ALTER TABLE project_releases
  ADD COLUMN squashed_causal_id INTEGER NULL REFERENCES causals(id),
  ADD COLUMN unsquashed_causal_id INTEGER NULL REFERENCES causals(id)
  ;

CREATE INDEX project_releases_by_squashed_causal_id_and_created_at ON project_releases (squashed_causal_id ASC, created_at DESC);
CREATE INDEX project_releases_by_unsquashed_causal_id_and_created_at ON project_releases (unsquashed_causal_id ASC, created_at DESC);

UPDATE project_releases
  SET squashed_causal_id = (SELECT id FROM causals WHERE project_releases.causal_hash_squashed = causals.hash);

UPDATE project_releases
  SET unsquashed_causal_id = (SELECT id FROM causals WHERE project_releases.causal_hash_unsquashed = causals.hash);

ALTER TABLE project_releases
  ALTER COLUMN squashed_causal_id SET NOT NULL,
  DROP COLUMN causal_hash_squashed,
  ALTER COLUMN unsquashed_causal_id SET NOT NULL,
  DROP COLUMN causal_hash_unsquashed;

ALTER TABLE project_releases ENABLE TRIGGER USER;

--------------------------------------------------------------------------------

ALTER TABLE project_branch_reflog DISABLE TRIGGER USER;

ALTER TABLE project_branch_reflog
  ADD COLUMN old_causal_id INTEGER NULL REFERENCES causals(id),
  ADD COLUMN new_causal_id INTEGER NULL REFERENCES causals(id)
  ;

UPDATE project_branch_reflog
  SET old_causal_id = (SELECT id FROM causals WHERE project_branch_reflog.old_causal_hash = causals.hash);

UPDATE project_branch_reflog
  SET new_causal_id = (SELECT id FROM causals WHERE project_branch_reflog.new_causal_hash = causals.hash);

ALTER TABLE project_branch_reflog
  DROP COLUMN old_causal_hash,
  ALTER COLUMN new_causal_id SET NOT NULL,
  DROP COLUMN new_causal_hash;

ALTER TABLE project_branch_reflog ENABLE TRIGGER USER;

--------------------------------------------------------------------------------

ALTER TABLE loose_code_roots DISABLE TRIGGER USER;

ALTER TABLE loose_code_roots
  ADD COLUMN causal_id INTEGER NULL REFERENCES causals(id);

UPDATE loose_code_roots
  SET causal_id = (SELECT id FROM causals WHERE loose_code_roots.causal_hash = causals.hash);

ALTER TABLE loose_code_roots
  ALTER COLUMN causal_id SET NOT NULL,
  DROP COLUMN causal_hash;

ALTER TABLE loose_code_roots ENABLE TRIGGER USER;

--------------------------------------------------------------------------------

ALTER TABLE loose_code_reflog DISABLE TRIGGER USER;

ALTER TABLE loose_code_reflog
  ADD COLUMN old_causal_id INTEGER NULL REFERENCES causals(id),
  ADD COLUMN new_causal_id INTEGER NULL REFERENCES causals(id)
  ;

UPDATE loose_code_reflog
  SET old_causal_id = (SELECT id FROM causals WHERE loose_code_reflog.old_causal_hash = causals.hash);

UPDATE loose_code_reflog
  SET new_causal_id = (SELECT id FROM causals WHERE loose_code_reflog.new_causal_hash = causals.hash);

ALTER TABLE loose_code_reflog
  DROP COLUMN old_causal_hash,
  ALTER COLUMN new_causal_id SET NOT NULL,
  DROP COLUMN new_causal_hash;

ALTER TABLE loose_code_reflog ENABLE TRIGGER USER;
