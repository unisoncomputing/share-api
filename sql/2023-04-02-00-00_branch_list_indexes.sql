-- Used by List branches endpoint
CREATE INDEX branches_by_project_id_and_contributor_id_and_updated_time ON project_branches(project_id, contributor_id, updated_at DESC) 
  WHERE deleted_at IS NULL;

CREATE INDEX branches_by_project_id_by_updated_time ON project_branches(project_id, updated_at DESC) 
  WHERE deleted_at IS NULL;

CREATE INDEX branches_by_contributor_id_and_updated_time ON project_branches(contributor_id, updated_at DESC) 
  WHERE deleted_at IS NULL
  AND contributor_id IS NOT NULL;
