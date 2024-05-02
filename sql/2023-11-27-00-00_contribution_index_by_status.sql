-- Allow easily filtering contributions by status.
CREATE INDEX contributions_by_project_and_status ON contributions(project_id, status, updated_at DESC);
