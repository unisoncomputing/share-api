-- Existence of a row represents that the user has favorited a given project.
CREATE TABLE project_favorites (
  project_id uuid NOT NULL REFERENCES projects (id) ON DELETE CASCADE,
  user_id uuid NOT NULL REFERENCES users (id) ON DELETE CASCADE,
  PRIMARY KEY (project_id, user_id)
);

CREATE INDEX project_favs_by_user ON project_favorites(user_id, project_id);
