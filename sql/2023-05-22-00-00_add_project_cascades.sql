-- Add missing ON DELETE CASCADE clauses to project_categories table
ALTER TABLE project_categories 
  DROP CONSTRAINT project_categories_project_id_fkey
, ADD CONSTRAINT project_categories_project_id_fkey
    FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE,
  DROP CONSTRAINT project_categories_category_id_fkey
, ADD CONSTRAINT project_categories_category_id_fkey
    FOREIGN KEY (category_id) REFERENCES catalog_categories (id) ON DELETE CASCADE;
