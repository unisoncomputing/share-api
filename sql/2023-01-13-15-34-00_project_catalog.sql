CREATE TABLE catalog_categories (
    id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
    name citext UNIQUE NOT NULL
);

CREATE INDEX catalog_categories_name ON catalog_categories (name);

CREATE TABLE project_categories (
    category_id uuid NOT NULL REFERENCES catalog_categories(id),
    project_id uuid NOT NULL REFERENCES projects(id),
    PRIMARY KEY (category_id, project_id)
);

CREATE INDEX project_categories_by_project ON project_categories (project_id);
