-- Drop the old indexes and re-make them with the text_pattern_ops operator class for better searching over slug.
DROP INDEX project_user_slug;
DROP INDEX project_slug;

CREATE UNIQUE INDEX project_user_slug ON projects(owner_user_id, slug text_pattern_ops);
CREATE INDEX project_slug ON projects(slug text_pattern_ops);
