-- Add a text search index for projects over slug, display_name and summary in decreasing priority

ALTER TABLE projects
  ADD COLUMN project_text_document tsvector
    GENERATED ALWAYS AS (
      setweight(to_tsvector('english', coalesce(slug,'')), 'A') ||
      setweight(to_tsvector('english', coalesce(display_name,'')), 'B') ||
      setweight(to_tsvector('english', coalesce(summary,'')), 'C')
    ) STORED;

CREATE INDEX projects_search_index ON projects USING GIN(project_text_document);
