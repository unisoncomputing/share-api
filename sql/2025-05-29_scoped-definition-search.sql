-- Table of all root branch hashes which need to be synced to the relevant definition search index(es).
CREATE TABLE scoped_definition_search_queue (
  -- Nullable release ID, if set we'll also updat the global definition search index.
  release_id UUID NULL REFERENCES project_releases(id) ON DELETE CASCADE,
  root_namespace_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes(id) ON DELETE CASCADE,
  -- A user who has this code. We don't index variable names, so it doesn't matter _which_ user.
  codebase_user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX scoped_definition_search_queue_created_at ON scoped_definition_search_queue(created_at ASC);

-- Port the old queue
INSERT INTO scoped_definition_search_queue (root_namespace_hash_id, codebase_user_id, created_at)
  SELECT c.namespace_hash_id AS root_namespace_hash_id,
         p.owner_user_id AS codebase_user_id,
         gdsrq.created_at AS created_at
  FROM global_definition_search_release_queue gdsrq
  JOIN project_releases release ON gdsrq.release_id = release.id
  JOIN causals c ON release.squashed_causal_id = c.id
  JOIN projects p ON release.project_id = p.id
  ON CONFLICT DO NOTHING;
TRUNCATE global_definition_search_release_queue;


-- Expand the global definition search to include all branch and release heads.

-- The set of roots we've indexed.
CREATE TABLE indexed_definition_search_doc_roots (
  root_namespace_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes(id) ON DELETE CASCADE
);

-- A version of the global_definition_search_docs table, but contains search docs scoped by the root namespace rather
-- than the project and release.
-- This allows us to do a definition search within any branch.
-- It can eventually be used to replace the global_definition_search_docs table,
-- but that can be done separately in the future.
CREATE TABLE scoped_definition_search_docs (
  root_namespace_hash_id INTEGER NOT NULL REFERENCES indexed_definition_search_doc_roots(root_namespace_hash_id) ON DELETE CASCADE,
  -- Fully qualified name of the definition
  name TEXT NOT NULL,
  search_tokens TSVECTOR NOT NULL,
  -- Number of arguments. 0 for values.
  arity INT NOT NULL,
  tag definition_tag NOT NULL,

  -- Contains the rendered type signature, type, hash, etc.
  -- so we don't need to look up types for hundreds of search results on the fly.
  metadata JSONB NOT NULL,

  -- Ostensibly there's the possibility of name conflicts,
  -- but those are rare enough we don't care, we just insert with ON CONFLICT DO NOTHING.
  PRIMARY KEY (root_namespace_hash_id, name)
);

-- Port the old indexes to the new table.
INSERT INTO indexed_definition_search_doc_roots (root_namespace_hash_id)
  SELECT DISTINCT c.namespace_hash_id AS root_namespace_hash_id
  FROM global_definition_search_docs gds
  JOIN project_releases release ON gds.release_id = release.id
  JOIN causals c ON release.squashed_causal_id = c.id
  ON CONFLICT DO NOTHING;

INSERT INTO scoped_definition_search_docs (root_namespace_hash_id, name, search_tokens, arity, tag, metadata)
  SELECT 
    c.namespace_hash_id AS root_namespace_hash_id,
    gds.name,
    gds.search_tokens,
    gds.arity,
    gds.tag,
    gds.metadata
  FROM global_definition_search_docs gds
  JOIN project_releases release ON gds.release_id = release.id
  JOIN causals c ON release.squashed_causal_id = c.id
  ON CONFLICT DO NOTHING;

-- Index for searching global definitions by 'search token', with an optional project/release filter.
-- P.s. there's a search token type for name, so we don't need to index that separately.
CREATE INDEX scoped_definition_search_docs_tokens ON scoped_definition_search_docs USING GIN(root_namespace_hash_id, search_tokens, tag);

-- Index for fuzzy-searching on the fully qualified name.
CREATE INDEX scoped_definition_search_docs_name_trigram ON scoped_definition_search_docs USING GIST (name gist_trgm_ops);

-- Insert into the queue to be synced using triggers on branches and releases
CREATE OR REPLACE FUNCTION scoped_definition_search_queue_on_branch_change_trigger()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if this is an INSERT or if the 
    IF TG_OP = 'INSERT' OR (TG_OP = 'UPDATE' AND OLD.causal_id IS DISTINCT FROM NEW.causal_id) THEN
        INSERT INTO scoped_definition_search_queue (root_namespace_hash_id, codebase_user_id)
          SELECT c.namespace_hash_id AS root_namespace_hash_id,
                 p.owner_user_id AS codebase_user_id
          FROM causals c
          JOIN projects p ON p.id = NEW.project_id
          WHERE c.id = NEW.causal_id
          ;
        PERFORM pg_notify('definition_sync');
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER scoped_definition_search_queue_on_branch_change
AFTER INSERT OR UPDATE ON project_branches
FOR EACH ROW
EXECUTE FUNCTION scoped_definition_search_queue_on_branch_change_trigger();

CREATE OR REPLACE FUNCTION scoped_definition_search_queue_on_release_publish_trigger()
RETURNS TRIGGER AS $$
BEGIN
    INSERT INTO scoped_definition_search_queue (root_namespace_hash_id, release_id, codebase_user_id)
      SELECT c.namespace_hash_id AS root_namespace_hash_id,
              NEW.id AS release_id,
              p.owner_user_id AS codebase_user_id
      FROM causals c
      JOIN projects p ON p.id = NEW.project_id
      WHERE c.id = NEW.squashed_causal_id
      ;
    PERFORM pg_notify('definition_sync');
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER scoped_definition_search_queue_on_release_publish
AFTER INSERT ON project_releases
FOR EACH ROW
EXECUTE FUNCTION scoped_definition_search_queue_on_release_publish_trigger();

