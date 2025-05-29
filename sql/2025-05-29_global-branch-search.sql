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
