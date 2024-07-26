-- Allows us to create composite indexes over traditionally non-GIN indexable types.
-- In this case it allows us to include the project_id and release_id in the GIN index for search tokens.
CREATE EXTENSION IF NOT EXISTS btree_gin;

-- Allows us to create trigram indexes for fuzzy searching.
CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- New table for coordinating background job for syncing global definitions for search.

-- Table of all releases which have been published, but not yet synced to the global definition search index.
CREATE TABLE global_definition_search_release_queue (
  release_id UUID PRIMARY KEY REFERENCES project_releases(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Every defn fits into one of these categories.
CREATE TYPE definition_tag AS ENUM ('doc', 'test', 'plain', 'data', 'ability', 'data-constructor', 'ability-constructor');

CREATE TABLE global_definition_search_docs (
  project_id UUID NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
  release_id UUID NOT NULL REFERENCES project_releases(id) ON DELETE CASCADE,
  -- Fully qualified name
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
  PRIMARY KEY (project_id, release_id, name)
);

-- Index for searching global definitions by 'search token', with an optional project/release filter.
-- P.s. there's a search token type for name, so we don't need to index that separately.
CREATE INDEX global_definition_search_tokens ON global_definition_search_docs USING GIN(search_tokens, tag, project_id, release_id);

-- Index for fuzzy-searching on the fully qualified name.
CREATE INDEX global_definition_search_name_trigram ON global_definition_search_docs USING GIST (name gist_trgm_ops);
