-- Switch from GIST trigram to GIN trigram for both scoped and global definition search tables.
-- GIN is slower to build, but faster to search, and we compute the indexes in background jobs anyways.

CREATE INDEX scoped_definition_search_docs_name_trigram_gin ON scoped_definition_search_docs USING GIN (name gin_trgm_ops);
-- Drop the old GIST trigram index.
DROP INDEX scoped_definition_search_docs_name_trigram;


CREATE INDEX global_definition_search_name_trigram_gin ON global_definition_search_docs USING GIN (name gin_trgm_ops);
DROP INDEX global_definition_search_name_trigram;
