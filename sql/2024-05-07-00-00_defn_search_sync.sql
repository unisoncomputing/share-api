-- New table for coordinating background job for syncing global definitions for search.

-- Table of all releases which have been published, but not yet synced to the global definition search index.
CREATE TABLE global_definition_search_release_queue (
  release_id UUID PRIMARY KEY REFERENCES releases(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
