-- Add a table for persistent caching of anything which can be serialized to JSON
-- Things in here should be considered immutable and tied to a specific key.
CREATE TABLE json_cache (
    -- The user this cache entry belongs to.
    -- Can be NULL for non-sandboxed entries.
    codebase_user_id UUID NULL REFERENCES users(id) ON DELETE CASCADE,
    -- Which category of data this cache entry belongs to.
    topic TEXT NOT NULL CHECK (topic <> ''),
    -- The key for this cache entry.
    key TEXT NOT NULL CHECK (key <> ''),
    value JSONB NOT NULL,
    -- When this cache entry was created.
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX json_cache_key ON json_cache (topic, codebase_user_id, key) NULLS NOT DISTINCT;
