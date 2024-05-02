-- Changes to existing tables to be ready for fully-postgres codebases.

-- Use text_pattern_ops for the hash columns so we can use the index for short-hash prefix searches.
DROP INDEX component_hashes_base_32;
CREATE UNIQUE INDEX component_hashes_base32 ON component_hashes (base32 text_pattern_ops);


