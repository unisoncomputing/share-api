-- A table which contains all the known hash mismatches.
-- This allows us to permit some known hash mismatches, while still
-- detecting new ones.
CREATE TABLE known_component_hash_mismatches (
    provided_component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id),
    actual_component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id),
    PRIMARY KEY (provided_component_hash_id, actual_component_hash_id)
);
