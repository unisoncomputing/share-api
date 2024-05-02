-- A table which contains all the known hash mismatches for causals.
-- This allows us to permit some known hash mismatches, while still
-- detecting new ones.
CREATE TABLE known_causal_hash_mismatches (
    provided_causal_hash TEXT NOT NULL,
    actual_causal_hash TEXT NOT NULL,
    PRIMARY KEY (provided_causal_hash, actual_causal_hash)
);

ALTER TABLE known_component_hash_mismatches
    ADD COLUMN provided_component_hash TEXT NULL,
    ADD COLUMN actual_component_hash TEXT NULL;

UPDATE known_component_hash_mismatches k
    SET provided_component_hash = provided_hash.base32,
        actual_component_hash = actual_hash.base32
    FROM component_hashes actual_hash, component_hashes provided_hash
    WHERE k.provided_component_hash_id = provided_hash.id
        AND k.actual_component_hash_id = actual_hash.id;

ALTER TABLE known_component_hash_mismatches
    DROP COLUMN provided_component_hash_id,
    DROP COLUMN actual_component_hash_id,
    ALTER COLUMN provided_component_hash SET NOT NULL,
    ALTER COLUMN actual_component_hash SET NOT NULL;
