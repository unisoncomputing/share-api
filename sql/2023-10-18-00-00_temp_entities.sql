CREATE TYPE entity_kind AS ENUM ('term_component', 'decl_component', 'namespace', 'patch', 'causal');

-- A "temp entity" is a term/decl/namespace/patch/causal that we cannot store in the database proper due to missing
-- dependencies.
--
-- The existence of each `temp_entity` row implies the existence of one or more corresponding
-- `temp_entity_missing_dependency` rows: it does not make sense to make a `temp_entity` row for a thing that has no
-- missing dependencies!
--
-- Similarly, each `temp_entity` row implies we do not have the entity in the database proper. When and if we *do* store
-- an entity proper (after storing all of its dependencies), we should always atomically delete the corresponding
-- `temp_entity` row, if any.
CREATE TABLE temp_entity (
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  hash TEXT NOT NULL,
  bytes BYTEA NOT NULL,
  kind entity_kind NOT NULL,
  PRIMARY KEY (user_id, hash)
);

-- A many-to-many relationship between `temp_entity` (entities we can't yet store due to missing dependencies), and the
-- non-empty set of hashes of each entity's dependencies.
--
-- For example, if we wanted to store term #foo, but couldn't because it depends on term #bar which we don't have yet,
-- we would end up with the following rows.
--
--  temp_entity
-- +----------------------------------+
-- | user_id | hash | bytes | type_id  |
-- |==================================|
-- | U-...   | #foo | ...  | term_component |
-- +----------------------------------+
--
--  temp_entity_missing_dependency
-- +----------------------------------+
-- | user_id | dependent | dependency |
-- |=========|========================|
-- | U-...   | #foo      | #bar       |
-- +----------------------------------+
CREATE TABLE temp_entity_missing_dependency (
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  -- Hash of the dependent
  dependent TEXT NOT NULL,
  -- Hash of the dependency
  dependency TEXT NOT NULL,
  PRIMARY KEY (user_id, dependent, dependency),
  FOREIGN KEY (user_id, dependent) REFERENCES temp_entity(user_id, hash) ON DELETE NO ACTION
);
CREATE INDEX temp_entity_missing_dependency_ix_dependent ON temp_entity_missing_dependency (user_id, dependent);
CREATE INDEX temp_entity_missing_dependency_ix_dependency ON temp_entity_missing_dependency (user_id, dependency);

-- Used to determine which entities are still missing dependencies during upload.
-- Includes all hashes in the user's codebase but not temp_entities
CREATE OR REPLACE VIEW main_hashes_for_user(user_id, hash, kind) AS (
    SELECT causal_ownership.user_id, causal.hash, 'causal'::entity_kind
      FROM causal_ownership JOIN causals causal ON causal_ownership.causal_id = causal.id
    UNION ALL
    SELECT namespace_ownership.user_id, branch_hashes.base32, 'namespace'::entity_kind
      FROM namespace_ownership
        JOIN namespaces ON namespace_ownership.namespace_hash_id = namespaces.namespace_hash_id
        JOIN branch_hashes ON namespaces.namespace_hash_id = branch_hashes.id
    UNION ALL
    SELECT patch_ownership.user_id, patch.hash, 'patch'::entity_kind
      FROM patch_ownership JOIN patches patch ON patch_ownership.patch_id = patch.id
    UNION ALL
    SELECT sandboxed_terms.user_id, component_hashes.base32, 'term_component'::entity_kind
      FROM sandboxed_terms
        JOIN terms ON sandboxed_terms.term_id = terms.id
        JOIN component_hashes ON terms.component_hash_id = component_hashes.id
    UNION ALL
    SELECT sandboxed_types.user_id, component_hashes.base32, 'decl_component'::entity_kind
      FROM sandboxed_types
        JOIN types ON sandboxed_types.type_id = types.id
        JOIN component_hashes ON types.component_hash_id = component_hashes.id
);

-- Includes all hashes in both the user's codebase and temp_entities
CREATE OR REPLACE VIEW all_hashes_for_user(user_id, hash, kind) AS (
    SELECT user_id, hash, kind FROM temp_entity
    UNION
    SELECT user_id, hash, kind FROM main_hashes_for_user
);

CREATE OR REPLACE FUNCTION remove_entity_from_temp(user_id UUID, hash TEXT)
RETURNS VOID AS $$
BEGIN
    DELETE FROM temp_entity_missing_dependency dep
      WHERE dep.user_id = remove_entity_from_temp.user_id
        AND (dep.dependent = remove_entity_from_temp.hash OR dep.dependency = remove_entity_from_temp.hash);
    DELETE FROM temp_entity te WHERE te.user_id = remove_entity_from_temp.user_id AND te.hash = remove_entity_from_temp.hash;
END;
$$ LANGUAGE plpgsql;

-- Copies ALL dependencies of a causal into the codebase.
CREATE OR REPLACE FUNCTION copy_causal_into_codebase(causal_id_to_copy INTEGER, from_codebase_user_id UUID, to_codebase_user_id UUID)
RETURNS VOID AS $$
DECLARE copied_hash TEXT;
BEGIN
    -- We use a recursive CTE to recursively collect all the dependencies of the causal.
    -- This probably uses a bit more memory than calling functions recursively, but is much more in line
    -- with how Postgres is designed to work. The recursive function approach hit the stack depth limit :|
    -- This will also save us time by not trying to import the same hashes multiple times since we can dedupe them
    -- up-front.
    FOR copied_hash IN
        WITH RECURSIVE transitive_causals(causal_id, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = causal_id_to_copy
                  AND NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = causal.id)
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = from_codebase_user_id AND co.causal_id = causal.id)
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT causal_id, causal_namespace_hash_id
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.namespace_hash_id
                FROM causal_ancestors ca
                    JOIN rec tc ON ca.causal_id = tc.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                    WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = ancestor_causal.id)
                UNION
                SELECT child_causal.id, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = child_causal.id)
            )
        ), all_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
            FROM transitive_causals
            WHERE NOT EXISTS (SELECT FROM namespace_ownership no WHERE no.user_id = to_codebase_user_id AND no.namespace_hash_id = causal_namespace_hash_id)
        ), all_patches(patch_id) AS (
            SELECT DISTINCT patch.id
            FROM all_namespaces an
                JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
                JOIN patches patch ON np.patch_id = patch.id
                WHERE NOT EXISTS (SELECT FROM patch_ownership po WHERE po.user_id = to_codebase_user_id AND po.patch_id = patch.id)
        ),
        -- term components to start transitively joining dependencies to
        base_term_components(component_hash_id) AS (
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN terms term ON nt.term_id = term.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            SELECT DISTINCT term.component_hash_id
            FROM all_patches ap
                JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN terms term ON ptm.to_term_id = term.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- term metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_term_metadata meta ON nt.id = meta.named_term
                JOIN terms term ON meta.metadata_term_id = term.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- type metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_type_metadata meta ON nt.id = meta.named_type
                JOIN terms term ON meta.metadata_term_id = term.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
        ),
        -- type components to start transitively joining dependencies to
        base_type_components(component_hash_id) AS (
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN types typ ON nt.type_id = typ.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN constructors con ON nt.constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN types typ ON ptm.to_type_id = typ.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
                JOIN constructors con ON pcm.to_constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
        ),
        -- All the dependencies we join in transitively from the known term & type components we depend on.
        -- Unfortunately it's not possible to know which hashes are terms vs types :'(
        transitive_components(component_hash_id) AS (
            SELECT DISTINCT btc.component_hash_id
            FROM base_term_components btc
            UNION
            SELECT DISTINCT btc.component_hash_id
            FROM base_type_components btc
            UNION
            ( WITH rec AS (
                SELECT component_hash_id
                FROM transitive_components tc
            )
                -- recursively union in term dependencies
                SELECT DISTINCT ref.component_hash_id
                FROM rec atc
                    -- This joins in ALL the terms from the component, not just the one that caused the dependency on the
                    -- component
                    JOIN terms term ON atc.component_hash_id = term.component_hash_id
                    JOIN term_local_component_references ref ON term.id = ref.term_id
                UNION
                -- recursively union in type dependencies
                SELECT DISTINCT ref.component_hash_id
                FROM rec atc
                    -- This joins in ALL the types from the component, not just the one that caused the dependency on the
                    -- component
                    JOIN types typ ON atc.component_hash_id = typ.component_hash_id
                    JOIN type_local_component_references ref ON typ.id = ref.type_id
            )
        ), copied_causals(causal_id) AS (
          INSERT INTO causal_ownership (user_id, causal_id)
            SELECT DISTINCT to_codebase_user_id, tc.causal_id
            FROM transitive_causals tc
            ON CONFLICT DO NOTHING
            RETURNING causal_id
        ), copied_namespaces(namespace_hash_id) AS (
          INSERT INTO namespace_ownership (user_id, namespace_hash_id)
            SELECT DISTINCT to_codebase_user_id, an.namespace_hash_id
            FROM all_namespaces an
            ON CONFLICT DO NOTHING
            RETURNING namespace_hash_id
        ), copied_patches(patch_id) AS (
          INSERT INTO patch_ownership (user_id, patch_id)
            SELECT DISTINCT to_codebase_user_id, ap.patch_id
            FROM all_patches ap
            ON CONFLICT DO NOTHING
            RETURNING patch_id
        ), copied_term_components AS (
              INSERT INTO sandboxed_terms (user_id, term_id, bytes_id)
                SELECT DISTINCT to_codebase_user_id, term.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN terms term ON tc.component_hash_id = term.component_hash_id
                    JOIN sandboxed_terms copy ON term.id = copy.term_id
                    WHERE copy.user_id = from_codebase_user_id
                    ON CONFLICT DO NOTHING
        ), copied_type_components AS (
              INSERT INTO sandboxed_types (user_id, type_id, bytes_id)
                SELECT DISTINCT to_codebase_user_id, typ.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN types typ ON tc.component_hash_id = typ.component_hash_id
                    JOIN sandboxed_types copy ON typ.id = copy.type_id
                    WHERE copy.user_id = from_codebase_user_id
                    ON CONFLICT DO NOTHING
        ) SELECT causal.hash
             FROM copied_causals cc
               JOIN causals causal ON cc.causal_id = causal.id
           UNION ALL
           SELECT branch_hashes.base32
             FROM copied_namespaces cn
               JOIN branch_hashes ON cn.namespace_hash_id = branch_hashes.id
           UNION ALL
           SELECT patch.hash
             FROM copied_patches cp
               JOIN patches patch ON cp.patch_id = patch.id
           UNION ALL
           SELECT component_hashes.base32
             FROM transitive_components tc
               JOIN component_hashes ON tc.component_hash_id = component_hashes.id
               WHERE EXISTS (SELECT FROM temp_entity te WHERE te.user_id = to_codebase_user_id AND te.hash = component_hashes.base32)
                  OR EXISTS (SELECT FROM temp_entity_missing_dependency dep WHERE (dep.user_id = to_codebase_user_id) AND (dep.dependent = component_hashes.base32 OR dep.dependency = component_hashes.base32))
    LOOP
        PERFORM remove_entity_from_temp(to_codebase_user_id, copied_hash);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

-- Check whether a hash exists in the user's main codebase.
CREATE OR REPLACE FUNCTION have_hash_in_codebase(codebase_owner_user_id UUID, hash_to_check TEXT)
RETURNS BOOLEAN
STABLE
PARALLEL SAFE
AS $$
BEGIN
    RETURN EXISTS (
        SELECT FROM causals causal
            JOIN causal_ownership co ON causal.id = co.causal_id
            WHERE co.user_id = codebase_owner_user_id AND causal.hash = hash_to_check
        UNION ALL
        SELECT FROM namespace_ownership no
            JOIN branch_hashes ON no.namespace_hash_id = branch_hashes.id
            WHERE no.user_id = codebase_owner_user_id AND branch_hashes.base32 = hash_to_check
        UNION ALL
        SELECT FROM patch_ownership po
            JOIN patches patch ON po.patch_id = patch.id
            WHERE po.user_id = codebase_owner_user_id AND patch.hash = hash_to_check
        UNION ALL
        SELECT FROM sandboxed_terms st
            JOIN terms term ON st.term_id = term.id
            JOIN component_hashes ON term.component_hash_id = component_hashes.id
            WHERE st.user_id = codebase_owner_user_id AND component_hashes.base32 = hash_to_check
        UNION ALL
        SELECT FROM sandboxed_types st
            JOIN types typ ON st.type_id = typ.id
            JOIN component_hashes ON typ.component_hash_id = component_hashes.id
            WHERE st.user_id = codebase_owner_user_id AND component_hashes.base32 = hash_to_check
    );
END;
$$ LANGUAGE plpgsql;
