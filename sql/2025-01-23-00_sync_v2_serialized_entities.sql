-- Gets the composite hash of all component element byte hashes keyed by user and component.
-- NOTE: this digest differs from the 'bytes_hash' you get from taking the digest of the serialized component, it's
-- only a digest of the byte hashes of the elements of the component.
CREATE OR REPLACE FUNCTION compute_component_summary_digest(the_user_id UUID, the_component_hash_id INTEGER) 
RETURNS bytea AS $$
  SELECT t.component_summary_digest FROM
  (
    (SELECT 
      digest(array_to_string(ARRAY_AGG(b.content_hash ORDER BY b.content_hash), '|')::text, 'sha256')::bytea as component_summary_digest
      FROM sandboxed_terms st
        JOIN terms t ON st.term_id = t.id
        JOIN bytes b ON st.bytes_id = b.id
        WHERE st.user_id = the_user_id AND t.component_hash_id = the_component_hash_id
    )
    UNION ALL
    (SELECT 
      digest(array_to_string(ARRAY_AGG(b.content_hash ORDER BY b.content_hash), '|')::text, 'sha256')::bytea as component_summary_digest
      FROM sandboxed_types st
        JOIN types t ON st.type_id = t.id
        JOIN bytes b ON st.bytes_id = b.id
        WHERE st.user_id = the_user_id AND t.component_hash_id = the_component_hash_id
    )
  ) AS t 
    WHERE t.component_summary_digest IS NOT NULL
  ;
$$ LANGUAGE sql;


-- Materialized view to be used just during migration, then removed afterwards.
CREATE MATERIALIZED VIEW user_component_summary_digest AS 
SELECT compute_component_summary_digest(user_id, component_hash_id) AS component_summary_digest, user_id, component_hash_id
  FROM (
    SELECT DISTINCT st.user_id, t.component_hash_id
    FROM sandboxed_terms st
      JOIN terms t ON st.term_id = t.id
    UNION
    SELECT DISTINCT st.user_id, t.component_hash_id
    FROM sandboxed_types st
      JOIN types t ON st.type_id = t.id
  ) AS t
;

-- Stores a mapping between a given component's elements byte hash summary 
CREATE TABLE component_summary_digests_to_serialized_component_bytes_hash(
  component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  -- This is the bytes hash of a sorted array of byte-hashes of all component elements.
  component_summary_digest bytea NOT NULL,
  -- The bytes of the serialized component which matches the elements_byte_hash_digest.
  serialized_component_bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

  PRIMARY KEY (component_hash_id, component_summary_digest) INCLUDE (serialized_component_bytes_id)
);


CREATE TABLE serialized_components (
    -- The user the term is sandboxed to.
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,

    -- The serialized component
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (user_id, component_hash_id) INCLUDE (bytes_id)
);

CREATE TABLE serialized_namespaces (
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,

    -- The serialized namespace
    bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    PRIMARY KEY (namespace_hash_id) INCLUDE (bytes_id)
);

CREATE TABLE serialized_patches (
  patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE CASCADE,
  bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  PRIMARY KEY (patch_id) INCLUDE (bytes_id)
);

CREATE TABLE serialized_causals (
  causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
  bytes_id INTEGER NOT NULL REFERENCES bytes(id) ON DELETE NO ACTION,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

  PRIMARY KEY (causal_id) INCLUDE (bytes_id)
);

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
        ), copied_serialized_components AS (
            INSERT INTO serialized_components(user_id, component_hash_id, bytes_id)
              SELECT DISTINCT to_codebase_user_id, sc.component_hash_id, sc.bytes_id
              FROM transitive_components tc
                JOIN serialized_components sc
                  ON (tc.component_hash_id = sc.component_hash_id
                        AND sc.user_id = from_codebase_user_id
                     )
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
