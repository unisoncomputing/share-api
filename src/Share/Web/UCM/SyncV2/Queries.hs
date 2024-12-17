module Share.Web.UCM.SyncV2.Queries
  ( allHashDependenciesOfCausalCursor,
    allSerializedDependenciesOfCausalCursor,
  )
where

import Control.Monad.Reader
import Share.Codebase (CodebaseM, codebaseOwner)
import Share.Postgres
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as PGCursor
import Share.Postgres.IDs
import Share.Prelude
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.SyncV2.Types (CBORBytes)

allHashDependenciesOfCausalCursor :: CausalId -> CodebaseM e (PGCursor Text)
allHashDependenciesOfCausalCursor cid = do
  ownerUserId <- asks codebaseOwner
  PGCursor.newColCursor
    "causal_dependencies"
    [sql|
        WITH RECURSIVE transitive_causals(causal_id, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = #{cid}
                  -- AND NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = causal.id)
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = causal.id)
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
                    -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = ancestor_causal.id)
                UNION
                SELECT child_causal.id, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = child_causal.id)
            )
        ), all_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
            FROM transitive_causals
            -- WHERE NOT EXISTS (SELECT FROM namespace_ownership no WHERE no.user_id = to_codebase_user_id AND no.namespace_hash_id = causal_namespace_hash_id)
        ), all_patches(patch_id) AS (
            SELECT DISTINCT patch.id
            FROM all_namespaces an
                JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
                JOIN patches patch ON np.patch_id = patch.id
                -- WHERE NOT EXISTS (SELECT FROM patch_ownership po WHERE po.user_id = to_codebase_user_id AND po.patch_id = patch.id)
        ),
        -- term components to start transitively joining dependencies to
        base_term_components(component_hash_id) AS (
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN terms term ON nt.term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            SELECT DISTINCT term.component_hash_id
            FROM all_patches ap
                JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN terms term ON ptm.to_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- term metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_term_metadata meta ON nt.id = meta.named_term
                JOIN terms term ON meta.metadata_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- type metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_type_metadata meta ON nt.id = meta.named_type
                JOIN terms term ON meta.metadata_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
        ),
        -- type components to start transitively joining dependencies to
        base_type_components(component_hash_id) AS (
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN types typ ON nt.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN constructors con ON nt.constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN types typ ON ptm.to_type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
                JOIN constructors con ON pcm.to_constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
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
            SELECT DISTINCT tc.causal_id
            FROM transitive_causals tc
        ), copied_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT an.namespace_hash_id
            FROM all_namespaces an
        ), copied_patches(patch_id) AS (
            SELECT DISTINCT ap.patch_id
            FROM all_patches ap
        ), copied_term_components AS (
                SELECT DISTINCT term.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN terms term ON tc.component_hash_id = term.component_hash_id
                    JOIN sandboxed_terms copy ON term.id = copy.term_id
                    WHERE copy.user_id = #{ownerUserId}
        ), copied_type_components AS (
                SELECT DISTINCT typ.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN types typ ON tc.component_hash_id = typ.component_hash_id
                    JOIN sandboxed_types copy ON typ.id = copy.type_id
                    WHERE copy.user_id = #{ownerUserId}
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
  |]

allSerializedDependenciesOfCausalCursor :: CausalId -> CodebaseM e (PGCursor (CBORBytes TempEntity, Hash32))
allSerializedDependenciesOfCausalCursor cid = do
  ownerUserId <- asks codebaseOwner
  PGCursor.newRowCursor
    "causal_dependencies"
    [sql|
        WITH RECURSIVE transitive_causals(causal_id, causal_hash, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.hash, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = #{cid}
                  -- AND NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = causal.id)
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = causal.id)
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT causal_id, causal_namespace_hash_id
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.hash, ancestor_causal.namespace_hash_id
                FROM causal_ancestors ca
                    JOIN rec tc ON ca.causal_id = tc.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                    -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = ancestor_causal.id)
                UNION
                SELECT child_causal.id, child_causal.hash, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = child_causal.id)
            )
        ), all_namespaces(namespace_hash_id, namespace_hash) AS (
            SELECT DISTINCT tc.causal_namespace_hash_id AS namespace_hash_id, bh.base32 as namespace_hash
            FROM transitive_causals tc
            JOIN branch_hashes bh ON tc.causal_namespace_hash_id = bh.id
            -- WHERE NOT EXISTS (SELECT FROM namespace_ownership no WHERE no.user_id = to_codebase_user_id AND no.namespace_hash_id = tc.causal_namespace_hash_id)
        ), all_patches(patch_id, patch_hash) AS (
            SELECT DISTINCT patch.id, patch.hash
            FROM all_namespaces an
                JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
                JOIN patches patch ON np.patch_id = patch.id
                -- WHERE NOT EXISTS (SELECT FROM patch_ownership po WHERE po.user_id = to_codebase_user_id AND po.patch_id = patch.id)
        ),
        -- term components to start transitively joining dependencies to
        base_term_components(component_hash_id) AS (
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN terms term ON nt.term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            SELECT DISTINCT term.component_hash_id
            FROM all_patches ap
                JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN terms term ON ptm.to_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- term metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_term_metadata meta ON nt.id = meta.named_term
                JOIN terms term ON meta.metadata_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
            UNION
            -- type metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_type_metadata meta ON nt.id = meta.named_type
                JOIN terms term ON meta.metadata_term_id = term.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
        ),
        -- type components to start transitively joining dependencies to
        base_type_components(component_hash_id) AS (
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN types typ ON nt.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN constructors con ON nt.constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN types typ ON ptm.to_type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
                JOIN constructors con ON pcm.to_constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
                -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
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
        )
           (SELECT bytes.bytes, ch.base32
             FROM transitive_components tc
               JOIN serialized_components sc ON tc.component_hash_id = sc.component_hash_id
               JOIN bytes ON sc.bytes_id = bytes.id
               JOIN component_hashes ch ON tc.component_hash_id = ch.id
               -- Reverse the ordering so deeper components come first
               ORDER BY row_number() OVER () DESC
           )
           UNION ALL
           (SELECT bytes.bytes, ap.patch_hash
             FROM all_patches ap
               JOIN serialized_patches sp ON ap.patch_id = sp.patch_id
               JOIN bytes ON sp.bytes_id = bytes.id
               -- Reverse the ordering so deeper components come first
               ORDER BY row_number() OVER () DESC
           )
           UNION ALL
           (SELECT bytes.bytes, an.namespace_hash
             FROM all_namespaces an
               JOIN serialized_namespaces sn ON an.namespace_hash_id = sn.namespace_hash_id
               JOIN bytes ON sn.bytes_id = bytes.id
               -- Reverse the ordering so deeper components come first
               ORDER BY row_number() OVER () DESC
           )
           UNION ALL
           (SELECT bytes.bytes, tc.causal_hash
             FROM transitive_causals tc
               JOIN serialized_causals sc ON tc.causal_id = sc.causal_id
               JOIN bytes ON sc.bytes_id = bytes.id
               -- Reverse the ordering so deeper components come first
               ORDER BY row_number() OVER () DESC
           )
  |]
