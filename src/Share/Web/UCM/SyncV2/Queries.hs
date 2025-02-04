module Share.Web.UCM.SyncV2.Queries
  ( allSerializedDependenciesOfCausalCursor,
    spineAndLibDependenciesOfCausalCursor,
  )
where

import Control.Monad.Reader
import Share.Codebase (CodebaseM, codebaseOwner)
import Share.Postgres
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as PGCursor
import Share.Postgres.IDs
import Share.Prelude
import Share.Web.UCM.SyncV2.Types (IsCausalSpine (..), IsLibRoot (..))
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.SyncV2.Types (CBORBytes)

-- Useful, but needs to be double-checked before use.
-- allHashDependenciesOfCausalCursor :: CausalId -> CodebaseM e (PGCursor Text)
-- allHashDependenciesOfCausalCursor cid = do
--   ownerUserId <- asks codebaseOwner
--   PGCursor.newColCursor
--     "causal_dependencies"
--     [sql|
--         WITH RECURSIVE transitive_causals(causal_id, causal_namespace_hash_id) AS (
--             SELECT causal.id, causal.namespace_hash_id
--             FROM causals causal
--                 WHERE causal.id = #{cid}
--                   AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = causal.id)
--             UNION
--             -- This nested CTE is required because RECURSIVE CTEs can't refer
--             -- to the recursive table more than once.
--             ( WITH rec AS (
--                 SELECT causal_id, causal_namespace_hash_id
--                 FROM transitive_causals tc
--             )
--                 SELECT ancestor_causal.id, ancestor_causal.namespace_hash_id
--                 FROM causal_ancestors ca
--                     JOIN rec tc ON ca.causal_id = tc.causal_id
--                     JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
--                     -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = ancestor_causal.id)
--                 UNION
--                 SELECT child_causal.id, child_causal.namespace_hash_id
--                 FROM rec tc
--                     JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
--                     JOIN causals child_causal ON nc.child_causal_id = child_causal.id
--                     -- WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = child_causal.id)
--             )
--         ), all_namespaces(namespace_hash_id) AS (
--             SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
--             FROM transitive_causals
--             -- WHERE NOT EXISTS (SELECT FROM namespace_ownership no WHERE no.user_id = to_codebase_user_id AND no.namespace_hash_id = causal_namespace_hash_id)
--         ), all_patches(patch_id) AS (
--             SELECT DISTINCT patch.id
--             FROM all_namespaces an
--                 JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
--                 JOIN patches patch ON np.patch_id = patch.id
--                 -- WHERE NOT EXISTS (SELECT FROM patch_ownership po WHERE po.user_id = to_codebase_user_id AND po.patch_id = patch.id)
--         ),
--         -- term components to start transitively joining dependencies to
--         base_term_components(component_hash_id) AS (
--             SELECT DISTINCT term.component_hash_id
--             FROM all_namespaces an
--                 JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
--                 JOIN terms term ON nt.term_id = term.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
--             UNION
--             SELECT DISTINCT term.component_hash_id
--             FROM all_patches ap
--                 JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
--                 JOIN terms term ON ptm.to_term_id = term.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
--             UNION
--             -- term metadata
--             SELECT DISTINCT term.component_hash_id
--             FROM all_namespaces an
--                 JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
--                 JOIN namespace_term_metadata meta ON nt.id = meta.named_term
--                 JOIN terms term ON meta.metadata_term_id = term.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
--             UNION
--             -- type metadata
--             SELECT DISTINCT term.component_hash_id
--             FROM all_namespaces an
--                 JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
--                 JOIN namespace_type_metadata meta ON nt.id = meta.named_type
--                 JOIN terms term ON meta.metadata_term_id = term.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_terms st WHERE st.user_id = to_codebase_user_id AND st.term_id = term.id)
--         ),
--         -- type components to start transitively joining dependencies to
--         base_type_components(component_hash_id) AS (
--             SELECT DISTINCT typ.component_hash_id
--             FROM all_namespaces an
--                 JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
--                 JOIN types typ ON nt.type_id = typ.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
--             UNION
--             SELECT DISTINCT typ.component_hash_id
--             FROM all_namespaces an
--                 JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
--                 JOIN constructors con ON nt.constructor_id = con.id
--                 JOIN types typ ON con.type_id = typ.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
--             UNION
--             SELECT DISTINCT typ.component_hash_id
--             FROM all_patches ap
--                 JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
--                 JOIN types typ ON ptm.to_type_id = typ.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
--             UNION
--             SELECT DISTINCT typ.component_hash_id
--             FROM all_patches ap
--                 JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
--                 JOIN constructors con ON pcm.to_constructor_id = con.id
--                 JOIN types typ ON con.type_id = typ.id
--                 -- WHERE NOT EXISTS (SELECT FROM sandboxed_types st WHERE st.user_id = to_codebase_user_id AND st.type_id = typ.id)
--         ),
--         -- All the dependencies we join in transitively from the known term & type components we depend on.
--         -- Unfortunately it's not possible to know which hashes are terms vs types :'(
--         transitive_components(component_hash_id) AS (
--             SELECT DISTINCT btc.component_hash_id
--             FROM base_term_components btc
--             UNION
--             SELECT DISTINCT btc.component_hash_id
--             FROM base_type_components btc
--             UNION
--             ( WITH rec AS (
--                 SELECT component_hash_id
--                 FROM transitive_components tc
--             )
--                 -- recursively union in term dependencies
--                 SELECT DISTINCT ref.component_hash_id
--                 FROM rec atc
--                     -- This joins in ALL the terms from the component, not just the one that caused the dependency on the
--                     -- component
--                     JOIN terms term ON atc.component_hash_id = term.component_hash_id
--                     JOIN term_local_component_references ref ON term.id = ref.term_id
--                 UNION
--                 -- recursively union in type dependencies
--                 SELECT DISTINCT ref.component_hash_id
--                 FROM rec atc
--                     -- This joins in ALL the types from the component, not just the one that caused the dependency on the
--                     -- component
--                     JOIN types typ ON atc.component_hash_id = typ.component_hash_id
--                     JOIN type_local_component_references ref ON typ.id = ref.type_id
--             )
--         ), copied_causals(causal_id) AS (
--             SELECT DISTINCT tc.causal_id
--             FROM transitive_causals tc
--         ), copied_namespaces(namespace_hash_id) AS (
--             SELECT DISTINCT an.namespace_hash_id
--             FROM all_namespaces an
--         ), copied_patches(patch_id) AS (
--             SELECT DISTINCT ap.patch_id
--             FROM all_patches ap
--         ), copied_term_components AS (
--                 SELECT DISTINCT term.id, copy.bytes_id
--                 FROM transitive_components tc
--                     JOIN terms term ON tc.component_hash_id = term.component_hash_id
--                     JOIN sandboxed_terms copy ON term.id = copy.term_id
--                     WHERE copy.user_id = #{ownerUserId}
--         ), copied_type_components AS (
--                 SELECT DISTINCT typ.id, copy.bytes_id
--                 FROM transitive_components tc
--                     JOIN types typ ON tc.component_hash_id = typ.component_hash_id
--                     JOIN sandboxed_types copy ON typ.id = copy.type_id
--                     WHERE copy.user_id = #{ownerUserId}
--         ) SELECT causal.hash
--              FROM copied_causals cc
--                JOIN causals causal ON cc.causal_id = causal.id
--            UNION ALL
--            SELECT branch_hashes.base32
--              FROM copied_namespaces cn
--                JOIN branch_hashes ON cn.namespace_hash_id = branch_hashes.id
--            UNION ALL
--            SELECT patch.hash
--              FROM copied_patches cp
--                JOIN patches patch ON cp.patch_id = patch.id
--            UNION ALL
--            SELECT component_hashes.base32
--              FROM transitive_components tc
--                JOIN component_hashes ON tc.component_hash_id = component_hashes.id
--   |]

allSerializedDependenciesOfCausalCursor :: CausalId -> Set CausalHash -> CodebaseM e (PGCursor (CBORBytes TempEntity, Hash32))
allSerializedDependenciesOfCausalCursor cid exceptCausalHashes = do
  ownerUserId <- asks codebaseOwner
  -- Create a temp table for storing the dependencies we know the calling client already has.
  execute_ [sql| CREATE TEMP TABLE except_causals (causal_id INTEGER NULL ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE except_components ( component_hash_id INTEGER NULL ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE except_namespaces ( branch_hash_ids INTEGER NULL ) ON COMMIT DROP |]
  execute_
    [sql|
    WITH the_causal_hashes(hash) AS (
      SELECT * FROM ^{singleColumnTable (toList exceptCausalHashes)}
    ), known_causal_ids(causal_id) AS (
      SELECT c.id
      FROM the_causal_hashes tch
        JOIN causals c ON tch.hash = c.hash
    ), dependency_hashes(hash) AS (
      SELECT DISTINCT deps.hash
      FROM dependencies_of_causals((SELECT ARRAY_AGG(kci.causal_id) FROM known_causal_ids kci)) AS deps
    ), do_causals AS (
      INSERT INTO except_causals(causal_id)
      SELECT causal.id
      FROM the_causal_hashes tch
        JOIN causals causal ON tch.hash = causal.hash
    ), do_namespaces AS (
      INSERT INTO except_namespaces(branch_hash_ids)
      SELECT bh.id
      FROM dependency_hashes dh
        JOIN branch_hashes bh ON dh.hash = bh.base32
    ) INSERT INTO except_components(component_hash_id)
      SELECT ch.id
      FROM dependency_hashes dh
        JOIN component_hashes ch ON dh.hash = ch.base32
    |]
  cursor <-
    PGCursor.newRowCursor
      "serialized_entities"
      [sql|
        WITH RECURSIVE transitive_causals(causal_id, causal_hash, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.hash, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = #{cid}
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = causal.id)
                  AND NOT EXISTS (SELECT FROM except_causals ec WHERE ec.causal_id = causal.id)
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT tc.causal_id, tc.causal_namespace_hash_id
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.hash, ancestor_causal.namespace_hash_id
                FROM causal_ancestors ca
                    JOIN rec tc ON ca.causal_id = tc.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                    WHERE NOT EXISTS (SELECT FROM except_causals ec WHERE ec.causal_id = ancestor_causal.id)
                UNION
                SELECT child_causal.id, child_causal.hash, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    WHERE NOT EXISTS (SELECT FROM except_causals ec WHERE ec.causal_id = child_causal.id)
            )
        ), all_namespaces(namespace_hash_id, namespace_hash) AS (
            SELECT DISTINCT tc.causal_namespace_hash_id AS namespace_hash_id, bh.base32 as namespace_hash
            FROM transitive_causals tc
            JOIN branch_hashes bh ON tc.causal_namespace_hash_id = bh.id
            WHERE NOT EXISTS (SELECT FROM except_namespaces en WHERE en.branch_hash_ids = tc.causal_namespace_hash_id)
        ), all_patches(patch_id, patch_hash) AS (
            SELECT DISTINCT patch.id, patch.hash
            FROM all_namespaces an
                JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
                JOIN patches patch ON np.patch_id = patch.id
        ),
        -- term components to start transitively joining dependencies to
        base_term_components(component_hash_id) AS (
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN terms term ON nt.term_id = term.id
            UNION
            SELECT DISTINCT term.component_hash_id
            FROM all_patches ap
                JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN terms term ON ptm.to_term_id = term.id
            UNION
            -- term metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_term_metadata meta ON nt.id = meta.named_term
                JOIN terms term ON meta.metadata_term_id = term.id
            UNION
            -- type metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_type_metadata meta ON nt.id = meta.named_type
                JOIN terms term ON meta.metadata_term_id = term.id
            WHERE NOT EXISTS (SELECT FROM except_components ec WHERE ec.component_hash_id = term.component_hash_id)
        ),
        -- type components to start transitively joining dependencies to
        base_type_components(component_hash_id) AS (
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN types typ ON nt.type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN constructors con ON nt.constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN types typ ON ptm.to_type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
                JOIN constructors con ON pcm.to_constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
            WHERE NOT EXISTS (SELECT FROM except_components ec WHERE ec.component_hash_id = typ.component_hash_id)
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
                    WHERE NOT EXISTS (SELECT FROM except_components ec WHERE ec.component_hash_id = ref.component_hash_id)
                UNION
                -- recursively union in type dependencies
                SELECT DISTINCT ref.component_hash_id
                FROM rec atc
                    -- This joins in ALL the types from the component, not just the one that caused the dependency on the
                    -- component
                    JOIN types typ ON atc.component_hash_id = typ.component_hash_id
                    JOIN type_local_component_references ref ON typ.id = ref.type_id
                    WHERE NOT EXISTS (SELECT FROM except_components ec WHERE ec.component_hash_id = ref.component_hash_id)
            )
        )
           (SELECT bytes.bytes, ch.base32
             FROM transitive_components tc
               JOIN serialized_components sc ON sc.user_id = #{ownerUserId} AND tc.component_hash_id = sc.component_hash_id
               JOIN bytes ON sc.bytes_id = bytes.id
               JOIN component_hashes ch ON tc.component_hash_id = ch.id
           )
           UNION ALL
           (SELECT bytes.bytes, ap.patch_hash
             FROM all_patches ap
               JOIN serialized_patches sp ON ap.patch_id = sp.patch_id
               JOIN bytes ON sp.bytes_id = bytes.id
           )
           UNION ALL
           (SELECT bytes.bytes, an.namespace_hash
             FROM all_namespaces an
               JOIN serialized_namespaces sn ON an.namespace_hash_id = sn.namespace_hash_id
               JOIN bytes ON sn.bytes_id = bytes.id
           )
           UNION ALL
           (SELECT bytes.bytes, tc.causal_hash
             FROM transitive_causals tc
               JOIN serialized_causals sc ON tc.causal_id = sc.causal_id
               JOIN bytes ON sc.bytes_id = bytes.id
           )
  |]
  pure cursor

spineAndLibDependenciesOfCausalCursor :: CausalId -> CodebaseM e (PGCursor (Hash32, IsCausalSpine, IsLibRoot))
spineAndLibDependenciesOfCausalCursor cid = do
  ownerUserId <- asks codebaseOwner
  libSegmentTextId <- queryExpect1Col @Int64 [sql| SELECT text.id FROM text WHERE content_hash = text_hash('lib') |]
  PGCursor.newRowCursor
    "causal_dependencies"
    [sql|
    -- is_lib_causal indicates the causal itself is the library, whereas is_lib_root indicates
    -- the causal is the root of a library INSIDE 'lib'
        WITH RECURSIVE transitive_causals(causal_id, causal_hash, causal_namespace_hash_id, is_spine, is_lib_causal, is_lib_root) AS (
            SELECT causal.id, causal.hash, causal.namespace_hash_id, true AS is_spine, false AS is_lib_causal, false AS is_lib_root
            FROM causals causal
                WHERE causal.id = #{cid}
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = causal.id)
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT tc.causal_id, tc.causal_namespace_hash_id, tc.is_spine, tc.is_lib_causal, tc.is_lib_root
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.hash, ancestor_causal.namespace_hash_id, rec.is_spine, rec.is_lib_causal, rec.is_lib_root
                FROM causal_ancestors ca
                    JOIN rec ON ca.causal_id = rec.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                    -- Only get the history of the top level spine
                    WHERE rec.is_spine
                UNION
                SELECT child_causal.id, child_causal.hash, child_causal.namespace_hash_id, false AS is_spine, nc.name_segment_id = #{libSegmentTextId} AS is_lib_causal, rec.is_lib_causal AS is_lib_root
                FROM rec
                    JOIN namespace_children nc ON rec.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    -- Don't sync children of lib roots.
                    WHERE NOT rec.is_lib_root
                          AND (nc.name_segment_id = #{libSegmentTextId} OR rec.is_lib_causal))
            )
           (SELECT tc.causal_hash, tc.is_spine, tc.is_lib_root
             FROM transitive_causals tc
             WHERE tc.is_spine OR tc.is_lib_causal
           )
  |]
    <&> fmap (\(hash, isSpine, isLibRoot) -> (hash, if isSpine then IsCausalSpine else NotCausalSpine, if isLibRoot then IsLibRoot else NotLibRoot))
