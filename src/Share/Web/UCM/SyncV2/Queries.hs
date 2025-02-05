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
    WITH causal_spine(causal_id, ord) AS (
      -- Empty OVER clause is valid and just numbers the rows in the order they come back,
      -- which is what we want in this case.
      -- Perhaps we can use a proper order-by on causal depth once that's available.
      SELECT ch.causal_id, ROW_NUMBER() OVER () FROM causal_history(#{cid}) AS ch
        WHERE EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = #{ownerUserId} AND co.causal_id = #{cid})
    ), lib_deps(causal_id, ord) AS (
      SELECT DISTINCT ON (lib_dep.child_causal_id), cs.ord
      FROM causal_spine cs
      -- Spinal causal
      -- Root where all library roots are attached
        JOIN causals spine_causal ON spine_causal.id = cs.causal_id
      -- The actual library dependency children
        JOIN namespace_children lib_root_ns ON spine_causal.namespace_hash_id = lib_root_ns.parent_namespace_hash_id
        JOIN causals lib_root_causal ON lib_root_ns.child_causal_id = lib_root_causal.id
        JOIN namespace_children lib_dep ON lib_root_causal.namespace_hash_id = lib_dep.parent_namespace_hash_id
        WHERE lib_root_ns.name_segment_id = #{libSegmentTextId}
        ORDER BY lib_dep.child_causal_id, cs.ord ASC
    )   SELECT c.hash AS hash, true AS is_spine, false AS is_lib, cs.ord AS ord
          FROM causal_spine cs
          JOIN causals c ON cs.causal_id = c.id
        UNION
        SELECT c.hash AS hash, false AS is_spine, true AS is_lib, ld.ord AS ord
          FROM lib_deps ld
          JOIN causals c ON ld.causal_id = c.id
      ORDER BY ord ASC, is_lib ASC, is_spine ASC
  |]
    <&> fmap (\(hash, isSpine, isLibRoot) -> (hash, if isSpine then IsCausalSpine else NotCausalSpine, if isLibRoot then IsLibRoot else NotLibRoot))
