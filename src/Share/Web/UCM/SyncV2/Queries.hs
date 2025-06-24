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
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.SyncV2.Types (CBORBytes)

allSerializedDependenciesOfCausalCursor ::
  CausalId ->
  Set CausalHash ->
  CodebaseM
    e
    ( PGCursor (CBORBytes TempEntity, Hash32),
      PGCursor (CBORBytes TempEntity, Hash32),
      PGCursor (CBORBytes TempEntity, Hash32)
    )
allSerializedDependenciesOfCausalCursor cid exceptCausalHashes = do
  ownerUserId <- asks codebaseOwner
  -- Create a temp table for storing the dependencies we know the calling client already has.
  execute_ [sql| CREATE TEMP TABLE except_causals (causal_id INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE except_components ( component_hash_id INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE except_namespaces ( branch_hash_ids INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE components_to_sync ( component_hash_id INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE patches_to_sync ( patch_id INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE namespaces_to_sync ( namespace_hash_id INTEGER PRIMARY KEY ) ON COMMIT DROP |]
  execute_ [sql| CREATE TEMP TABLE causals_to_sync (causal_id INTEGER PRIMARY KEY, causal_hash TEXT ) ON COMMIT DROP |]
  execute_
    [sql|
    WITH the_causal_hashes(hash) AS (
      SELECT * FROM ^{singleColumnTable (toList exceptCausalHashes)}
    ), known_causal_ids(causal_id) AS (
      SELECT c.id
      FROM the_causal_hashes tch
        JOIN causals c ON tch.hash = c.hash
    ), dependency_hashes(hash, kind) AS (
      SELECT DISTINCT deps.hash, deps.kind
      FROM dependencies_of_causals_without_ancestors((SELECT ARRAY_AGG(kci.causal_id) FROM known_causal_ids kci)) AS deps
    )
      INSERT INTO except_causals(causal_id)
      SELECT DISTINCT causal.id
      FROM dependency_hashes dh
        JOIN causals causal ON dh.hash = causal.hash
      WHERE dh.kind = 'causal'::dependency_kind
      ON CONFLICT DO NOTHING
     |]
  numExceptedCausals <- queryExpect1Col @Int64 [sql| SELECT COUNT(*) FROM ^{singleColumnTable (toList exceptCausalHashes)} |]
  Debug.debugM Debug.Temp "populating except hashes for N causal hashes:" (numExceptedCausals)
  numExceptedComponents <- queryExpect1Col @Int64 [sql| SELECT COUNT(*) FROM component_hashes WHERE id IN (SELECT component_hash_id FROM except_components) |]
  Debug.debugM Debug.Temp "populating except hashes for N component hashes:" (numExceptedComponents)
  numExceptedNamespaces <- queryExpect1Col @Int64 [sql| SELECT COUNT(*) FROM branch_hashes WHERE id IN (SELECT branch_hash_ids FROM except_namespaces) |]
  Debug.debugM Debug.Temp "populating except hashes for N namespace hashes:" (numExceptedNamespaces)
  execute_
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
), all_namespaces(namespace_hash_id) AS (
    SELECT DISTINCT tc.causal_namespace_hash_id AS namespace_hash_id
    FROM transitive_causals tc
), all_patches(patch_id) AS (
    SELECT DISTINCT np.patch_id
    FROM all_namespaces an
        JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
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
),
-- Note that this contains type component hash ids as well,
-- it just doesn't crawl those transitively, that's left to transitive_type_component_deps.
transitive_term_component_deps(component_hash_id) AS (
    SELECT DISTINCT btc.component_hash_id
    FROM base_term_components btc
    UNION
    -- recursively union in term dependencies
    SELECT DISTINCT ref.component_hash_id
    FROM transitive_term_component_deps ttd
        JOIN terms term ON ttd.component_hash_id = term.component_hash_id
        JOIN term_local_component_references ref ON term.id = ref.term_id
), transitive_type_component_deps(component_hash_id) AS (
    SELECT DISTINCT btc.component_hash_id
    FROM base_type_components btc
      UNION
    -- recursively union in type dependencies
    SELECT DISTINCT ref.component_hash_id
    FROM transitive_term_component_deps ttd
        JOIN types typ ON ttd.component_hash_id = typ.component_hash_id
        JOIN type_local_component_references ref ON typ.id = ref.type_id
      UNION
    -- recursively union in type dependencies
    SELECT DISTINCT ref.component_hash_id
    FROM transitive_type_component_deps rec
        JOIN types typ ON rec.component_hash_id = typ.component_hash_id
        JOIN type_local_component_references ref ON typ.id = ref.type_id
),
-- All the dependencies we join in transitively from the known term & type components we depend on.
-- Unfortunately it's not possible to know which hashes are terms vs types :'(
transitive_components(component_hash_id) AS (
    SELECT DISTINCT ttcd.component_hash_id
    FROM transitive_term_component_deps ttcd
    UNION
    SELECT DISTINCT ttcd.component_hash_id
    FROM transitive_type_component_deps ttcd
), do_causals AS (
  INSERT INTO causals_to_sync(causal_id, causal_hash)
  SELECT tc.causal_id, tc.causal_hash
  FROM transitive_causals tc
), do_components AS (
  INSERT INTO components_to_sync(component_hash_id)
  SELECT tc.component_hash_id
  FROM transitive_components tc
), do_patches AS (
  INSERT INTO patches_to_sync(patch_id)
  SELECT ap.patch_id
  FROM all_patches ap
)
-- do_namespaces
  INSERT INTO namespaces_to_sync(namespace_hash_id)
  SELECT an.namespace_hash_id
  FROM all_namespaces an
|]

  componentsCursor <-
    PGCursor.newRowCursor @(CBORBytes TempEntity, Hash32, Maybe Int32)
      "serialized_components"
      [sql|
    SELECT bytes.bytes, ch.base32, cd.depth
      FROM components_to_sync cts
        JOIN serialized_components sc ON sc.user_id = #{ownerUserId} AND cts.component_hash_id = sc.component_hash_id
        JOIN bytes ON sc.bytes_id = bytes.id
        JOIN component_hashes ch ON cts.component_hash_id = ch.id
        LEFT JOIN component_depth cd ON ch.id = cd.component_hash_id
    ORDER BY cd.depth ASC NULLS FIRST
  |]
  patchesCursor <-
    PGCursor.newRowCursor @(CBORBytes TempEntity, Hash32, Maybe Int32)
      "serialized_patches"
      [sql|
    SELECT bytes.bytes, p.hash, pd.depth
      FROM patches_to_sync pts
        JOIN patches p ON pts.patch_id = p.id
        JOIN serialized_patches sp ON pts.patch_id = sp.patch_id
        JOIN bytes ON sp.bytes_id = bytes.id
        LEFT JOIN patch_depth pd ON pts.patch_id = pd.patch_id
    ORDER BY pd.depth ASC NULLS FIRST
    |]
  namespacesAndCausalsCursor <-
    PGCursor.newRowCursor @(CBORBytes TempEntity, Hash32, Maybe Int32)
      "serialized_namespaces"
      [sql|
    SELECT bytes, hash, depth FROM (
      SELECT bytes.bytes, bh.base32, nd.depth
        FROM namespaces_to_sync nts
          JOIN branch_hashes bh ON nts.namespace_hash_id = bh.id
          JOIN serialized_namespaces sn ON nts.namespace_hash_id = sn.namespace_hash_id
          JOIN bytes ON sn.bytes_id = bytes.id
          LEFT JOIN namespace_depth nd ON nts.namespace_hash_id = nd.namespace_hash_id
        UNION
      SELECT bytes.bytes, cts.causal_hash, cd.depth
        FROM causals_to_sync cts
          JOIN serialized_causals sc ON cts.causal_id = sc.causal_id
          JOIN bytes ON sc.bytes_id = bytes.id
          LEFT JOIN causal_depth cd ON cts.causal_id = cd.causal_id
    ) AS combined(bytes, hash, depth)
    ORDER BY depth ASC NULLS FIRST
  |]
  pure
    ( componentsCursor <&> \(bytes, hash, _depth) -> (bytes, hash),
      patchesCursor <&> \(bytes, hash, _depth) -> (bytes, hash),
      namespacesAndCausalsCursor <&> \(bytes, hash, _depth) -> (bytes, hash)
    )

-- ( cursor <&> \(bytes, hash, depth) -> case depth of
--     -- This should never happen, but is a sanity check in case we're missing a depth.
--     -- Better than silently omitting a required result.
--     Nothing -> error $ "allSerializedDependenciesOfCausalCursor: Missing depth for entity: " <> show hash
--     Just _ -> (bytes, hash)
-- )

spineAndLibDependenciesOfCausalCursor :: CausalId -> CodebaseM e (PGCursor (Hash32, IsCausalSpine, IsLibRoot))
spineAndLibDependenciesOfCausalCursor cid = do
  ownerUserId <- asks codebaseOwner
  libSegmentTextId <- query1Col @Int64 [sql| SELECT text.id FROM text WHERE content_hash = text_hash('lib') |]
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
      SELECT DISTINCT ON (lib_dep.child_causal_id) lib_dep.child_causal_id, cs.ord
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
      LIMIT 300
  |]
    <&> fmap (\(hash, isSpine, isLibRoot) -> (hash, if isSpine then IsCausalSpine else NotCausalSpine, if isLibRoot then IsLibRoot else NotLibRoot))
