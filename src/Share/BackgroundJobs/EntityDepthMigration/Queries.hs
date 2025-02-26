module Share.BackgroundJobs.EntityDepthMigration.Queries
  ( updateComponentDepths,
    updatePatchDepths,
    updateNamespaceDepths,
    updateCausalDepths,
  )
where

import Data.Int (Int64)
import Share.Postgres

updateComponentDepths :: Transaction e (Int64)
updateComponentDepths = do
  queryExpect1Col
    [sql|
    WITH updatable_components(component_hash_id) AS (
      -- Find all component hashes which aren't missing depth info for any of their
      -- dependencies.
      SELECT ch.id
        FROM unfinished_component_depths ch
        WHERE NOT EXISTS (
            SELECT
              FROM terms t
              JOIN term_local_component_references cr_sub ON cr_sub.term_id = t.id
              LEFT JOIN component_depth cd ON cd.component_hash_id = cr_sub.component_hash_id
            WHERE
              t.component_hash_id = ch.id
              AND cr_sub.term_id = t.id AND cd.depth IS NULL
            UNION
            SELECT
              FROM types t
              JOIN type_local_component_references cr_sub ON cr_sub.type_id = t.id
              LEFT JOIN component_depth cd ON cd.component_hash_id = cr_sub.component_hash_id
            WHERE
              t.component_hash_id = ch.id
              AND cr_sub.type_id = t.id AND cd.depth IS NULL
        )
        LIMIT 1
        FOR UPDATE SKIP LOCKED
    ), updated(component_hash_id, x) AS (
        SELECT ch.component_hash_id, update_component_depth(ch.component_hash_id)
          FROM updatable_components ch
    ), mark_finished AS (
       DELETE FROM unfinished_component_depths ufd
       WHERE ufd.id IN (SELECT u.component_hash_id FROM updated u)
    ) SELECT COUNT(*) FROM updated
    |]

updatePatchDepths :: Transaction e Int64
updatePatchDepths = do
  queryExpect1Col
    [sql|
    WITH updatable_patches(patch_id) AS (
      -- Find all patches which aren't missing depth info for any of their
      -- dependencies.
      SELECT p.id
        FROM unfinished_patch_depths p
        WHERE NOT EXISTS (
            SELECT
              FROM patch_term_mappings ptm
              JOIN terms t
                ON ptm.to_term_id = t.id
              LEFT JOIN component_depth cd
                ON cd.component_hash_id = t.component_hash_id
              WHERE ptm.patch_id = p.id
                AND cd.depth IS NULL
            UNION
            SELECT
              FROM patch_constructor_mappings pcm
              JOIN constructors c
                ON pcm.to_constructor_id = c.id
              JOIN types t
                ON c.type_id = t.id
              LEFT JOIN component_depth cd
                ON cd.component_hash_id = t.component_hash_id
              WHERE pcm.patch_id = p.id
                AND cd.depth IS NULL
            UNION
            SELECT
              FROM patch_type_mappings ptm
              JOIN types t
                ON ptm.to_type_id = t.id
              LEFT JOIN component_depth cd
                ON cd.component_hash_id = t.component_hash_id
              WHERE ptm.patch_id = p.id
                AND cd.depth IS NULL
          )
        LIMIT 1
        FOR UPDATE SKIP LOCKED
    ), updated(patch_id, x) AS (
        SELECT up.patch_id, update_patch_depth(up.patch_id)
          FROM updatable_patches up
    ), mark_finished AS (
       DELETE FROM unfinished_patch_depths ufd
       WHERE ufd.id IN (SELECT u.patch_id FROM updated u)
    ) SELECT COUNT(*) FROM updated
    |]

updateNamespaceDepths :: Transaction e Int64
updateNamespaceDepths = do
  queryExpect1Col
    [sql|
    WITH updatable_namespaces(namespace_hash_id) AS (
      -- Find all namespaces which aren't missing depth info for any of their
      -- dependencies.
      SELECT n.id
        FROM unfinished_namespace_depths n
        WHERE NOT EXISTS (
            SELECT
              FROM namespace_children nc
              JOIN causals c
                ON nc.child_causal_id = c.id
              LEFT JOIN causal_depth cd ON nc.child_causal_id = cd.causal_id
              WHERE nc.parent_namespace_hash_id = n.id
              AND cd.depth IS NULL
            UNION
            SELECT
              FROM namespace_patches np
              LEFT JOIN patch_depth pd ON np.patch_id = pd.patch_id
              WHERE np.namespace_hash_id = n.id
              AND pd.depth IS NULL
            UNION
            SELECT
              FROM namespace_terms nt
              JOIN terms t
                ON nt.term_id = t.id
              LEFT JOIN component_depth cd
                ON t.component_hash_id = cd.component_hash_id
              WHERE nt.namespace_hash_id = n.id
              AND cd.depth IS NULL
            UNION
            SELECT
              FROM namespace_terms nt
              JOIN namespace_term_metadata ntm
                ON ntm.named_term = nt.id
              JOIN terms t
                ON ntm.metadata_term_id = t.id
              LEFT JOIN component_depth cd
                ON t.component_hash_id = cd.component_hash_id
              WHERE nt.namespace_hash_id = n.id
              AND cd.depth IS NULL
            UNION
            SELECT
              FROM namespace_terms nt
              JOIN constructors c
                ON c.id = nt.constructor_id
              JOIN types t
                ON c.type_id = t.id
              LEFT JOIN component_depth cd
                ON t.component_hash_id = cd.component_hash_id
              WHERE nt.namespace_hash_id = n.id
              AND cd.depth IS NULL
            UNION
            SELECT
              FROM namespace_types nt
              JOIN types t
                ON nt.type_id = t.id
              LEFT JOIN component_depth cd
                ON t.component_hash_id = cd.component_hash_id
              WHERE nt.namespace_hash_id = n.id
              AND cd.depth IS NULL
            UNION
            SELECT
              FROM namespace_types nt
              JOIN namespace_type_metadata ntm
                ON ntm.named_type = nt.id
              JOIN terms t
                ON ntm.metadata_term_id = t.id
              LEFT JOIN component_depth cd
                ON t.component_hash_id = cd.component_hash_id
              WHERE nt.namespace_hash_id = n.id
              AND cd.depth IS NULL
          )
        LIMIT 1
        FOR UPDATE SKIP LOCKED
    ), updated(namespace_hash_id, x) AS (
        SELECT un.namespace_hash_id, update_namespace_depth(un.namespace_hash_id)
          FROM updatable_namespaces un
    ), mark_finished AS (
        DELETE FROM unfinished_namespace_depths ufd
        WHERE ufd.id IN (SELECT u.namespace_hash_id FROM updated u)
    ) SELECT COUNT(*) FROM updated
  |]

updateCausalDepths :: Transaction e Int64
updateCausalDepths = do
  queryExpect1Col
    [sql|
    WITH updatable_causals AS (
      -- Find all causals which aren't missing depth info for any of their
      -- dependencies.
      SELECT c.id
        FROM unfinished_causal_depths ucd
        JOIN causals c ON ucd.id = c.id
        WHERE EXISTS (
            SELECT
              FROM namespace_depth nd
              WHERE nd.namespace_hash_id = c.namespace_hash_id
          ) AND NOT EXISTS (
            SELECT
              FROM causal_ancestors ca
              LEFT JOIN causal_depth cd
                ON ca.ancestor_id = cd.causal_id
              WHERE ca.causal_id = c.id
              AND cd.depth IS NULL
        )
        LIMIT 1
        FOR UPDATE SKIP LOCKED
    ), updated(causal_id) AS (
        SELECT c.id, update_causal_depth(c.id)
          FROM updatable_causals c
    ), mark_finished AS (
        DELETE FROM unfinished_causal_depths ucd
        WHERE ucd.id IN (SELECT u.causal_id FROM updated u)
    ) SELECT COUNT(*) FROM updated
  |]

-- Sanity checks
--
-- Should be 0

-- SELECT count(*) from causals
--   WHERE NOT EXISTS (
--   SELECT FROM causal_depth cd
--   WHERE cd.causal_id = causals.id
--   );

-- SELECT count(ch.id)
--   FROM component_hashes ch
--   LEFT JOIN component_depth cd ON cd.component_hash_id = ch.id
--   WHERE cd.depth IS NULL;

-- SELECT COUNT(*)
--   FROM branch_hashes bh
--   LEFT JOIN namespace_depth nd ON nd.namespace_hash_id = bh.id
--   WHERE nd.depth IS NULL;

-- SELECT COUNT(*)
--  FROM patches p
--  LEFT JOIN patch_depth pd ON pd.patch_id = p.id
--  WHERE pd.depth IS NULL;
