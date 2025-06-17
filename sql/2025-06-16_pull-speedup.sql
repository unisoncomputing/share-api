-- Edits the dependencies_of_causals_without_ancestors function to include the dependency kind, which should help
-- avoid unnecessary joins.


-- Create an enum for the dependency kind
CREATE TYPE dependency_kind AS ENUM (
  'component',
  'namespace',
  'patch',
  'causal'
);

DROP FUNCTION IF EXISTS dependencies_of_causals_without_ancestors;

-- Takes a causal_id and returns a table of ALL hashes which are dependencies of that causal, EXCEPT for the
-- ancestors of the causal.
CREATE OR REPLACE FUNCTION dependencies_of_causals_without_ancestors(the_causal_ids INTEGER[]) RETURNS TABLE (hash TEXT, kind dependency_kind) AS $$
  WITH RECURSIVE all_causals(causal_id, causal_hash, causal_namespace_hash_id) AS (
      -- Base causal
      SELECT DISTINCT causal.id, causal.hash, causal.namespace_hash_id
      FROM UNNEST(the_causal_ids) AS causal_id
          JOIN causals causal ON causal.id = causal_id
      UNION
      -- This nested CTE is required because RECURSIVE CTEs can't refer
      -- to the recursive table more than once.
      -- I don't fully understand why or how this works, but it does
      ( WITH rec AS (
          SELECT tc.causal_id, tc.causal_namespace_hash_id
          FROM all_causals tc
      )
          SELECT child_causal.id, child_causal.hash, child_causal.namespace_hash_id
          FROM rec tc
              JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
              JOIN causals child_causal ON nc.child_causal_id = child_causal.id
      )
  ), all_namespaces(namespace_hash_id, namespace_hash) AS (
      SELECT DISTINCT tc.causal_namespace_hash_id AS namespace_hash_id, bh.base32 as namespace_hash
      FROM all_causals tc
      JOIN branch_hashes bh ON tc.causal_namespace_hash_id = bh.id
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
  -- All the dependencies we join in transitively from the known term & type components we depend on.
  all_components(component_hash_id) AS (
      SELECT DISTINCT btc.component_hash_id
      FROM base_term_components btc
      UNION
      SELECT DISTINCT btc.component_hash_id
      FROM base_type_components btc
      UNION
      ( WITH rec AS (
          SELECT DISTINCT ac.component_hash_id
          FROM all_components ac
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
      (SELECT ch.base32 AS hash, 'component'::dependency_kind AS kind
        FROM all_components ac
          JOIN component_hashes ch ON ac.component_hash_id = ch.id
      )
      UNION ALL
      (SELECT ap.patch_hash AS hash, 'patch'::dependency_kind AS kind
        FROM all_patches ap
      )
      UNION ALL
      (SELECT an.namespace_hash AS hash, 'namespace'::dependency_kind AS kind
        FROM all_namespaces an
      )
      UNION ALL
      (SELECT ac.causal_hash AS hash, 'causal'::dependency_kind AS kind
        FROM all_causals ac
      )
$$ LANGUAGE SQL;
