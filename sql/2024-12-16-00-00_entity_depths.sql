-- We can track the maximum dependency depth of any sub-dag rooted at each entity.
-- The depth of any entity is simply the maximum depth of any of its children plus one.
-- This allows us to trivially sort entities into a valid dependency order without needing a complex topological
-- sort at query time.

-- Unfortunately we can't use triggers for most of these since for some entities their depth is dependent on
-- references which, due to foreign keys, must be inserted AFTER the entity itself, it must be run after all
-- the entity's local references are inserted, but there's no way for us to trigger
-- only when the LAST one of those is done, so we'd need to run this on every
-- local reference insert, and remove the optimistic exit in the case where the row
-- already exists, which is a big waste.
--
-- Instead we just run these functions manually after an entity's references are all inserted.

CREATE TABLE causal_depth (
  causal_id INTEGER PRIMARY KEY REFERENCES causals (id) ON DELETE CASCADE,
  depth INTEGER NOT NULL
);

CREATE TABLE component_depth (
  component_hash_id INTEGER PRIMARY KEY REFERENCES component_hashes (id) ON DELETE CASCADE,
  depth INTEGER NOT NULL
);

CREATE TABLE namespace_depth (
  namespace_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes (id) ON DELETE CASCADE,
  depth INTEGER NOT NULL
);

CREATE TABLE patch_depth (
  patch_id INTEGER PRIMARY KEY REFERENCES patches (id) ON DELETE CASCADE,
  depth INTEGER NOT NULL
);


-- Triggers

CREATE OR REPLACE FUNCTION update_causal_depth(the_causal_id integer) RETURNS VOID AS $$
DECLARE
  max_namespace_depth INTEGER;
  max_child_causal_depth INTEGER;
  the_namespace_hash_id INTEGER;
BEGIN
  -- If there's already a depth entry for this causal, we're done.
  IF EXISTS (SELECT FROM causal_depth cd WHERE cd.causal_id = the_causal_id) THEN
    RETURN;
  END IF;

  SELECT c.namespace_hash_id INTO the_namespace_hash_id
    FROM causals c
    WHERE c.id = the_causal_id;
  -- Find the max depth of the associated namespace
  -- Find the max depth of any child causal
  -- Set the depth of this causal to the max of those two plus one
  SELECT COALESCE(MAX(nd.depth), -1) INTO max_namespace_depth
    FROM namespace_depth nd
    WHERE nd.namespace_hash_id = the_namespace_hash_id;
  SELECT COALESCE(MAX(cd.depth), -1) INTO max_child_causal_depth
    FROM causal_depth cd
      JOIN causal_ancestors ca ON cd.causal_id = ca.ancestor_id
    WHERE ca.causal_id = the_causal_id;
  INSERT INTO causal_depth (causal_id, depth)
    VALUES (the_causal_id, GREATEST(max_namespace_depth, max_child_causal_depth) + 1);

  RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_component_depth(the_component_hash_id integer) RETURNS VOID AS $$
DECLARE
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this component, we're done.
   IF EXISTS (SELECT FROM component_depth cd WHERE cd.component_hash_id = the_component_hash_id) THEN
     RETURN;
   END IF;
  -- Find the max depth of any component referenced by this component
  -- Set the depth of this component to that plus one
  SELECT COALESCE(MAX(refs.depth), -1) INTO max_referenced_component_depth
    FROM (
      ( SELECT cd.depth AS depth
        FROM terms t
        JOIN term_local_component_references cr
          ON cr.term_id = t.id
        JOIN component_depth cd
          ON cd.component_hash_id = cr.component_hash_id
        WHERE t.component_hash_id = the_component_hash_id
      ) UNION
      ( SELECT cd.depth AS depth
        FROM types t
        JOIN type_local_component_references cr
          ON cr.type_id = t.id
        JOIN component_depth cd
          ON cd.component_hash_id = cr.component_hash_id
        WHERE t.component_hash_id = the_component_hash_id
      )
  ) AS refs;
  INSERT INTO component_depth (component_hash_id, depth)
    VALUES (the_component_hash_id, max_referenced_component_depth + 1);
  RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_namespace_depth(the_namespace_hash_id integer) RETURNS VOID AS $$
DECLARE
  max_child_causal_depth INTEGER;
  max_patch_depth INTEGER;
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this namespace, we're done.
  IF EXISTS (SELECT FROM namespace_depth nd WHERE nd.namespace_hash_id = the_namespace_hash_id) THEN
    RETURN;
  END IF;
  -- Find the max depth of any child causal
  -- Find the max depth of any patch
  -- Find the max depth of any component referenced by a term, type, or term metadata in this namespace
  -- Set the depth of this namespace to the max of those plus one
  SELECT COALESCE(MAX(cd.depth), -1) INTO max_child_causal_depth
    FROM causal_depth cd
      JOIN namespace_children nc ON cd.causal_id = nc.child_causal_id
    WHERE nc.parent_namespace_hash_id = the_namespace_hash_id;
  SELECT COALESCE(MAX(pd.depth), -1) INTO max_patch_depth
    FROM patch_depth pd
      JOIN namespace_patches np ON pd.patch_id = np.patch_id
    WHERE np.namespace_hash_id = the_namespace_hash_id;
  SELECT COALESCE(MAX(depth), -1) INTO max_referenced_component_depth
    FROM (
      -- direct term references
      ( SELECT cd.depth AS depth
        FROM namespace_terms nt
        JOIN terms t
          ON nt.term_id = t.id
        JOIN component_depth cd
          ON t.component_hash_id = cd.component_hash_id
        WHERE nt.namespace_hash_id = the_namespace_hash_id
      ) UNION
      -- term metadata references
      ( SELECT cd.depth AS depth
        FROM namespace_terms nt
        JOIN namespace_term_metadata ntm
          ON ntm.named_term = nt.id
        JOIN terms t
          ON ntm.metadata_term_id = t.id
        JOIN component_depth cd
          ON t.component_hash_id = cd.component_hash_id
        WHERE nt.namespace_hash_id = the_namespace_hash_id
      ) UNION
      -- direct constructor references
      ( SELECT cd.depth AS depth
        FROM namespace_terms nt
        JOIN constructors c
          ON c.id = nt.constructor_id
        JOIN types t 
          ON c.type_id = t.id
        JOIN component_depth cd
          ON t.component_hash_id = cd.component_hash_id
        WHERE nt.namespace_hash_id = the_namespace_hash_id
      ) UNION
      -- direct type references
      ( SELECT cd.depth AS depth
        FROM namespace_types nt
        JOIN types t
          ON nt.type_id = t.id
        JOIN component_depth cd
          ON t.component_hash_id = cd.component_hash_id
        WHERE nt.namespace_hash_id = the_namespace_hash_id
      ) UNION
      -- type metadata references
      ( SELECT cd.depth AS depth
        FROM namespace_types nt
        JOIN namespace_type_metadata ntm
          ON ntm.named_type = nt.id
        JOIN terms t
          ON ntm.metadata_term_id = t.id
        JOIN component_depth cd
          ON t.component_hash_id = cd.component_hash_id
        WHERE nt.namespace_hash_id = the_namespace_hash_id
      )
  ) AS refs;
  INSERT INTO namespace_depth (namespace_hash_id, depth)
    VALUES (the_namespace_hash_id, GREATEST(max_child_causal_depth, max_patch_depth, max_referenced_component_depth) + 1);

  RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_patch_depth(the_patch_id integer) RETURNS VOID AS $$
DECLARE
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this patch, we're done.
  IF EXISTS (SELECT FROM patch_depth pd WHERE pd.patch_id = the_patch_id) THEN
    RETURN;
  END IF;
  -- Find the max depth of any term component referenced by a patch
  -- Find the max depth of any type component referenced by a patch
  -- Set the depth of this patch to that plus one

  SELECT COALESCE(MAX(cd.depth), -1) INTO max_referenced_component_depth
    FROM (
      -- term references
      ( SELECT t.component_hash_id AS component_hash_id
        FROM patch_term_mappings ptm
        JOIN terms t
          ON ptm.to_term_id = t.id
        WHERE ptm.patch_id = the_patch_id
      ) UNION
      -- constructor mappings
      ( SELECT t.component_hash_id AS component_hash_id
        FROM patch_constructor_mappings pcm
        JOIN constructors c
          ON pcm.to_constructor_id = c.id
        JOIN types t
          ON c.type_id = t.id
        WHERE pcm.patch_id = the_patch_id
      ) UNION
      -- type references
      ( SELECT t.component_hash_id AS component_hash_id
        FROM patch_type_mappings ptm
        JOIN types t
          ON ptm.to_type_id = t.id
        WHERE ptm.patch_id = the_patch_id
      )
    ) AS refs JOIN component_depth cd
      ON cd.component_hash_id = refs.component_hash_id;
  INSERT INTO patch_depth (patch_id, depth)
    VALUES (the_patch_id, max_referenced_component_depth + 1);

  RETURN;
END;
$$ LANGUAGE plpgsql;
