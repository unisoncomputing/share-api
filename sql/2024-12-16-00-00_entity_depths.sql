-- We can track the maximum dependency depth of any sub-dag rooted at each entity.
-- The depth of any entity is simply the maximum depth of any of its children plus one.
-- This allows us to trivially sort entities into a valid dependency order without needing a complex topological
-- sort at query time.

-- These are all maintained by triggers on the relevant tables.

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
WHERE nc.namespace_hash_id = NEW.namespace_hash_id
CREATE TABLE patch_depth (
  patch_id INTEGER PRIMARY KEY REFERENCES patches (id) ON DELETE CASCADE,
  depth INTEGER NOT NULL
);


-- Triggers

CREATE OR REPLACE FUNCTION update_causal_depth() RETURNS TRIGGER AS $$
DECLARE
  max_namespace_depth INTEGER;
  max_child_causal_depth INTEGER;
  namespace_id INTEGER;
BEGIN
  -- If there's already a depth entry for this causal, we're done.
  IF EXISTS (SELECT FROM causal_depth WHERE causal_id = NEW.id) THEN
    RETURN NEW;
  END IF;
  -- Find the max depth of the associated namespace
  -- Find the max depth of any child causal
  -- Set the depth of this causal to the max of those two plus one
  SELECT max(nd.depth) INTO max_namespace_depth
    FROM namespace_depth nd
    WHERE nd.namespace_hash_id = NEW.namespace_hash_id;
  SELECT max(cd.depth) INTO max_child_causal_depth
    FROM causal_depth cd
      JOIN causal_ancestors ca ON cd.causal_id = ca.ancestor_id
    WHERE ca.causal_id = NEW.id;
  INSERT INTO causal_depth (causal_id, depth)
    VALUES (NEW.id, GREATEST(max_namespace_depth, max_child_causal_depth) + 1)
    ON CONFLICT (causal_id) DO NOTHING;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER causals_update_causal_depth_trig AFTER INSERT OR UPDATE ON causals
    FOR EACH ROW EXECUTE FUNCTION update_causal_depth();


CREATE OR REPLACE FUNCTION update_component_depth() RETURNS TRIGGER AS $$
DECLARE
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this component, we're done.
  IF EXISTS (SELECT FROM component_depth WHERE component_hash_id = NEW.component_hash_id) THEN
    RETURN NEW;
  END IF;
  -- Find the max depth of any component referenced by this component
  -- Set the depth of this component to that plus one
  SELECT max(refs.depth) INTO max_referenced_component_depth
    FROM (
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN term_local_component_references cr
          ON cd.component_hash_id = cr.component_hash_id
        WHERE cr.component_hash_id = NEW.component_hash_id
      ) UNION
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN type_local_component_references cr
          ON cd.component_hash_id = cr.component_hash_id
        WHERE cr.component_hash_id = NEW.component_hash_id
      )
  ) AS refs;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER component_hashes_update_component_depth_trig AFTER INSERT OR UPDATE ON component_hashes
    FOR EACH ROW EXECUTE FUNCTION update_component_depth();

CREATE OR REPLACE FUNCTION update_namespace_depth() RETURNS TRIGGER AS $$
DECLARE
  max_child_causal_depth INTEGER;
  max_patch_depth INTEGER;
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this namespace, we're done.
  IF EXISTS (SELECT FROM namespace_depth nd WHERE nd.namespace_hash_id = NEW.namespace_hash_id) THEN
    RETURN NEW;
  END IF;
  -- Find the max depth of any child causal
  -- Find the max depth of any patch
  -- Find the max depth of any component referenced by a term, type, or term metadata in this namespace
  -- Set the depth of this namespace to the max of those plus one
  SELECT max(cd.depth) INTO max_child_causal_depth
    FROM causal_depth cd
      JOIN namespace_children nc ON cd.causal_id = nc.child_causal_id
    WHERE nc.parent_namespace_hash_id = NEW.namespace_hash_id;
  SELECT max(pd.depth) INTO max_patch_depth
    FROM patch_depth pd
      JOIN namespace_patches np ON pd.patch_id = np.patch_id
    WHERE np.namespace_hash_id = NEW.namespace_hash_id;
  SELECT max(depth) INTO max_referenced_component_depth
    FROM (
      -- direct term references
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN term_local_component_references cr
          ON cd.component_hash_id = cr.component_hash_id
        JOIN terms t
          ON cr.term_id = t.id
        JOIN namespace_terms nt
          ON t.id = nt.term_id
        WHERE nt.namespace_hash_id = NEW.namespace_hash_id
      ) UNION
      -- term metadata references
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN terms t
          ON cd.component_hash_id = t.component_hash_id
        JOIN namespace_term_metadata ntm
          ON ntm.metadata_term_id = t.id
        JOIN namespace_terms nt
          ON ntm.named_term = nt.id
        WHERE nt.namespace_hash_id = NEW.namespace_hash_id
      ) UNION
      -- direct constructor references
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN constructors c
          ON cd.component_hash_id = c.constructor_type_component_hash_id
        JOIN namespace_terms nt
          ON c.id = nt.constructor_id
        WHERE nt.namespace_hash_id = NEW.namespace_hash_id
      ) UNION
      -- direct type references
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN type_local_component_references cr
          ON cd.component_hash_id = cr.component_hash_id
        JOIN types t
          ON cr.type_id = t.id
        JOIN namespace_types nt
          ON t.id = nt.type_id
        WHERE nt.namespace_hash_id = NEW.namespace_hash_id
      ) UNION
      -- type metadata references
      ( SELECT cd.depth AS depth
        FROM component_depth cd
        JOIN terms t
          ON cd.component_hash_id = t.component_hash_id
        JOIN namespace_type_metadata ntm
          ON ntm.metadata_term_id = t.id
        JOIN namespace_types nt
          ON ntm.named_type = nt.id
        WHERE nt.namespace_hash_id = NEW.namespace_hash_id
      )
  ) AS refs;
  INSERT INTO namespace_depth (namespace_hash_id, depth)
    VALUES (NEW.namespace_hash_id, GREATEST(max_child_causal_depth, max_patch_depth, max_referenced_component_depth) + 1)
    ON CONFLICT (namespace_hash_id) DO NOTHING;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER namespaces_update_namespace_depth_trig AFTER INSERT OR UPDATE ON namespaces
    FOR EACH ROW EXECUTE FUNCTION update_namespace_depth();


CREATE OR REPLACE FUNCTION update_patch_depth() RETURNS TRIGGER AS $$
DECLARE
  max_referenced_component_depth INTEGER;
BEGIN
  -- If there's already a depth entry for this patch, we're done.
  IF EXISTS (SELECT FROM patch_depth WHERE patch_id = NEW.id) THEN
    RETURN NEW;
  END IF;
  -- Find the max depth of any term component referenced by a patch
  -- Find the max depth of any type component referenced by a patch
  -- Set the depth of this patch to that plus one

  SELECT max(cd.depth) INTO max_referenced_component_depth
    FROM (
      -- term references
      ( SELECT from_term_component_hash_id AS component_hash_id
        FROM patch_term_mappings
        WHERE patch_id = NEW.id
      ) UNION
      -- constructor mappings
      ( SELECT from_constructor_component_hash_id AS component_hash_id
        FROM patch_constructor_mappings
        WHERE patch_id = NEW.id
      ) UNION
      -- type references
      ( SELECT from_type_component_hash_id AS component_hash_id
        FROM patch_type_mappings
        WHERE patch_id = NEW.id
      )
    ) AS refs JOIN component_depth cd
      ON cd.component_hash_id = refs.component_hash_id;
  INSERT INTO patch_depth (patch_id, depth)
    VALUES (NEW.id, max_referenced_component_depth + 1)
    ON CONFLICT (patch_id) DO NOTHING;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER patches_update_patch_depth_trig AFTER INSERT OR UPDATE ON patches
    FOR EACH ROW EXECUTE FUNCTION update_patch_depth();