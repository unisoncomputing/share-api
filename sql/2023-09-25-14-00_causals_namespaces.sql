CREATE TABLE causals (
    id SERIAL,
    hash TEXT NOT NULL,
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE NO ACTION,
    PRIMARY KEY (id) INCLUDE(hash, namespace_hash_id)
);

CREATE UNIQUE INDEX causals_by_hash ON causals(hash text_pattern_ops) INCLUDE (id, namespace_hash_id);

CREATE INDEX causals_by_namespace_hash_id ON causals(namespace_hash_id) INCLUDE (id);

CREATE TABLE causal_ancestors (
    causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE,
    ancestor_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE NO ACTION,
    PRIMARY KEY (causal_id, ancestor_id)
);

CREATE INDEX causal_ancestors_by_ancestor_id ON causal_ancestors(ancestor_id);

CREATE TABLE causal_ownership (
    causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE NO ACTION,
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    PRIMARY KEY (user_id, causal_id)
);

CREATE TABLE namespaces (
    namespace_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes(id) ON DELETE NO ACTION,
    contained_terms INTEGER NOT NULL,
    deep_contained_terms INTEGER NOT NULL,
    contained_types INTEGER NOT NULL,
    deep_contained_types INTEGER NOT NULL,
    contained_constructors INTEGER NOT NULL,
    deep_contained_constructors INTEGER NOT NULL
);

CREATE TABLE namespace_ownership (
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE NO ACTION,
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    PRIMARY KEY (user_id, namespace_hash_id)
);

CREATE TABLE namespace_terms (
    -- id used to associate metadata
    id SERIAL PRIMARY KEY,
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    name_segment_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,
    builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    term_id INTEGER NULL REFERENCES terms(id) ON DELETE NO ACTION,
    constructor_id INTEGER NULL REFERENCES constructors(id) ON DELETE NO ACTION,

    CHECK (
       ( builtin_id IS NOT NULL AND term_id IS NULL AND constructor_id IS NULL)
         OR (builtin_id IS NULL AND term_id IS NOT NULL AND constructor_id IS NULL)
         OR (builtin_id IS NULL AND term_id IS NULL AND constructor_id IS NOT NULL)
      )
);

-- Historically we DO allow multiple terms with the same name_segment if they're different.
-- This may change in the future, but we need to allow it for backwards compatibility.
CREATE UNIQUE INDEX namespace_terms_by_name_and_ref ON namespace_terms(namespace_hash_id, name_segment_id, builtin_id, term_id, constructor_id)
  NULLS NOT DISTINCT;

CREATE TABLE namespace_term_metadata (
    id SERIAL PRIMARY KEY,
    named_term INTEGER NOT NULL REFERENCES namespace_terms(id) ON DELETE CASCADE,

    metadata_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    metadata_term_id INTEGER NULL REFERENCES terms(id) ON DELETE NO ACTION,

    CHECK (
        metadata_builtin_id IS NOT NULL AND metadata_term_id IS NULL
        OR metadata_builtin_id IS NULL AND metadata_term_id IS NOT NULL
    )
);

CREATE UNIQUE INDEX namespace_term_metadata_by_named_term_and_ref ON namespace_term_metadata(named_term, metadata_builtin_id, metadata_term_id)
  NULLS NOT DISTINCT;


CREATE TABLE namespace_types (
    -- id used to associate metadata
    id SERIAL PRIMARY KEY,
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    name_segment_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,
    builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    type_id INTEGER NULL REFERENCES types(id) ON DELETE NO ACTION,

    CHECK
      ((builtin_id IS NOT NULL AND type_id IS NULL)
        OR (type_id IS NOT NULL AND builtin_id IS NULL)
      )
);

CREATE UNIQUE INDEX namespace_types_by_name_and_ref ON namespace_types(namespace_hash_id, name_segment_id, builtin_id, type_id)
  NULLS NOT DISTINCT;

CREATE TABLE namespace_type_metadata (
    id SERIAL PRIMARY KEY,
    named_type INTEGER NOT NULL REFERENCES namespace_types(id) ON DELETE CASCADE,

    metadata_builtin_id INTEGER NULL REFERENCES text(id) ON DELETE NO ACTION,
    metadata_term_id INTEGER NULL REFERENCES terms(id) ON DELETE NO ACTION,

    CHECK (
        metadata_builtin_id IS NOT NULL AND metadata_term_id IS NULL
        OR metadata_builtin_id IS NULL AND metadata_term_id IS NOT NULL
    )
);

CREATE UNIQUE INDEX namespace_type_metadata_by_named_type_and_ref ON namespace_type_metadata(named_type, metadata_builtin_id, metadata_term_id)
  NULLS NOT DISTINCT;

CREATE TABLE namespace_patches (
    id SERIAL PRIMARY KEY,
    namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    name_segment_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,
    patch_id INTEGER NOT NULL REFERENCES patches(id) ON DELETE NO ACTION
);

-- Historically we DO allow multiple patches with the same name_segment if they have different hashes.
CREATE UNIQUE INDEX namespace_patches_by_name_and_patch_id ON namespace_patches(namespace_hash_id, name_segment_id, patch_id);

CREATE TABLE namespace_children (
    id SERIAL PRIMARY KEY,
    parent_namespace_hash_id INTEGER NOT NULL REFERENCES branch_hashes(id) ON DELETE CASCADE,
    name_segment_id INTEGER NOT NULL REFERENCES text(id) ON DELETE NO ACTION,
    child_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE NO ACTION
);

-- We DON'T allow multiple children with the same name_segment.
CREATE UNIQUE INDEX namespace_children_by_name_and_name_segment_id ON namespace_children(parent_namespace_hash_id, name_segment_id) INCLUDE(child_causal_id);

CREATE TABLE squash_results (
    -- Pre-squash branch hash
    -- There should only ever be one result for each unsquashed value hash.
    unsquashed_branch_hash_id INTEGER PRIMARY KEY REFERENCES branch_hashes(id) ON DELETE CASCADE,
    squashed_causal_id INTEGER NOT NULL REFERENCES causals(id) ON DELETE CASCADE
);

-- Helper functions

CREATE OR REPLACE FUNCTION causal_at_path(root_causal_id INTEGER, path TEXT[])
RETURNS INTEGER AS $$
DECLARE
    current_causal_id INTEGER;
    path_segment TEXT;
BEGIN
    current_causal_id := root_causal_id;
    FOREACH path_segment IN ARRAY path LOOP
        SELECT child_causal_id INTO current_causal_id
          FROM causals causal
           JOIN namespace_children child ON causal.namespace_hash_id = child.parent_namespace_hash_id
           JOIN text path_segment_text ON path_segment_text.id = child.name_segment_id
          WHERE causal.id = current_causal_id
            AND child.name_segment_id = path_segment_text.id
            AND path_segment_text.text = path_segment;
    END LOOP;
    RETURN current_causal_id;
END;
$$ LANGUAGE plpgsql;

-- Compute the number of terms, types, and constructors in a namespace and its children
-- This must be run AFTER inserting all elements of the namespace itself, and all of its
-- children.
CREATE OR REPLACE FUNCTION save_namespace(current_namespace_hash_id INTEGER)
RETURNS VOID AS $$
DECLARE
  term_count INTEGER;
  type_count INTEGER;
  constructor_count INTEGER;
  child_counts RECORD;
BEGIN
    SELECT COUNT(*) INTO term_count FROM namespace_terms WHERE namespace_hash_id = current_namespace_hash_id;
    SELECT COUNT(*) INTO type_count FROM namespace_types WHERE namespace_hash_id = current_namespace_hash_id;
    SELECT COUNT(*) INTO constructor_count FROM namespace_patches WHERE namespace_hash_id = current_namespace_hash_id;
    SELECT COALESCE(SUM(child_namespace.deep_contained_terms), 0) AS child_contained_terms,
           COALESCE(SUM(child_namespace.deep_contained_types), 0) AS child_contained_types, 
           COALESCE(SUM(child_namespace.deep_contained_constructors), 0) AS child_contained_constructors
      INTO child_counts
      FROM namespace_children child_namespace_mapping
        JOIN causals child_causal ON child_namespace_mapping.child_causal_id = child_causal.id
        JOIN namespaces child_namespace ON child_causal.namespace_hash_id = child_namespace.namespace_hash_id
      WHERE child_namespace_mapping.parent_namespace_hash_id = current_namespace_hash_id;

    INSERT INTO namespaces (
        namespace_hash_id,
        contained_terms,
        deep_contained_terms,
        contained_types,
        deep_contained_types,
        contained_constructors,
        deep_contained_constructors
    )
      VALUES (
        current_namespace_hash_id,
        term_count,
        term_count + child_counts.child_contained_terms,
        type_count,
        type_count + child_counts.child_contained_types,
        constructor_count,
        constructor_count + child_counts.child_contained_constructors
    );
END;
$$ LANGUAGE plpgsql;
