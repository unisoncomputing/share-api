-- Name lookup tables were created before the new PG schema, so they don't actually link in term ids, type ids, or
-- constructor ids. It'll help us join over things if we use the proper foreign keys here.

-- === First the term name lookup: ===

-- Adds term_id and constructor_id to scoped_term_name_lookup.
-- Each row must have exactly one of term_id or constructor_id set.
-- We add a constraint to enforce it.
BEGIN;
ALTER TABLE scoped_term_name_lookup
  ADD COLUMN term_id INTEGER NULL REFERENCES terms(id) ON DELETE CASCADE,
  ADD COLUMN constructor_id INTEGER NULL REFERENCES constructors(id) ON DELETE CASCADE;


-- === Now the type name lookup: ===

-- Adds type_id to scoped_type_name_lookup
ALTER TABLE scoped_type_name_lookup
  ADD COLUMN type_id INTEGER NULL REFERENCES types(id) ON DELETE CASCADE;

-- Replace the old name lookup builder with one that inserts the new columns.
CREATE OR REPLACE FUNCTION add_name_lookup_definitions_at_namespace(root_namespace_hash_id INTEGER, namespace_path TEXT, reversed_namespace_path TEXT, current_namespace_hash_id INTEGER) RETURNS VOID AS $$
BEGIN
    INSERT INTO scoped_term_name_lookup(root_branch_hash_id,
                                        reversed_name,
                                        last_name_segment,
                                        namespace,
                                        referent_builtin,
                                        referent_component_hash_id,
                                        referent_component_index,
                                        referent_constructor_index,
                                        referent_constructor_type,
                                        term_id,
                                        constructor_id
                                       )
    SELECT root_namespace_hash_id,
           n_term_name_segment.text || '.' || reversed_namespace_path,
           n_term_name_segment.text,
           namespace_path,
           builtin.text,
           COALESCE(term.component_hash_id, constructor_type.component_hash_id),
           COALESCE(term.component_index, constructor_type.component_index),
           constr.constructor_index,
           CASE
             WHEN constructor_type.kind = 'data' THEN 0
             WHEN constructor_type.kind = 'ability' THEN 1
             ELSE NULL
           END,
           n_term.term_id,
           n_term.constructor_id

        FROM namespace_terms n_term
          JOIN text n_term_name_segment ON n_term.name_segment_id = n_term_name_segment.id
          LEFT JOIN text builtin ON n_term.builtin_id = builtin.id
          LEFT JOIN terms term ON n_term.term_id = term.id
          LEFT JOIN constructors constr ON n_term.constructor_id = constr.id
          LEFT JOIN types constructor_type ON constr.type_id = constructor_type.id
          WHERE n_term.namespace_hash_id = current_namespace_hash_id;

    INSERT INTO scoped_type_name_lookup(root_branch_hash_id,
                                        reversed_name,
                                        last_name_segment,
                                        namespace,
                                        reference_builtin,
                                        reference_component_hash_id,
                                        reference_component_index,
                                        type_id
                                       )
    SELECT root_namespace_hash_id,
           n_typ_name_segment.text || '.' || reversed_namespace_path,
           n_typ_name_segment.text,
           namespace_path,
           builtin.text,
           typ.component_hash_id,
           typ.component_index,
           n_typ.type_id
        FROM namespace_types n_typ
          JOIN text n_typ_name_segment ON n_typ.name_segment_id = n_typ_name_segment.id
          LEFT JOIN text builtin ON n_typ.builtin_id = builtin.id
          LEFT JOIN types typ ON n_typ.type_id = typ.id
          WHERE n_typ.namespace_hash_id = current_namespace_hash_id;
END;
$$ LANGUAGE plpgsql;
-- Commit table changes quickly so we don't block transactions.
COMMIT;

-- Fill in the new columns with the appropriate values.
BEGIN;
UPDATE scoped_term_name_lookup AS nl
  SET term_id = (SELECT t.id FROM terms t
                   WHERE t.component_hash_id = nl.referent_component_hash_id
                     AND t.component_index = nl.referent_component_index
                  ),
      constructor_id = (SELECT c.id
                          FROM constructors c
                          JOIN types typ ON c.type_id = typ.id
                          WHERE typ.component_hash_id = nl.referent_component_hash_id
                            AND typ.component_index = nl.referent_component_index
                            AND c.constructor_index = nl.referent_constructor_index
                          );
COMMIT;



-- Fill in the new column with the appropriate values.
BEGIN;
UPDATE scoped_type_name_lookup AS nl
  SET type_id = (SELECT typ.id FROM types typ
                   WHERE typ.component_hash_id = nl.reference_component_hash_id
                     AND typ.component_index = nl.reference_component_index
                  );
COMMIT;



-- === Now we add constraints and indexes to the new columns: ===

-- Each row must be either a term or constructor, so one of term_id or constructor_id must be set, OR it must be a builtin.
BEGIN;
ALTER TABLE scoped_term_name_lookup
  ADD CONSTRAINT term_or_constructor_or_builtin CHECK (
    (term_id IS NOT NULL AND constructor_id IS NULL AND referent_builtin IS NULL) OR
    (term_id IS NULL AND constructor_id IS NOT NULL AND referent_builtin IS NULL) OR
    (term_id IS NULL AND constructor_id IS NULL AND referent_builtin IS NOT NULL)
  );

-- We should really have _some_ unique index for this table.
-- Each root should only have a single row for a given name <-> definition pair
CREATE UNIQUE INDEX scoped_term_name_lookup_unique ON scoped_term_name_lookup(root_branch_hash_id, reversed_name, term_id, constructor_id, referent_builtin) NULLS NOT DISTINCT;

-- Each row must have a corresponding type_id set, OR it must be a builtin.
ALTER TABLE scoped_type_name_lookup
  ADD CONSTRAINT type_or_builtin CHECK (
    (type_id IS NOT NULL AND reference_builtin IS NULL) OR
    (type_id IS NULL AND reference_builtin IS NOT NULL)
  );

-- We should really have _some_ unique index for this table.
-- Each root should only have a single row for a given name <-> definition pair
CREATE UNIQUE INDEX scoped_type_name_lookup_key ON scoped_type_name_lookup(root_branch_hash_id, reversed_name, type_id, reference_builtin) NULLS NOT DISTINCT;
COMMIT;
