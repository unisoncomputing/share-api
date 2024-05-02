-- Inserts all the term, type, and constructors within a given namespace (shallow, non-recursive) into the
-- provided name lookup.
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
                                        referent_constructor_type
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
           END

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
                                        reference_component_index
                                       )
    SELECT root_namespace_hash_id,
           n_typ_name_segment.text || '.' || reversed_namespace_path,
           n_typ_name_segment.text,
           namespace_path,
           builtin.text,
           typ.component_hash_id,
           typ.component_index
        FROM namespace_types n_typ
          JOIN text n_typ_name_segment ON n_typ.name_segment_id = n_typ_name_segment.id
          LEFT JOIN text builtin ON n_typ.builtin_id = builtin.id
          LEFT JOIN types typ ON n_typ.type_id = typ.id
          WHERE n_typ.namespace_hash_id = current_namespace_hash_id;
END;
$$ LANGUAGE plpgsql;

-- Fills in the dependency mounts table for the given branch, e.g. lib.* in project branches or other
-- projects which exist within a loose-code root.
CREATE OR REPLACE FUNCTION create_dependency_mounts(root_namespace_hash_id INTEGER)
  RETURNS TABLE (mount_path TEXT, reversed_mount_path TEXT, namespace_hash_id INTEGER) AS $$
BEGIN
    -- Find all child namespaces so we can determine which ones are dependency mounts:
    RETURN QUERY WITH RECURSIVE recursive_children(name_segment, mount_path, reversed_mount_path, namespace_hash_id, is_dependency_mount) AS (
        SELECT name_segment.text, name_segment.text || '.', name_segment.text || '.', child_causal.namespace_hash_id, false
          FROM namespace_children child
            JOIN text name_segment ON child.name_segment_id = name_segment.id
            JOIN causals child_causal ON child.child_causal_id = child_causal.id
            WHERE child.parent_namespace_hash_id = root_namespace_hash_id
        UNION ALL
        SELECT child_name_segment.text,
               (parent.mount_path || child_name_segment.text || '.'),
               (child_name_segment.text || '.' || parent.reversed_mount_path),
               causal.namespace_hash_id,
               -- This namespace is a mount if its parent is 'lib' or if it has a child named 'lib'
               parent.name_segment = 'lib'
                 OR EXISTS(SELECT FROM namespace_children c
                             JOIN text child_name_segment ON c.name_segment_id = child_name_segment.id
                             WHERE c.parent_namespace_hash_id = causal.namespace_hash_id AND child_name_segment.text = 'lib')
          FROM namespace_children child
            JOIN text child_name_segment ON child.name_segment_id = child_name_segment.id
            JOIN recursive_children parent ON child.parent_namespace_hash_id = parent.namespace_hash_id
            JOIN causals causal ON child.child_causal_id = causal.id
            -- Don't recurse into dependency mounts:
            WHERE NOT parent.is_dependency_mount
    )
    INSERT INTO name_lookup_mounts AS nlm (parent_root_branch_hash_id, mounted_root_branch_hash_id, mount_path, reversed_mount_path)
      SELECT root_namespace_hash_id, rc.namespace_hash_id, rc.mount_path, rc.reversed_mount_path
      FROM recursive_children rc
      WHERE rc.is_dependency_mount
      RETURNING nlm.mount_path, nlm.reversed_mount_path, nlm.mounted_root_branch_hash_id;
END;
$$ LANGUAGE plpgsql;


-- This function will build a name lookup index for a given branch_hash_id.
CREATE OR REPLACE FUNCTION ensure_name_lookup(root_bh_id INTEGER) RETURNS VOID AS $$
DECLARE
    mount_path TEXT;
    reversed_mount_path TEXT;
    namespace_path TEXT;
    reversed_namespace_path TEXT;
    branch_hash_id INTEGER;
    mount RECORD;
BEGIN
    -- Short circuit if the name lookup already exists:
    IF EXISTS (SELECT FROM name_lookups nl WHERE nl.root_branch_hash_id = root_bh_id) THEN
        RETURN;
    ELSE
        IF NOT EXISTS(SELECT FROM namespaces WHERE namespace_hash_id = root_bh_id) THEN
            RAISE EXCEPTION 'Can''t make name lookup, no namespace exists for branch_hash_id: %', root_bh_id;
        END IF;

        -- We need to add the name lookup before populating it because mounts have a foreign key relation to it.
        INSERT INTO name_lookups (root_branch_hash_id) VALUES (root_bh_id);
        -- First ensure all name lookups we depend on exist, creating them if not:
        FOR mount IN SELECT namespace_hash_id FROM create_dependency_mounts(root_bh_id) LOOP
            PERFORM ensure_name_lookup(mount.namespace_hash_id);
        END LOOP;

        -- Recursive query to find all child namespaces which are NOT dependency mounts:
        FOR namespace_path, reversed_namespace_path, branch_hash_id IN
          WITH RECURSIVE child_namespaces(namespace_path, reversed_namespace_path, namespace_hash_id) AS (
            SELECT child_name_segment.text || '.', child_name_segment.text || '.', child_causal.namespace_hash_id
              FROM namespace_children child
                JOIN causals child_causal ON child.child_causal_id = child_causal.id
                JOIN text child_name_segment ON child.name_segment_id = child_name_segment.id
                WHERE child.parent_namespace_hash_id = root_bh_id
                  AND NOT EXISTS(SELECT FROM name_lookup_mounts m
                                  WHERE m.parent_root_branch_hash_id = root_bh_id
                                        AND m.mount_path = child_name_segment.text || '.'
                                )
            UNION ALL
            SELECT (parent.namespace_path || child_name_segment.text || '.'),
                   (child_name_segment.text || '.' || parent.reversed_namespace_path),
                   causal.namespace_hash_id
              FROM namespace_children child
                JOIN child_namespaces parent ON child.parent_namespace_hash_id = parent.namespace_hash_id
                JOIN causals causal ON child.child_causal_id = causal.id
                JOIN text child_name_segment ON child.name_segment_id = child_name_segment.id
                -- Don't recurse into dependency mounts:
                WHERE NOT EXISTS(SELECT FROM name_lookup_mounts m
                                  WHERE m.parent_root_branch_hash_id = root_bh_id
                                        AND m.mount_path = parent.namespace_path || child_name_segment.text || '.'
                                )
        ) SELECT cn.namespace_path, cn.reversed_namespace_path, cn.namespace_hash_id FROM child_namespaces cn
        LOOP

          -- For every namespace, add all the terms, types, and constructors to the name lookup:
          PERFORM add_name_lookup_definitions_at_namespace(root_bh_id, namespace_path, reversed_namespace_path, branch_hash_id);
        END LOOP;
        -- Also add definitions in the root namespace:
        PERFORM add_name_lookup_definitions_at_namespace(root_bh_id, '.', '', root_bh_id);
    END IF;
END;
$$ LANGUAGE plpgsql;
