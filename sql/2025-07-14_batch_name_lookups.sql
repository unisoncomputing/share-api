CREATE TYPE name_with_suffix AS (
   reversed_name TEXT,
   suffixified_name TEXT
);

CREATE OR REPLACE FUNCTION transitive_dependency_mounts(arg_root_branch_hash_id integer)
RETURNS TABLE (
  root_branch_hash_id integer,
  reversed_mount_path text
) AS $$
    WITH RECURSIVE
      transitive_dependency_mounts(root_branch_hash_id, reversed_mount_path) AS (
        -- We've already searched direct deps above, so start with children of direct deps
        SELECT transitive.mounted_root_branch_hash_id, transitive.reversed_mount_path || direct.reversed_mount_path
        FROM name_lookup_mounts direct
              JOIN name_lookup_mounts transitive on direct.mounted_root_branch_hash_id = transitive.parent_root_branch_hash_id
        WHERE direct.parent_root_branch_hash_id = arg_root_branch_hash_id
        UNION ALL
        SELECT mount.mounted_root_branch_hash_id, mount.reversed_mount_path || rec.reversed_mount_path
        FROM name_lookup_mounts mount
          INNER JOIN transitive_dependency_mounts rec ON mount.parent_root_branch_hash_id = rec.root_branch_hash_id
    ) SELECT root_branch_hash_id, reversed_mount_path
        FROM transitive_dependency_mounts;
$$ LANGUAGE sql STABLE;

CREATE OR REPLACE FUNCTION term_names_for_ref_within_namespace(
  arg_bh_id integer,
  arg_namespace_prefix text,
  arg_referent_builtin text,
  arg_referent_component_hash_id integer,
  arg_referent_component_index bigint,
  arg_referent_constructor_index bigint,
  arg_reversed_name_prefix text
) RETURNS TABLE (
  reversed_name text,
  suffixified_name text
) AS $$
DECLARE
  names name_with_suffix[];
BEGIN
  SELECT array_agg(ROW(names.reversed_name, names.suffixified_name)::name_with_suffix) INTO names
        FROM (
          SELECT stnl.reversed_name, suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, '', ROW(stnl.*)) AS suffixified_name
            FROM scoped_term_name_lookup stnl
          WHERE root_branch_hash_id = arg_bh_id
                -- This may seem overly verbose, but it nudges the query planner to use the
                -- correct partial index, which is keyed on whether the refBuiltin is null or not.
                AND (
                  (arg_referent_builtin IS NULL
                      AND referent_builtin IS NULL
                      AND referent_component_hash_id = arg_referent_component_hash_id
                      AND referent_component_index = arg_referent_component_index
                      AND referent_constructor_index IS NOT DISTINCT FROM arg_referent_constructor_index
                  )
                  OR
                  ( arg_referent_builtin IS NOT NULL
                    AND referent_builtin = arg_referent_builtin
                  )
                )
                AND namespace LIKE like_escape(arg_namespace_prefix) || '%'
                AND stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%'
          UNION ALL
          SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, mount.reversed_mount_path, ROW(names.*)) AS suffixified_name
          FROM name_lookup_mounts mount
            INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
          WHERE mount.parent_root_branch_hash_id = arg_bh_id
                AND mount.mount_path LIKE like_escape(arg_namespace_prefix) || '%'
                AND (
                  (arg_referent_builtin IS NULL
                      AND referent_builtin IS NULL
                      AND referent_component_hash_id = arg_referent_component_hash_id
                      AND referent_component_index = arg_referent_component_index
                      AND referent_constructor_index IS NOT DISTINCT FROM arg_referent_constructor_index
                  )
                  OR
                  ( arg_referent_builtin IS NOT NULL
                    AND referent_builtin = arg_referent_builtin
                  )
                )
                AND names.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%'
        ) AS names;

  IF names IS NOT NULL AND array_length(names, 1) > 0 THEN
    RETURN QUERY 
      SELECT n.reversed_name, n.suffixified_name 
      FROM unnest(names) AS n(reversed_name, suffixified_name);
  ELSE
    RETURN QUERY
      SELECT (stnl.reversed_name || reversed_mount_path) AS reversed_name, suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, reversed_mount_path, ROW(stnl.*)) AS suffixified_name
        FROM transitive_dependency_mounts(arg_bh_id)
          INNER JOIN scoped_term_name_lookup stnl
            ON stnl.root_branch_hash_id = transitive_dependency_mounts.root_branch_hash_id
      WHERE (
              (arg_referent_builtin IS NULL
                  AND referent_builtin IS NULL
                  AND referent_component_hash_id = arg_referent_component_hash_id
                  AND referent_component_index = arg_referent_component_index
                  AND referent_constructor_index IS NOT DISTINCT FROM arg_referent_constructor_index
              )
              OR
              ( arg_referent_builtin IS NOT NULL
                AND referent_builtin = arg_referent_builtin
              )
            ) AND stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%'
      LIMIT 1;
  END IF;
END;
$$ LANGUAGE plpgsql STABLE;

