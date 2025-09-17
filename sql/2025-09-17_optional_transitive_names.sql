-- Add an additional argument for whether to include dependencies or transitive dependencies.
CREATE FUNCTION term_names_for_ref(
  arg_bh_id integer,
  arg_namespace_prefix text,
  arg_reversed_name_prefix text,
  arg_should_suffixify boolean,
  arg_referent_builtin text,
  arg_referent_component_hash_id integer,
  arg_referent_component_index bigint,
  arg_referent_constructor_index bigint,
  arg_include_dependencies boolean,
  arg_include_transitive_dependencies boolean
) RETURNS TABLE (
  reversed_name text,
  suffixified_name text
) AS $$
DECLARE
  names name_with_suffix[];
BEGIN
  SELECT array_agg(ROW(names.reversed_name, names.suffixified_name)::name_with_suffix) INTO names
        FROM (
          SELECT stnl.reversed_name,
                 CASE
                   WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, '', ROW(stnl.*))
                   ELSE stnl.reversed_name
                 END AS suffixified_name
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
                AND (arg_namespace_prefix = '' OR namespace LIKE like_escape(arg_namespace_prefix) || '%')
                AND (arg_reversed_name_prefix = '' OR stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
          UNION ALL
          SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name,
            CASE
              WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, mount.reversed_mount_path, ROW(names.*))
              ELSE names.reversed_name || mount.reversed_mount_path
            END AS suffixified_name
          FROM name_lookup_mounts mount
            INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
          WHERE arg_include_dependencies
                AND mount.parent_root_branch_hash_id = arg_bh_id
                AND (arg_namespace_prefix = '' OR mount.mount_path LIKE like_escape(arg_namespace_prefix) || '%')
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
                AND (arg_reversed_name_prefix = '' OR names.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
        ) AS names;

  IF NOT arg_include_transitive_dependencies OR (names IS NOT NULL AND array_length(names, 1) > 0) THEN
    RETURN QUERY
      SELECT n.reversed_name, n.suffixified_name
      FROM unnest(names) AS n(reversed_name, suffixified_name);
  ELSE
    RETURN QUERY
      SELECT (stnl.reversed_name || reversed_mount_path) AS reversed_name,
        CASE
          WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, reversed_mount_path, ROW(stnl.*))
          ELSE stnl.reversed_name || reversed_mount_path
        END AS suffixified_name
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
            ) AND (arg_reversed_name_prefix = '' OR stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
      LIMIT 1;
  END IF;
END;
$$ LANGUAGE plpgsql STABLE;


CREATE FUNCTION type_names_for_ref(
  arg_bh_id integer,
  arg_namespace_prefix text,
  arg_reversed_name_prefix text,
  arg_should_suffixify boolean,
  arg_reference_builtin text,
  arg_reference_component_hash_id integer,
  arg_reference_component_index bigint,
  arg_include_dependencies boolean,
  arg_include_transitive_dependencies boolean
) RETURNS TABLE (
  reversed_name text,
  suffixified_name text
) AS $$
DECLARE
  names name_with_suffix[];
BEGIN
  SELECT array_agg(ROW(names.reversed_name, names.suffixified_name)::name_with_suffix) INTO names
        FROM (
          SELECT stnl.reversed_name,
          CASE
            WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, '', ROW(stnl.*))
            ELSE stnl.reversed_name
          END AS suffixified_name
            FROM scoped_type_name_lookup stnl
          WHERE root_branch_hash_id = arg_bh_id
                -- This may seem overly verbose, but it nudges the query planner to use the
                -- correct partial index, which is keyed on whether the refBuiltin is null or not.
                AND (
                  (arg_reference_builtin IS NULL
                      AND reference_builtin IS NULL
                      AND reference_component_hash_id = arg_reference_component_hash_id
                      AND reference_component_index = arg_reference_component_index
                  )
                  OR
                  ( arg_reference_builtin IS NOT NULL
                    AND reference_builtin = arg_reference_builtin
                  )
                )
                AND (arg_namespace_prefix = '' OR namespace LIKE like_escape(arg_namespace_prefix) || '%')
                AND (arg_reversed_name_prefix = '' OR stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
          UNION ALL
          SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name,
            CASE
              WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, mount.reversed_mount_path, ROW(names.*))
              ELSE names.reversed_name || mount.reversed_mount_path
            END AS suffixified_name
          FROM name_lookup_mounts mount
            INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
          WHERE arg_include_dependencies
                AND mount.parent_root_branch_hash_id = arg_bh_id
                AND (arg_namespace_prefix = '' OR mount.mount_path LIKE like_escape(arg_namespace_prefix) || '%')
                AND (
                  (arg_reference_builtin IS NULL
                      AND reference_builtin IS NULL
                      AND reference_component_hash_id = arg_reference_component_hash_id
                      AND reference_component_index = arg_reference_component_index
                  )
                  OR
                  ( arg_reference_builtin IS NOT NULL
                    AND reference_builtin = arg_reference_builtin
                  )
                ) AND (arg_reversed_name_prefix = '' OR names.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
        ) AS names;

  IF NOT arg_include_transitive_dependencies OR (names IS NOT NULL AND array_length(names, 1) > 0) THEN
    RETURN QUERY
      SELECT n.reversed_name, n.suffixified_name
      FROM unnest(names) AS n(reversed_name, suffixified_name);
  ELSE
    RETURN QUERY
      SELECT (stnl.reversed_name || reversed_mount_path) AS reversed_name,
        CASE
          WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, reversed_mount_path, ROW(stnl.*))
          ELSE stnl.reversed_name || reversed_mount_path
        END AS suffixified_name
        FROM transitive_dependency_mounts(arg_bh_id)
          INNER JOIN scoped_type_name_lookup stnl
            ON stnl.root_branch_hash_id = transitive_dependency_mounts.root_branch_hash_id
      WHERE (
              (arg_reference_builtin IS NULL
                  AND reference_builtin IS NULL
                  AND reference_component_hash_id = arg_reference_component_hash_id
                  AND reference_component_index = arg_reference_component_index
              )
              OR
              ( arg_reference_builtin IS NOT NULL
                AND reference_builtin = arg_reference_builtin
              )
            ) AND (arg_reversed_name_prefix = '' OR stnl.reversed_name LIKE like_escape(arg_reversed_name_prefix) || '%')
      LIMIT 1;
  END IF;
END;
$$ LANGUAGE plpgsql STABLE;
