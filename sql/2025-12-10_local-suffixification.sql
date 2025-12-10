-- If the name we're using for a term is in the project, 
-- don't suffixify it against things in lib.

CREATE OR REPLACE FUNCTION term_names_for_ref(
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
                   WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, '', ROW(stnl.*), false)
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
              WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, mount.reversed_mount_path, ROW(names.*), true)
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
          WHEN arg_should_suffixify THEN suffixify_term_fqn(arg_bh_id, arg_namespace_prefix, reversed_mount_path, ROW(stnl.*), true)
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


CREATE OR REPLACE FUNCTION type_names_for_ref(
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
            WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, '', ROW(stnl.*), false)
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
              WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, mount.reversed_mount_path, ROW(names.*), true)
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
          WHEN arg_should_suffixify THEN suffixify_type_fqn(arg_bh_id, arg_namespace_prefix, reversed_mount_path, ROW(stnl.*), true)
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

CREATE OR REPLACE FUNCTION suffixify_term_fqn(root_bh_id integer, namespace_prefix text, arg_reversed_mount_path text, term_name scoped_term_name_lookup, include_deps boolean)
RETURNS text AS $$
DECLARE
    suffixes text[];
    current_suffix text;
    fallback_suffix text := NULL;
BEGIN
    suffixes := generate_valid_suffixifications(term_name.reversed_name || coalesce(arg_reversed_mount_path, ''));
    FOREACH current_suffix IN ARRAY suffixes
    LOOP
        IF NOT has_name_matching_term_suffixification(root_bh_id, namespace_prefix, current_suffix, term_name, include_deps)
          THEN
            RETURN current_suffix;
          ELSE
            fallback_suffix := current_suffix;
        END IF;
    END LOOP;

    RETURN fallback_suffix;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION suffixify_type_fqn(root_bh_id integer, namespace_prefix text, mount_path text, type_name scoped_type_name_lookup, include_deps boolean)
RETURNS text AS $$
DECLARE
    suffixes text[];
    current_suffix text;
    fallback_suffix text := NULL;
BEGIN
    suffixes := generate_valid_suffixifications(type_name.reversed_name || coalesce(mount_path, ''));
    FOREACH current_suffix IN ARRAY suffixes
    LOOP
        IF NOT has_name_matching_type_suffixification(root_bh_id, namespace_prefix, current_suffix, type_name, include_deps)
          THEN
            RETURN current_suffix;
          ELSE
            fallback_suffix := current_suffix;
        END IF;
    END LOOP;

    RETURN fallback_suffix;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION has_name_matching_term_suffixification(root_bh_id integer, namespace_prefix text, reversed_name_prefix text, term_name scoped_term_name_lookup, include_deps boolean)
RETURNS boolean AS $$
BEGIN
  RETURN EXISTS (
    SELECT scoped_term_name_lookup.reversed_name, scoped_term_name_lookup.referent_builtin, scoped_term_name_lookup.referent_component_hash_id, scoped_term_name_lookup.referent_component_index, scoped_term_name_lookup.referent_constructor_index, scoped_term_name_lookup.referent_constructor_type
    FROM scoped_term_name_lookup
    WHERE scoped_term_name_lookup.root_branch_hash_id = root_bh_id
        AND scoped_term_name_lookup.last_name_segment = term_name.last_name_segment
        AND scoped_term_name_lookup.namespace LIKE like_escape(namespace_prefix) || '%'
        AND scoped_term_name_lookup.reversed_name LIKE like_escape(reversed_name_prefix) || '%'
        -- We don't need to consider names for the same definition when suffixifying, so
        -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
        AND NOT is_same_term_hash(scoped_term_name_lookup, term_name)
    -- pull in names from mounted dependencies
    UNION ALL
    SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, names.referent_builtin, referent_component_hash_id, names.referent_component_index, names.referent_constructor_index, names.referent_constructor_type
    FROM name_lookup_mounts mount
    INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
    WHERE mount.parent_root_branch_hash_id = root_bh_id
        AND mount.mount_path LIKE like_escape(namespace_prefix) || '%'
        AND names.last_name_segment = term_name.last_name_segment
        AND (names.reversed_name || mount.reversed_mount_path) LIKE like_escape(reversed_name_prefix) || '%'
        -- We don't need to consider names for the same definition when suffixifying, so
        -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
        AND NOT is_same_term_hash(names, term_name)
        AND include_deps
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION has_name_matching_type_suffixification(root_bh_id integer, namespace_prefix text, reversed_name_prefix text, type_name scoped_type_name_lookup, include_deps boolean)
RETURNS boolean AS $$
BEGIN
  RETURN EXISTS (
  SELECT reversed_name, reference_builtin, reference_component_hash_id, reference_component_index
    FROM scoped_type_name_lookup
  WHERE root_branch_hash_id = root_bh_id
        AND last_name_segment = type_name.last_name_segment
        AND namespace LIKE like_escape(namespace_prefix) || '%'
        AND reversed_name LIKE like_escape(reversed_name_prefix) || '%'
        -- We don't need to consider names for the same definition when suffixifying, so
        -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
        AND NOT is_same_type_hash(scoped_type_name_lookup, type_name)
  UNION ALL
  SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, names.reference_builtin, reference_component_hash_id, names.reference_component_index
  FROM name_lookup_mounts mount
    INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
  WHERE mount.parent_root_branch_hash_id = root_bh_id
        AND mount.mount_path LIKE like_escape(namespace_prefix) || '%'
        AND last_name_segment = type_name.last_name_segment
        AND (names.reversed_name || mount.reversed_mount_path) LIKE like_escape(reversed_name_prefix) || '%'
        -- We don't need to consider names for the same definition when suffixifying, so
        -- we filter those out. Importantly this also avoids matching the name we're trying to suffixify.
        AND NOT is_same_type_hash(names, type_name)
        AND include_deps
  );
END;
$$ LANGUAGE plpgsql;

