-- Escape strings for use with LIKE
CREATE OR REPLACE FUNCTION like_escape(text)
  RETURNS text
  LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE AS
$func$
SELECT replace(replace(replace($1
         , '\', '\\')  -- must come 1st
         , '%', '\%')
         , '_', '\_');
$func$;

-- E.g. 'foo.bar.baz.' -> '{foo., foo.bar., foo.bar.baz.}'
CREATE OR REPLACE FUNCTION generate_valid_suffixifications(fqn text)
RETURNS text[] AS $$
DECLARE
    segments text[];
    input_segments text[];
    current_segment text;
BEGIN
    input_segments := string_to_array(trim(trailing '.' from fqn), '.');
    segments := ARRAY[]::text[];

    FOREACH current_segment IN ARRAY input_segments
    LOOP
        segments := array_append(segments, array_to_string(input_segments[1:array_position(input_segments, current_segment)], '.') || '.');
    END LOOP;

    RETURN segments;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION is_same_term_hash(term1 scoped_term_name_lookup, term2 scoped_term_name_lookup)
RETURNS boolean AS $$
BEGIN
    RETURN 
      -- If both are not builtins, compare on component info.
      ((term1.referent_builtin IS NULL AND term2.referent_builtin IS NULL)
            AND (term1.referent_component_hash_id = term2.referent_component_hash_id
                    AND term1.referent_component_index = term2.referent_component_index
                    AND term1.referent_constructor_index IS NOT DISTINCT FROM term2.referent_constructor_index
                )
      ) OR
           -- If both are not builtins, the builtin must be equal
           ( (term1.referent_builtin IS NOT NULL AND term2.referent_builtin IS NOT NULL)
            AND term1.referent_builtin = term2.referent_builtin
           );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION is_same_type_hash(type1 scoped_type_name_lookup, type2 scoped_type_name_lookup)
RETURNS boolean AS $$
BEGIN
    RETURN 
      -- If both are not builtins, compare on component info.
      ((type1.reference_builtin IS NULL AND type2.reference_builtin IS NULL)
            AND (type1.reference_component_hash_id = type2.reference_component_hash_id
                    AND type1.reference_component_index = type2.reference_component_index
                )
      ) OR
           -- If both are not builtins, the builtin must be equal
           ( (type1.reference_builtin IS NOT NULL AND type2.reference_builtin IS NOT NULL)
            AND type1.reference_builtin = type2.reference_builtin
           );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION has_name_matching_term_suffixification(root_bh_id integer, namespace_prefix text, reversed_name_prefix text, term_name scoped_term_name_lookup)
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
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION has_name_matching_type_suffixification(root_bh_id integer, namespace_prefix text, reversed_name_prefix text, type_name scoped_type_name_lookup)
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
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION suffixify_term_fqn(root_bh_id integer, namespace_prefix text, mount_path text, term_name scoped_term_name_lookup)
RETURNS text AS $$
DECLARE
    suffixes text[];
    current_suffix text;
    fallback_suffix text := NULL;
BEGIN
    suffixes := generate_valid_suffixifications(term_name.reversed_name || coalesce(mount_path, ''));
    FOR current_suffix IN SELECT unnest(suffixes)
    LOOP
        IF NOT has_name_matching_term_suffixification(root_bh_id, namespace_prefix, current_suffix, term_name)
          THEN
            RETURN current_suffix;
          ELSE
            fallback_suffix := current_suffix;
        END IF;
    END LOOP;

    RETURN fallback_suffix;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION suffixify_type_fqn(root_bh_id integer, namespace_prefix text, mount_path text, type_name scoped_type_name_lookup)
RETURNS text AS $$
DECLARE
    suffixes text[];
    current_suffix text;
    fallback_suffix text := NULL;
BEGIN
    suffixes := generate_valid_suffixifications(type_name.reversed_name || coalesce(mount_path, ''));
    FOR current_suffix IN SELECT unnest(suffixes)
    LOOP
        IF NOT has_name_matching_type_suffixification(root_bh_id, namespace_prefix, current_suffix, type_name)
          THEN
            RETURN current_suffix;
          ELSE
            fallback_suffix := current_suffix;
        END IF;
    END LOOP;

    RETURN fallback_suffix;
END;
$$ LANGUAGE plpgsql;
