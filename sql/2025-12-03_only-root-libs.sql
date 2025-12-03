-- Replace existing dependency_mounts function to only create 'lib' mounts at the root level.
CREATE OR REPLACE FUNCTION create_dependency_mounts(root_namespace_hash_id INTEGER)
  RETURNS TABLE (mount_path TEXT, reversed_mount_path TEXT, namespace_hash_id INTEGER) AS $$
BEGIN
    -- Find all child namespaces so we can determine which ones are dependency mounts:
    RETURN QUERY WITH lib_mounts(mount_path, reversed_mount_path, namespace_hash_id) AS (
        SELECT 'lib.' || child_name_segment.text || '.', child_name_segment.text || '.lib.', child_causal.namespace_hash_id
          FROM namespace_children lib
            JOIN text lib_name_segment ON lib.name_segment_id = lib_name_segment.id
            JOIN causals lib_causal ON lib.child_causal_id = lib_causal.id
            JOIN namespace_children child_namespace ON lib_causal.namespace_hash_id = child_namespace.parent_namespace_hash_id
            JOIN causals child_causal ON child_namespace.child_causal_id = child_causal.id
            JOIN text child_name_segment ON child_namespace.name_segment_id = child_name_segment.id
            WHERE lib.parent_namespace_hash_id = root_namespace_hash_id
              AND lib_name_segment.text = 'lib'
    )
    INSERT INTO name_lookup_mounts AS nlm (parent_root_branch_hash_id, mounted_root_branch_hash_id, mount_path, reversed_mount_path)
      SELECT root_namespace_hash_id, lib_mounts.namespace_hash_id, lib_mounts.mount_path, lib_mounts.reversed_mount_path
      FROM lib_mounts 
      RETURNING nlm.mount_path, nlm.reversed_mount_path, nlm.mounted_root_branch_hash_id;
END;
$$ LANGUAGE plpgsql;
