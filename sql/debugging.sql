CREATE OR REPLACE FUNCTION debug_namespace_hash_id(namespace_hash text)
RETURNS int AS $$
  SELECT id FROM branch_hashes
    WHERE base32 = namespace_hash;
$$ LANGUAGE sql;


-- E.g.
-- SELECT * FROM debug_namespace_hash_id('3z4g5h6j7k8m9n8p');
--                           path                          | causal_id | namespace_hash_id
-- --------------------------------------------------------+-----------+-------------------
--  .public                                                |       226 |               538
--  .public.code                                           |       225 |               536
CREATE OR REPLACE FUNCTION debug_child_namespaces(arg_namespace_hash_id int)
RETURNS TABLE (path text, causal_id int, namespace_hash_id int) AS $$
BEGIN
    RETURN QUERY
        WITH RECURSIVE namespace_tree(path, causal_id, namespace_hash_id) AS (
            SELECT ''::text, NULL::int, arg_namespace_hash_id
            UNION ALL
            SELECT rec.path || '.' || name_segment.text, nc.child_causal_id, child_causal.namespace_hash_id
              FROM namespace_tree rec
                JOIN namespace_children nc ON nc.parent_namespace_hash_id = rec.namespace_hash_id
                JOIN causals child_causal ON child_causal.id = nc.child_causal_id
                JOIN text name_segment ON nc.name_segment_id = name_segment.id
        ) SELECT * FROM namespace_tree nt WHERE nt.causal_id IS NOT NULL;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE VIEW debug_project_branches AS
SELECT CASE WHEN contributor.id IS NULL THEN ('@' || po.handle || '/' || p.slug || '/' || pb.name)
            ELSE ('@' || po.handle || '/' || p.slug || '/@' || contributor.handle || '/' || pb.name)
       END AS shorthand,
       po.handle AS handle,
       p.slug AS slug,
       contributor.handle contributor_handle,
       contributor.id as contributor_id,
       pb.name AS branch_name,
       CASE WHEN contributor.id IS NULL THEN pb.name
            ELSE ('@' || contributor.handle || '/' || pb.name)
       END AS branch_shorthand,
       CASE WHEN contributor.id IS NULL THEN ('@' || po.handle || '/' || p.slug || '/' || pb.name)
            ELSE ('@' || po.handle || '/' || p.slug || '/@' || contributor.handle || '/' || pb.name)
       END AS project_branch_shorthand,
       p.id AS project_id,
       pb.id AS branch_id,
       c.hash AS causal_hash,
       c.id AS causal_id,
       bh.base32 AS namespace_hash,
       c.namespace_hash_id AS namespace_hash_id
FROM project_branches pb
    JOIN projects p ON p.id = pb.project_id
    JOIN users po ON po.id = p.owner_user_id
    LEFT JOIN users contributor ON contributor.id = pb.contributor_id
    JOIN causals c ON c.id = pb.causal_id
    JOIN branch_hashes bh ON bh.id = c.namespace_hash_id;

CREATE OR REPLACE VIEW debug_project_releases AS
SELECT ('@' || po.handle || '/' || p.slug || '/releases/' || (r.major_version::text || '.' || r.minor_version::text || '.' || r.patch_version::text)) AS shorthand,
       po.handle AS handle,
       p.slug AS slug,
       (r.major_version::text || '.' || r.minor_version::text || '.' || r.patch_version::text) AS version,
       p.id AS project_id,
       r.id AS release_id,
       squashed_causal.hash AS squashed_causal_hash,
       squashed_causal.id AS squashed_causal_id,
       unsquashed_causal.hash AS unsquashed_causal_hash,
       unsquashed_causal.id AS unsquashed_causal_id,
       unsquashed_namespace_hash.base32 AS unsquashed_namespace_hash,
       unsquashed_namespace_hash.id AS unsquashed_namespace_id,
       squashed_namespace_hash.base32 AS squashed_namespace_hash,
       squashed_namespace_hash.id AS squashed_namespace_id
FROM project_releases r
    JOIN projects p ON p.id = r.project_id
    JOIN users po ON po.id = p.owner_user_id
    JOIN causals unsquashed_causal ON r.unsquashed_causal_id = unsquashed_causal.id
    JOIN branch_hashes unsquashed_namespace_hash ON unsquashed_namespace_hash.id = unsquashed_causal.namespace_hash_id
    JOIN causals squashed_causal ON r.squashed_causal_id = squashed_causal.id
    JOIN branch_hashes squashed_namespace_hash ON squashed_namespace_hash.id = squashed_causal.namespace_hash_id;


CREATE OR REPLACE VIEW debug_projects AS
SELECT po.handle as handle, p.slug as slug, p.id as project_id
FROM projects p
    JOIN users po ON po.id = p.owner_user_id;

-- BE VERY CAREFUL AND TALK TO CHRIS BEFORE USING THIS
-- Copies ALL dependencies of a causal into the codebase; does NOT assume that dependencies of something in the
-- codebase are already in the codebase (they should be, but this is used for fixing mistakes).
CREATE OR REPLACE FUNCTION debug_force_copy_causal_into_codebase(causal_id_to_copy INTEGER, from_codebase_user_id UUID, to_codebase_user_id UUID)
RETURNS VOID AS $$
DECLARE copied_hash TEXT;
BEGIN
    -- We use a recursive CTE to recursively collect all the dependencies of the causal.
    -- This probably uses a bit more memory than calling functions recursively, but is much more in line
    -- with how Postgres is designed to work. The recursive function approach hit the stack depth limit :|
    -- This will also save us time by not trying to import the same hashes multiple times since we can dedupe them
    -- up-front.
    FOR copied_hash IN
        WITH RECURSIVE transitive_causals(causal_id, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = causal_id_to_copy
                  AND EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = from_codebase_user_id AND co.causal_id = causal.id)
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT causal_id, causal_namespace_hash_id
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.namespace_hash_id
                FROM causal_ancestors ca
                    JOIN rec tc ON ca.causal_id = tc.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                UNION
                SELECT child_causal.id, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
            )
        ), all_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
            FROM transitive_causals
        ), all_patches(patch_id) AS (
            SELECT DISTINCT patch.id
            FROM all_namespaces an
                JOIN namespace_patches np ON an.namespace_hash_id = np.namespace_hash_id
                JOIN patches patch ON np.patch_id = patch.id
        ),
        -- term components to start transitively joining dependencies to
        base_term_components(component_hash_id) AS (
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN terms term ON nt.term_id = term.id
            UNION
            SELECT DISTINCT term.component_hash_id
            FROM all_patches ap
                JOIN patch_term_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN terms term ON ptm.to_term_id = term.id
            UNION
            -- term metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_term_metadata meta ON nt.id = meta.named_term
                JOIN terms term ON meta.metadata_term_id = term.id
            UNION
            -- type metadata
            SELECT DISTINCT term.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN namespace_type_metadata meta ON nt.id = meta.named_type
                JOIN terms term ON meta.metadata_term_id = term.id
        ),
        -- type components to start transitively joining dependencies to
        base_type_components(component_hash_id) AS (
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_types nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN types typ ON nt.type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_namespaces an
                JOIN namespace_terms nt ON an.namespace_hash_id = nt.namespace_hash_id
                JOIN constructors con ON nt.constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_type_mappings ptm ON ap.patch_id = ptm.patch_id
                JOIN types typ ON ptm.to_type_id = typ.id
            UNION
            SELECT DISTINCT typ.component_hash_id
            FROM all_patches ap
                JOIN patch_constructor_mappings pcm ON ap.patch_id = pcm.patch_id
                JOIN constructors con ON pcm.to_constructor_id = con.id
                JOIN types typ ON con.type_id = typ.id
        ),
        -- All the dependencies we join in transitively from the known term & type components we depend on.
        -- Unfortunately it's not possible to know which hashes are terms vs types :'(
        transitive_components(component_hash_id) AS (
            SELECT DISTINCT btc.component_hash_id
            FROM base_term_components btc
            UNION
            SELECT DISTINCT btc.component_hash_id
            FROM base_type_components btc
            UNION
            ( WITH rec AS (
                SELECT component_hash_id
                FROM transitive_components tc
            )
                -- recursively union in term dependencies
                SELECT DISTINCT ref.component_hash_id
                FROM rec atc
                    -- This joins in ALL the terms from the component, not just the one that caused the dependency on the
                    -- component
                    JOIN terms term ON atc.component_hash_id = term.component_hash_id
                    JOIN term_local_component_references ref ON term.id = ref.term_id
                UNION
                -- recursively union in type dependencies
                SELECT DISTINCT ref.component_hash_id
                FROM rec atc
                    -- This joins in ALL the types from the component, not just the one that caused the dependency on the
                    -- component
                    JOIN types typ ON atc.component_hash_id = typ.component_hash_id
                    JOIN type_local_component_references ref ON typ.id = ref.type_id
            )
        ), copied_causals(causal_id) AS (
          INSERT INTO causal_ownership (user_id, causal_id)
            SELECT DISTINCT to_codebase_user_id, tc.causal_id
            FROM transitive_causals tc
            ON CONFLICT DO NOTHING
            RETURNING causal_id
        ), copied_namespaces(namespace_hash_id) AS (
          INSERT INTO namespace_ownership (user_id, namespace_hash_id)
            SELECT DISTINCT to_codebase_user_id, an.namespace_hash_id
            FROM all_namespaces an
            ON CONFLICT DO NOTHING
            RETURNING namespace_hash_id
        ), copied_patches(patch_id) AS (
          INSERT INTO patch_ownership (user_id, patch_id)
            SELECT DISTINCT to_codebase_user_id, ap.patch_id
            FROM all_patches ap
            ON CONFLICT DO NOTHING
            RETURNING patch_id
        ), copied_term_components AS (
              INSERT INTO sandboxed_terms (user_id, term_id, bytes_id)
                SELECT DISTINCT to_codebase_user_id, term.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN terms term ON tc.component_hash_id = term.component_hash_id
                    JOIN sandboxed_terms copy ON term.id = copy.term_id
                    WHERE copy.user_id = from_codebase_user_id
                    ON CONFLICT DO NOTHING
        ), copied_type_components AS (
              INSERT INTO sandboxed_types (user_id, type_id, bytes_id)
                SELECT DISTINCT to_codebase_user_id, typ.id, copy.bytes_id
                FROM transitive_components tc
                    JOIN types typ ON tc.component_hash_id = typ.component_hash_id
                    JOIN sandboxed_types copy ON typ.id = copy.type_id
                    WHERE copy.user_id = from_codebase_user_id
                    ON CONFLICT DO NOTHING
        ) SELECT causal.hash
             FROM copied_causals cc
               JOIN causals causal ON cc.causal_id = causal.id
           UNION ALL
           SELECT branch_hashes.base32
             FROM copied_namespaces cn
               JOIN branch_hashes ON cn.namespace_hash_id = branch_hashes.id
           UNION ALL
           SELECT patch.hash
             FROM copied_patches cp
               JOIN patches patch ON cp.patch_id = patch.id
           UNION ALL
           SELECT component_hashes.base32
             FROM transitive_components tc
               JOIN component_hashes ON tc.component_hash_id = component_hashes.id
               WHERE EXISTS (SELECT FROM temp_entity te WHERE te.user_id = to_codebase_user_id AND te.hash = component_hashes.base32)
                  OR EXISTS (SELECT FROM temp_entity_missing_dependency dep WHERE (dep.user_id = to_codebase_user_id) AND (dep.dependent = component_hashes.base32 OR dep.dependency = component_hashes.base32))
    LOOP
        PERFORM remove_entity_from_temp(to_codebase_user_id, copied_hash);
    END LOOP;
END;
$$ LANGUAGE plpgsql;


-- BE VERY CAREFUL AND TALK TO CHRIS BEFORE USING THIS
-- Inherit the causals and namespaces reachable from a root causal into a codebase.
-- ONLY adds the causals and namespaces, because otherwise you need to specify where to copy terms/types from.
CREATE OR REPLACE FUNCTION debug_force_inherit_causals_and_namespaces(causal_id_to_copy INTEGER, to_codebase_user_id UUID)
RETURNS VOID AS $$
DECLARE copied_hash TEXT;
BEGIN
    -- We use a recursive CTE to recursively collect all the dependencies of the causal.
    -- This probably uses a bit more memory than calling functions recursively, but is much more in line
    -- with how Postgres is designed to work. The recursive function approach hit the stack depth limit :|
    -- This will also save us time by not trying to import the same hashes multiple times since we can dedupe them
    -- up-front.
    FOR copied_hash IN
        WITH RECURSIVE transitive_causals(causal_id, causal_namespace_hash_id) AS (
            SELECT causal.id, causal.namespace_hash_id
            FROM causals causal
                WHERE causal.id = causal_id_to_copy
            UNION
            -- This nested CTE is required because RECURSIVE CTEs can't refer
            -- to the recursive table more than once.
            ( WITH rec AS (
                SELECT causal_id, causal_namespace_hash_id
                FROM transitive_causals tc
            )
                SELECT ancestor_causal.id, ancestor_causal.namespace_hash_id
                FROM causal_ancestors ca
                    JOIN rec tc ON ca.causal_id = tc.causal_id
                    JOIN causals ancestor_causal ON ca.ancestor_id = ancestor_causal.id
                UNION
                SELECT child_causal.id, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
            )
        ), all_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
            FROM transitive_causals
        ), copied_causals(causal_id) AS (
          INSERT INTO causal_ownership (user_id, causal_id)
            SELECT DISTINCT to_codebase_user_id, tc.causal_id
            FROM transitive_causals tc
            ON CONFLICT DO NOTHING
            RETURNING causal_id
        ), copied_namespaces(namespace_hash_id) AS (
          INSERT INTO namespace_ownership (user_id, namespace_hash_id)
            SELECT DISTINCT to_codebase_user_id, an.namespace_hash_id
            FROM all_namespaces an
            ON CONFLICT DO NOTHING
            RETURNING namespace_hash_id
        ) SELECT causal.hash
             FROM copied_causals cc
               JOIN causals causal ON cc.causal_id = causal.id
           UNION ALL
           SELECT branch_hashes.base32
             FROM copied_namespaces cn
               JOIN branch_hashes ON cn.namespace_hash_id = branch_hashes.id
    LOOP
        PERFORM remove_entity_from_temp(to_codebase_user_id, copied_hash);
    END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION debug_history_of_causal(causal_id INTEGER)
RETURNS TABLE (causal_id INTEGER, causal_hash TEXT, namespace_hash_id INTEGER, namespace_hash TEXT)
AS $$
  WITH RECURSIVE history(causal_id, causal_hash, namespace_hash_id, namespace_hash) AS (
      SELECT causal.id, causal.hash, causal.namespace_hash_id, bh.base32
      FROM causals causal
          JOIN branch_hashes bh ON causal.namespace_hash_id = bh.id
          WHERE causal.id = causal_id
      UNION
      SELECT c.id, c.hash, c.namespace_hash_id, bh.base32
      FROM history h
          JOIN causal_ancestors ca ON h.causal_id = ca.causal_id
          JOIN causals c ON ca.ancestor_id = c.id
          JOIN branch_hashes bh ON c.namespace_hash_id = bh.id
  ) SELECT * FROM history;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION debug_is_fast_forward(old_causal_id INTEGER, new_causal_id INTEGER)
RETURNS BOOLEAN AS $$
  SELECT EXISTS (
    SELECT FROM debug_history_of_causal(new_causal_id) history
      WHERE history.causal_id = old_causal_id
  );
$$ LANGUAGE sql;

CREATE OR REPLACE VIEW debug_orgs AS
SELECT o.id AS org_id,
       org_user.name AS org_name,
       org_user.handle AS org_handle,
       o.user_id AS org_user_id,
       o.subject_id AS org_subject_id,
       o.resource_id AS org_resource_id,
       o.created_at AS org_created_at,
       o.updated_at AS org_updated_at,
       o.creator_user_id AS org_creator_user_id,
       creator.handle AS creator_handle,
       o.is_commercial AS is_commercial,
       (SELECT COUNT(*) AS member_count
                 FROM org_members om WHERE om.organization_user_id = o.user_id
              )
FROM orgs o
  JOIN users org_user ON o.user_id = org_user.id
  JOIN users creator ON o.creator_user_id = creator.id
  ;

