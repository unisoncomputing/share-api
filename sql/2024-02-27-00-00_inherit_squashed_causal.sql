-- Inherit the causals and namespaces reachable from a root causal into your codebase.
-- ONLY adds the causals and namespaces because it's assumed that if we're running a squash the definitions don't
-- change and you already have them in your codebase.
CREATE OR REPLACE FUNCTION inherit_squashed_causal(causal_id_to_copy INTEGER, to_codebase_user_id UUID)
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
                  AND NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = causal.id)
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
                    WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = ancestor_causal.id)
                UNION
                SELECT child_causal.id, child_causal.namespace_hash_id
                FROM rec tc
                    JOIN namespace_children nc ON tc.causal_namespace_hash_id = nc.parent_namespace_hash_id
                    JOIN causals child_causal ON nc.child_causal_id = child_causal.id
                    WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.user_id = to_codebase_user_id AND co.causal_id = child_causal.id)
            )
        ), all_namespaces(namespace_hash_id) AS (
            SELECT DISTINCT causal_namespace_hash_id AS namespace_hash_id
            FROM transitive_causals
            WHERE NOT EXISTS (SELECT FROM namespace_ownership no WHERE no.user_id = to_codebase_user_id AND no.namespace_hash_id = causal_namespace_hash_id)
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
