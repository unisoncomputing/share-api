-- SQL function for finding the lowest common ancestor (LCA) of two causal hashes via breadth-first ancestor search.
-- I.e. the most recent common ancestor of two causal hashes in the causal tree.

-- Finds the best common ancestor(s) between two causals, i.e. to use in a three-way merge or a diff.
-- The best common ancestor is the most recent common ancestor of two causals in the causal tree, which is
-- not the ancestor of any other common ancestor.
-- "Best" is a partial order, there may be more than one "best" common ancestor. 
-- In this case we just return the first one we find.
CREATE OR REPLACE FUNCTION best_common_causal_ancestor(causal_id_one INTEGER, causal_id_two INTEGER)
RETURNS INTEGER AS $$
    WITH RECURSIVE history_one(causal_id) AS (
        SELECT causal_id_one AS causal_id
        UNION
        SELECT ca.ancestor_id
        FROM history_one h
          JOIN causal_ancestors ca ON h.causal_id = ca.causal_id
    ), history_two(causal_id) AS (
        SELECT causal_id_two AS causal_id
        UNION
        SELECT ca.ancestor_id
        FROM history_two h
          JOIN causal_ancestors ca ON h.causal_id = ca.causal_id
    ), common_ancestors(causal_id) AS (
        SELECT h1.causal_id AS causal_id FROM history_one h1
          INTERSECT
        SELECT h2.causal_id AS causal_id FROM history_two h2
        -- Order by most largest causal_id, since that's typically going to be the most recent.
        -- This also serves to make this query deterministic.
        -- But we still filter to ensure it's the best in the next step.
        ORDER BY causal_id DESC
    ) SELECT common.causal_id
        FROM common_ancestors common
        -- Ensure there aren't any better common ancestors than the one we picked.
        WHERE NOT EXISTS (SELECT FROM causal_ancestors ca
                            WHERE ca.ancestor_id = common.causal_id
                              AND EXISTS (SELECT FROM common_ancestors c
                                            WHERE c.causal_id = ca.causal_id)
                         )
        LIMIT 1;
$$ LANGUAGE sql;
