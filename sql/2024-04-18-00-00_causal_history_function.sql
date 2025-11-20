
-- Return all other causals in the history (a.k.a. spine) of a causal, including itself.
-- This does not include any namespace child causals.
CREATE FUNCTION causal_history(causal_id INTEGER)
RETURNS TABLE (causal_id INTEGER)
AS $$
  WITH RECURSIVE history(causal_id) AS (
      SELECT causal.id
      FROM causals causal
          WHERE causal.id = causal_id
      UNION
      SELECT c.id
      FROM history h
          JOIN causal_ancestors ca ON h.causal_id = ca.causal_id
          JOIN causals c ON ca.ancestor_id = c.id
  ) SELECT h.causal_id FROM history h;
$$ LANGUAGE sql;
