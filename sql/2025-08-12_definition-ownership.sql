-- Check codebase ownership of definitions by component hash.
CREATE VIEW definition_ownership(component_hash_id, user_id) AS (
  SELECT component_hash_id, st.user_id
    FROM sandboxed_terms st
      JOIN terms term ON st.term_id = term.id
  UNION ALL
  SELECT component_hash_id, st.user_id
    FROM sandboxed_types st
      JOIN types type ON st.type_id = type.id
);

-- These are just simple views to make the intent of checking ownership clearer rather than simply joining into
-- sandboxed_terms
CREATE VIEW term_ownership(term_id, user_id) AS (
  SELECT st.term_id, st.user_id
    FROM sandboxed_terms st
);

-- These are just simple views to make the intent of checking ownership clearer rather than simply joining into
-- sandboxed_types
CREATE VIEW type_ownership(type_id, user_id) AS (
  SELECT st.type_id, st.user_id
    FROM sandboxed_types st
);
