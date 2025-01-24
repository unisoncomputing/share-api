-- Add the table for the migration queue. This can be deleted after the migration
CREATE TABLE migrate_serialized_queue(
  hash text NOT NULL,
  user_id uuid NOT NULL REFERENCES users(id),

  PRIMARY KEY (user_id, hash)
);

-- Run this manually to populate the queue

-- INSERT INTO migrate_serialized_queue (hash, user_id)
--     SELECT c.hash, co.user_id
--       FROM causal_ownership co
--         JOIN causals c ON co.causal_id = c.id
--   UNION
--     SELECT bh.base32, no.user_id
--       FROM namespace_ownership no
--         JOIN branch_hashes bh ON no.namespace_hash_id = bh.id
--   UNION
--     SELECT p.hash, po.user_id
--     FROM patch_ownership po
--       JOIN patches p ON po.patch_id = p.id
--   UNION
--     SELECT ch.base32, st.user_id
--       FROM sandboxed_terms st
--         JOIN terms t ON st.term_id = t.id
--         JOIN component_hashes ch ON t.component_hash_id = ch.id
--   UNION
--     SELECT ch.base32, st.user_id
--       FROM sandboxed_types st
--         JOIN types t ON st.type_id = t.id
--         JOIN component_hashes ch ON t.component_hash_id = ch.id
--   ON CONFLICT DO NOTHING;
