-- Add the table for the migration queue. This can be deleted after the migration
CREATE TABLE migrate_serialized_queue_sandboxed (
  component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,


  PRIMARY KEY (component_hash_id, user_id)
);

-- select digest(ARRAY(select * FROM ( VALUES ('a'), ('b') ) as t) ::text, 'sha256');

-- Add the table for the migration queue. This can be deleted after the migration
CREATE TABLE migrate_serialized_queue_unsandboxed (
  hash text NOT NULL PRIMARY KEY
);

-- Run this manually to populate the unsandboxed queue

-- INSERT INTO migrate_serialized_queue_unsandboxed (hash)
--     SELECT DISTINCT c.hash
--       FROM causals c
--   UNION
--     SELECT DISTINCT bh.base32
--       FROM namespace_ownership no
--         JOIN branch_hashes bh ON no.namespace_hash_id = bh.id
--   UNION
--     SELECT p.hash
--     FROM patch_ownership po
--       JOIN patches p ON po.patch_id = p.id;

-- Run this manually to populate the sandboxed queue

-- INSERT INTO migrate_serialized_queue_sandboxed (component_hash_id, user_id)
--     SELECT DISTINCT t.component_hash_id, st.user_id
--       FROM sandboxed_terms st
--         JOIN terms t ON st.term_id = t.id
--   UNION
--     SELECT DISTINCT t.component_hash_id, st.user_id
--       FROM sandboxed_types st
--         JOIN types t ON st.type_id = t.id
--   ON CONFLICT DO NOTHING;


-- AFTER the automated migration is done, you'll need to run the following add the appropriate mappings from user
-- id to serialized component
-- INSERT INTO serialized_components (user_id, component_hash_id, bytes_id)
--   SELECT summaries.user_id, summaries.component_hash_id, serialized.bytes_id
--     FROM user_component_summary_digest summaries
--     JOIN component_summary_digests_to_serialized_component_bytes_hash serialized 
--       ON summaries.component_summary_digest = serialized.component_summary_digest AND summaries.component_hash_id = serialized.component_hash_id;

