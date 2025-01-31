-- Add the table for the migration queue. This can be deleted after the migration
CREATE TABLE migrate_serialized_queue_sandboxed (
  component_hash_id INTEGER NOT NULL REFERENCES component_hashes(id) ON DELETE CASCADE,
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,

  PRIMARY KEY (component_hash_id, user_id)
);

-- Add the table for the migration queue. This can be deleted after the migration
CREATE TABLE migrate_serialized_queue_unsandboxed (
  hash text NOT NULL PRIMARY KEY,
  -- We still have a user ID here, but it's only used to know which codebase to load things from,
  -- the entities themselves aren't sandboxed
  user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE
);

-- Run this manually to populate the unsandboxed queue

-- INSERT INTO migrate_serialized_queue_unsandboxed (hash, user_id)
--     SELECT DISTINCT ON(c.hash) c.hash, co.user_id
--       FROM causal_ownership co
--         JOIN causals c ON co.causal_id = c.id
--   UNION
--     SELECT DISTINCT ON(bh.base32) bh.base32, no.user_id
--       FROM namespace_ownership no
--         JOIN branch_hashes bh ON no.namespace_hash_id = bh.id
--   UNION
--     SELECT DISTINCT ON(p.hash) p.hash, po.user_id
--     FROM patch_ownership po
--       JOIN patches p ON po.patch_id = p.id;

-- Run this manually to populate the sandboxed queue

-- INSERT INTO migrate_serialized_queue_sandboxed (component_hash_id, user_id)
--     SELECT DISTINCT ON (sd.component_hash_id, sd.component_summary_digest) sd.component_hash_id, sd.user_id
--       FROM user_component_summary_digest sd
--   ON CONFLICT DO NOTHING;


-- AFTER the automated migration is done, you'll need to run the following add the appropriate mappings from user
-- id to serialized component

-- INSERT INTO serialized_components (user_id, component_hash_id, bytes_id)
--   SELECT summaries.user_id AS user_id, summaries.component_hash_id AS component_hash_id, serialized.serialized_component_bytes_id AS bytes_id
--     FROM user_component_summary_digest summaries
--     JOIN component_summary_digests_to_serialized_component_bytes_hash serialized
--       ON summaries.component_summary_digest = serialized.component_summary_digest AND summaries.component_hash_id = serialized.component_hash_id
--   ON CONFLICT DO NOTHING;


--- Sanity Checks, If all has gone well, these should all return 0 or no results respectively.
-- SELECT COUNT(*) FROM migrate_serialized_queue_sandboxed;
-- SELECT COUNT(*) FROM migrate_serialized_queue_unsandboxed;

-- SELECT c.id FROM causals c WHERE NOT EXISTS(SELECT FROM serialized_causals sc WHERE sc.causal_id = c.id);
-- SELECT n.namespace_hash_id FROM namespaces n WHERE NOT EXISTS(SELECT FROM serialized_namespaces sn WHERE sn.namespace_hash_id = n.namespace_hash_id);
-- SELECT p.id FROM patches p WHERE NOT EXISTS(SELECT FROM serialized_patches sp WHERE sp.patch_id = p.id);
-- SELECT st.user_id, t.component_hash_id 
--   FROM sandboxed_terms st 
--   JOIN terms t ON st.term_id = t.id
--   WHERE NOT EXISTS(SELECT FROM serialized_components sc WHERE sc.user_id = st.user_id AND sc.component_hash_id = t.component_hash_id);
-- SELECT st.user_id, t.component_hash_id 
--   FROM sandboxed_types st 
--   JOIN types t ON st.type_id = t.id
--   WHERE NOT EXISTS(SELECT FROM serialized_components sc WHERE sc.user_id = st.user_id AND sc.component_hash_id = t.component_hash_id);


-- EXTRA

-- If some components were somehow missing, this will detect which ones still need processing and add them back to
-- the queue.
-- INSERT INTO migrate_serialized_queue_sandboxed (component_hash_id, user_id)
-- SELECT DISTINCT ON (sd.component_hash_id, sd.component_summary_digest) sd.component_hash_id, sd.user_id
--   FROM user_component_summary_digest sd
--   WHERE NOT EXISTS(SELECT FROM component_summary_digests_to_serialized_component_bytes_hash csd WHERE csd.component_summary_digest = sd.component_summary_digest AND csd.component_hash_id = sd.component_hash_id)
--   ;
