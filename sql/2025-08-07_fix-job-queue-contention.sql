-- We have an issue with new branch creation where we're trying to insert new queue entries, but the row in the queue
-- already exists and so the insert blocks until the job finishes, which can take minutes :'(
-- Instead of using ON CONFLICT DO NOTHING, we now simply use an exists check, which doesn't block.


-- Insert into the queue to be synced using triggers on branches and releases
CREATE OR REPLACE FUNCTION scoped_definition_search_queue_on_branch_change_trigger()
RETURNS TRIGGER AS $$
DECLARE
  already_exists BOOLEAN;
BEGIN
    -- Check if this is an INSERT or if the causal has changed.
    IF TG_OP = 'INSERT' OR (TG_OP = 'UPDATE' AND OLD.causal_id IS DISTINCT FROM NEW.causal_id) THEN
        SELECT EXISTS(
            SELECT 1
            FROM scoped_definition_search_queue q
            WHERE q.root_namespace_hash_id = (
                SELECT c.namespace_hash_id
                FROM causals c
                WHERE c.id = NEW.causal_id
            )
        ) INTO already_exists;
        IF NOT already_exists THEN
          INSERT INTO scoped_definition_search_queue (root_namespace_hash_id, codebase_user_id)
            SELECT c.namespace_hash_id AS root_namespace_hash_id,
                  branch_codebase_owner(NEW.id) AS codebase_user_id
            FROM causals c
            WHERE c.id = NEW.causal_id
            -- This seems redundant to the ON CONFLICT DO NOTHING, but it prevents lock contention
              AND NOT EXISTS (
                  SELECT FROM scoped_definition_search_queue
                  WHERE root_namespace_hash_id = c.namespace_hash_id
              )
            ON CONFLICT DO NOTHING;
          NOTIFY definition_sync;
        END IF;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION scoped_definition_search_queue_on_release_publish_trigger()
RETURNS TRIGGER AS $$
DECLARE 
  already_exists BOOLEAN;
BEGIN
    SELECT EXISTS(
        SELECT 1
        FROM scoped_definition_search_queue q
        WHERE q.root_namespace_hash_id = (
            SELECT c.namespace_hash_id
            FROM causals c
            WHERE c.id = NEW.causal_id
        )
    ) INTO already_exists;
    IF NOT already_exists THEN
        INSERT INTO scoped_definition_search_queue (root_namespace_hash_id, release_id, codebase_user_id)
          SELECT c.namespace_hash_id AS root_namespace_hash_id,
                  NEW.id AS release_id,
                  p.owner_user_id AS codebase_user_id
          FROM causals c
          JOIN projects p ON p.id = NEW.project_id
          WHERE c.id = NEW.squashed_causal_id 
          ON CONFLICT DO NOTHING;
    END IF;
    NOTIFY definition_sync;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

