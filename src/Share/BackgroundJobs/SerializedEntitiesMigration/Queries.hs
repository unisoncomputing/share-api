module Share.BackgroundJobs.SerializedEntitiesMigration.Queries
  ( claimEntity,
    claimComponent,
  )
where

import Share.IDs
import Share.Postgres
import Share.Postgres.IDs
import Unison.Hash32

claimEntity :: Transaction e (Maybe (Hash32, UserId))
claimEntity = do
  query1Row
    [sql|
    WITH chosen_entity(hash, user_id) AS (
      SELECT q.hash, q.user_id
      FROM migrate_serialized_queue_unsandboxed q
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM migrate_serialized_queue_unsandboxed
      USING chosen_entity
      WHERE migrate_serialized_queue_unsandboxed.hash = chosen_entity.hash AND migrate_serialized_queue_unsandboxed.user_id = chosen_entity.user_id
    RETURNING chosen_entity.hash, chosen_entity.user_id
    |]

claimComponent :: Transaction e (Maybe (ComponentHashId, UserId))
claimComponent = do
  query1Row
    [sql|
    WITH chosen_component(component_hash_id, user_id) AS (
      SELECT q.component_hash_id, q.user_id
      FROM migrate_serialized_queue_sandboxed q
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM migrate_serialized_queue_sandboxed
      USING chosen_component
      WHERE migrate_serialized_queue_sandboxed.component_hash_id = chosen_component.component_hash_id 
        AND migrate_serialized_queue_sandboxed.user_id = chosen_component.user_id

    RETURNING chosen_component.component_hash_id, chosen_component.user_id
    |]
