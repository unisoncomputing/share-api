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
      FROM migrate_serialized_queue q
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM migrate_serialized_queue
      USING chosen_entity
      WHERE migrate_serialized_queue.hash = chosen_entity.hash AND migrate_serialized_queue.user_id = chosen_entity.user_id
    RETURNING chosen_entity.hash, chosen_entity.user_id
    |]

claimComponent :: Transaction e (Maybe (ComponentHashId, UserId))
claimComponent = undefined
