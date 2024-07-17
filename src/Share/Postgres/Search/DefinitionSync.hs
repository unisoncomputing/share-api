{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Search.DefinitionSync
  ( submitReleaseToBeSynced,
    claimUnsyncedRelease,
  )
where

import Share.BackgroundJobs.Search.DefinitionSync.Types
import Share.IDs (ReleaseId)
import Share.Postgres

submitReleaseToBeSynced :: ReleaseId -> Transaction e ()
submitReleaseToBeSynced releaseId = do
  execute_
    [sql|
    INSERT INTO global_definition_search_release_queue (id)
    VALUES (#{releaseId})
    |]

-- | Claim the oldest unsynced release to be indexed.
claimUnsyncedRelease :: Transaction e (Maybe ReleaseId)
claimUnsyncedRelease = do
  query1Col
    [sql|
    WITH chosen_release AS (
      SELECT q.id
      FROM global_definition_search_release_queue q
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM global_definition_search_release_queue
      USING chosen_release
      WHERE global_definition_search_release_queue.id = chosen_release.id
    RETURNING chosen_release.id
    |]

-- | Save definition documents to be indexed for search.
insertDefinitionDocuments :: [DefinitionDocument] -> Transaction e ()
insertDefinitionDocuments docs = do
  _
