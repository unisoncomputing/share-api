{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.UCM.HistoryComments.Queries
  ( projectBranchCommentsCursor,
    insertHistoryComments,
    filterForUnknownHistoryCommentHashes,
    filterForUnknownHistoryCommentRevisionHashes,
  )
where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Share.IDs
import Share.Postgres (whenNonEmpty)
import Share.Postgres qualified as PG
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as PG
import Share.Postgres.IDs
import Share.Prelude
import Share.Web.Authorization (AuthZReceipt)
import Unison.Hash32 (Hash32)
import Unison.Server.HistoryComments.Types

projectBranchCommentsCursor :: AuthZReceipt -> CausalId -> PG.Transaction e (PGCursor (HistoryCommentHash32, HistoryCommentRevisionHash32))
projectBranchCommentsCursor !_authZ causalId = do
  PG.newRowCursor @(HistoryCommentHash32, HistoryCommentRevisionHash32)
    "projectBranchCommentsCursor"
    [PG.sql|
    WITH history(causal_Id) AS (
      SELECT causal_id FROM causal_history(#{causalId})
    ), revisions(id, comment_id, subject, contents, created_at_ms, hidden, revision_hash, comment_hash, author_signature) AS (
      SELECT hcr.id, hc.id, hcr.subject, hcr.content, hcr.created_at_ms, hcr.is_hidden, hcr.revision_hash, hc.comment_hash, hcr.author_signature
            FROM history
            JOIN history_comments hc
              ON hc.causal_id = history.causal_id
            JOIN history_comment_revisions hcr
              ON hcr.comment_id = hc.id
            WHERE
              hc.causal_id IN ()
    ) SELECT hc.comment_hash, hcr.revision_hash
      FROM history
      JOIN history_comments hc
        ON hc.causal_id = history.causal_id
      JOIN causals causal
        ON hc.causal_id = causal.id
      JOIN personal_keys key
        ON hc.author_key_id = key.id
      JOIN LATERAL (
        SELECT * FROM revisions rev
        WHERE rev.comment_id = hc.id
        ORDER BY rev.created_at_ms DESC
        LIMIT 1
      )
    |]

insertThumbprints :: (PG.QueryA m) => NESet Text -> m ()
insertThumbprints thumbprints = do
  PG.execute_
    [PG.sql|
      WITH thumbprints(thumbprint) AS (
        SELECT * FROM ^{PG.singleColumnTable $ Foldable.toList thumbprints}
      )
      INSERT INTO personal_keys (thumbprint)
      SELECT thumbprint FROM thumbprints
      ON CONFLICT DO NOTHING
    |]

-- Convert milliseconds since epoch to UTCTime _exactly_.
-- UTCTime has picosecond precision so this is lossless.
_millisToUTCTime :: Int64 -> UTCTime
_millisToUTCTime ms =
  toRational ms
    & (/ (1_000 :: Rational))
    & fromRational
    & POSIX.posixSecondsToUTCTime

utcTimeToMillis :: UTCTime -> Int64
utcTimeToMillis utcTime =
  POSIX.utcTimeToPOSIXSeconds utcTime
    & toRational
    & (* (1_000 :: Rational))
    & round

insertHistoryComments :: AuthZReceipt -> ProjectId -> [Either HistoryComment HistoryCommentRevision] -> PG.Transaction e ()
insertHistoryComments !_authZ projectId chunks = do
  let thumbprints = NESet.nonEmptySet $ Set.fromList (comments <&> \HistoryComment {authorThumbprint} -> authorThumbprint)
  for thumbprints insertThumbprints
  whenNonEmpty comments $ insertHistoryComments comments
  whenNonEmpty revisions $ insertRevisions revisions
  whenNonEmpty revisions $ insertDiscoveryInfo revisions
  pure ()
  where
    (comments, revisions) =
      chunks & foldMap \case
        Left comment -> ([comment], [])
        Right revision -> ([], [revision])
    insertHistoryComments :: (PG.QueryA m) => [HistoryComment] -> m ()
    insertHistoryComments comments = do
      PG.execute_
        [PG.sql|
          WITH new_comments(author, created_at_ms, author_thumbprint, causal_hash, comment_hash) AS (
            SELECT * FROM ^{PG.toTable commentsTable}
          )
          INSERT INTO history_comments(causal_id, author, created_at_ms, comment_hash, author_key_id)
            SELECT causal.id, nc.author, nc.created_at_ms, nc.comment_hash, pk.id
            FROM new_comments nc
            JOIN causals causal
              ON causal.hash = nc.causal_hash
            JOIN personal_keys pk
              ON pk.thumbprint = nc.author_thumbprint
          ON CONFLICT (comment_hash) DO NOTHING
        |]
      where
        commentsTable :: [(Text, Int64, Text, Hash32, Hash32)]
        commentsTable =
          comments <&> \HistoryComment {..} ->
            ( author,
              utcTimeToMillis createdAt,
              authorThumbprint,
              causalHash,
              commentHash
            )

    insertRevisions :: (PG.QueryA m) => [HistoryCommentRevision] -> m ()
    insertRevisions revs = do
      let doRevs =
            PG.execute_
              [PG.sql|
                WITH new_revisions(subject, contents, created_at_ms, hidden, author_signature, revision_hash, comment_hash) AS (
                  SELECT * FROM ^{PG.toTable revsTable}
                )
                INSERT INTO history_comment_revisions(comment_id, subject, contents, created_at_ms, hidden, author_signature, revision_hash)
                  SELECT hc.id, nr.subject, nr.contents, nr.created_at_ms, nr.hidden, nr.author_signature, nr.revision_hash
                  FROM new_revisions nr
                  JOIN history_comments hc
                    ON hc.comment_hash = nr.comment_hash
                ON CONFLICT DO NOTHING
              |]
          doDiscovery =
            PG.execute_
              [PG.sql|
                  WITH new_discoveries(revision_hash) AS (
                    SELECT * FROM ^{PG.singleColumnTable revHashTable}
                  )
                  INSERT INTO history_comment_revisions_project_discovery(project_id, comment_revision_id)
                    SELECT #{projectId}, hcr.id
                    FROM new_discoveries nd
                    JOIN history_comment_revisions hcr
                      ON hcr.revision_hash = nd.revision_hash
                  ON CONFLICT DO NOTHING
                |]
      doRevs *> doDiscovery
      where
        revsTable =
          revs <&> \HistoryCommentRevision {..} ->
            ( subject,
              content,
              utcTimeToMillis createdAt,
              isHidden,
              authorSignature,
              revisionHash,
              commentHash
            )
        revHashTable = revs <&> \HistoryCommentRevision {..} -> (revisionHash)
    insertDiscoveryInfo :: (PG.QueryA m) => [HistoryCommentRevision] -> m ()
    insertDiscoveryInfo revs = do
      PG.execute_
        [PG.sql|
          WITH new_discoveries(project_id, history_comment_hash) AS (
            SELECT * FROM ^{PG.toTable discoveryTable}
          )
          INSERT INTO history_comment_revisions_project_discovery(project_id, comment_revision_id)
            SELECT #{projectId}, hcr.id
            FROM new_discoveries nd
            JOIN history_comments hc
              ON hc.comment_hash = nd.history_comment_hash
            JOIN history_comment_revisions hcr
              ON hcr.comment_id = hc.id
          ON CONFLICT DO NOTHING
        |]
      where
        discoveryTable :: [(ProjectId, Hash32)]
        discoveryTable =
          revs <&> \HistoryCommentRevision {..} ->
            ( projectId,
              commentHash
            )

filterForUnknownHistoryCommentHashes :: (PG.QueryA m) => [Hash32] -> m [Hash32]
filterForUnknownHistoryCommentHashes commentHashes = do
  PG.queryListCol
    [PG.sql|
      SELECT hash FROM ^{PG.singleColumnTable commentHashes} AS t(hash)
        WHERE NOT EXISTS (
          SELECT FROM history_comments hc
           WHERE hc.comment_hash = t.hash
        )
    |]

filterForUnknownHistoryCommentRevisionHashes :: (PG.QueryA m) => ProjectId -> [Hash32] -> m [Hash32]
filterForUnknownHistoryCommentRevisionHashes projectId revisionHashes = do
  PG.queryListCol
    [PG.sql|
      SELECT hash FROM ^{PG.singleColumnTable revisionHashes} AS t(hash)
        WHERE NOT EXISTS (
          SELECT FROM history_comment_revisions_project_discovery hcrpd
            JOIN history_comment_revisions hcr
              ON hcrpd.comment_revision_id = hcr.id
          WHERE hcrpd.project_id = #{projectId}
            AND hcr.revision_hash = t.hash
        )
    |]
