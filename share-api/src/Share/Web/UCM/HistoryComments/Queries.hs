{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.UCM.HistoryComments.Queries
  ( fetchProjectBranchCommentsSince,
    insertHistoryComments,
  )
where

import Control.Lens
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

fetchProjectBranchCommentsSince :: AuthZReceipt -> ProjectId -> CausalId -> UTCTime -> PG.Transaction e (PGCursor HistoryCommentChunk)
fetchProjectBranchCommentsSince !_authZ projectId causalId sinceTime = do
  PG.newRowCursor @(Bool, Maybe Text, Maybe Text, Maybe Int64, Maybe Bool, Maybe ByteString, Maybe Hash32, Maybe Hash32, Maybe Text, Maybe Int64, Maybe Text, Maybe Hash32, Maybe Hash32)
    "fetchProjectBranchCommentsSince"
    [PG.sql|
    WITH revisions(id, comment_id, subject, contents, created_at_ms, hidden, revision_hash, comment_hash, author_signature) AS (
      SELECT hcr.id, hc.id, hcr.subject, hcr.content, hcr.created_at_ms, hcr.is_hidden, hcr.revision_hash, hc.comment_hash, hcr.author_signature
            history_comment_revisions_project_discovery pd
            JOIN history_comment_revisions hcr
              ON pd.history_comment_revision_id = hcr.id
            JOIN history_comments hc
              ON hcr.history_comment_id = hc.id
            WHERE
              pd.project_id = #{projectId}
              AND pd.discovered_at > #{sinceTime}
              AND hc.causal_id IN (SELECT causal_id FROM causal_history(#{causalId}))
    ) (SELECT true, NULL, NULL, NULL, NULL, NULL, NULL, NULL, hc.author, hc.created_at_ms, key.thumbprint, causal.hash, hc.comment_hash
      FROM revisions rev
      JOIN history_comments hc
        ON revisions.comment_id = hc.id
      JOIN causals causal
        ON hc.causal_id = causal.id
      JOIN personal_keys key
        ON hc.author_key_id = key.id
      )
      UNION ALL
      -- Include ALL the base comments regardless of time,
      -- the vast majority of the time we'll need them, it simplifies logic,
      -- and the client can just ignore them if they already have them.
      (SELECT DISTINCT ON (rev.comment_id)
        false, rev.subject, rev.content, rev.created_at_ms, rev.is_hidden, rev.author_signature, rev.revision_hash, rev.comment_hash, NULL, NULL, NULL, NULL, NULL
      FROM revisions rev
      )
    |]
    <&> fmap \case
      (True, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just author, Just createdAtMs, Just authorThumbprint, Just causalHash, Just commentHash) ->
        let createdAt = millisToUTCTime createdAtMs
         in HistoryCommentChunk $ HistoryComment {..}
      (False, Just subject, Just content, Just createdAtMs, Just isHidden, Just authorSignature, Just revisionHash, Just commentHash, Nothing, Nothing, Nothing, Nothing, Nothing) ->
        let createdAt = millisToUTCTime createdAtMs
         in HistoryCommentRevisionChunk $ HistoryCommentRevision {..}
      row -> error $ "fetchProjectBranchCommentsSince: Unexpected row format: " <> show row

insertThumbprints :: (PG.QueryA m) => NESet Text -> m ()
insertThumbprints thumbprints = do
  PG.execute_
    [PG.sql|
      INSERT INTO personal_keys (thumbprint)
      SELECT * FROM ^{PG.singleColumnTable $ toList thumbprints}
      ON CONFLICT (thumbprint) DO NOTHING
    |]

-- Convert milliseconds since epoch to UTCTime _exactly_.
-- UTCTime has picosecond precision so this is lossless.
millisToUTCTime :: Int64 -> UTCTime
millisToUTCTime ms =
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

insertHistoryComments :: AuthZReceipt -> ProjectId -> [HistoryCommentChunk] -> PG.Transaction e ()
insertHistoryComments !_authZ projectId chunks = PG.pipelined $ do
  let thumbprints = NESet.nonEmptySet $ Set.fromList (comments <&> \HistoryComment {authorThumbprint} -> authorThumbprint)
  for thumbprints insertThumbprints
  whenNonEmpty comments $ insertHistoryComments comments
  whenNonEmpty revisions $ insertRevisions revisions
  whenNonEmpty revisions $ insertDiscoveryInfo revisions
  pure ()
  where
    (comments, revisions) =
      chunks & foldMap \case
        HistoryCommentChunk comment -> ([comment], [])
        HistoryCommentRevisionChunk revision -> ([], [revision])
        HistoryCommentErrorChunk err -> error $ "HistoryCommentErrorChunk: " <> show err -- TODO Handle this
    insertHistoryComments :: [HistoryComment] -> PG.Pipeline e ()
    insertHistoryComments comments = do
      PG.execute_
        [PG.sql|
          WITH new_comments(author, created_at_ms, author_thumbprint, causal_hash, comment_hash) AS (
            VALUES ^{PG.toTable commentsTable}
          )
          INSERT INTO history_comments(causal_id, author, created_at_ms, comment_hash, author_key_id)
            SELECT causal.id, nc.author, nc.created_at_ms, nc.comment_hash, pk.id
            FROM new_comments nc
            JOIN causal causal
              ON causal.hash = nc.causal_hash
            JOIN personal_keys pk
              ON pk.thumbprint = nc.author_thumbprint
          ON CONFLICT DO NOTHING
        |]
      where
        commentsTable =
          comments <&> \HistoryComment {..} ->
            ( author,
              utcTimeToMillis createdAt,
              authorThumbprint,
              causalHash,
              commentHash
            )

    insertRevisions :: [HistoryCommentRevision] -> PG.Pipeline e ()
    insertRevisions revs = do
      let doRevs =
            PG.execute_
              [PG.sql|
                WITH new_revisions(subject, content, created_at_ms, hidden, author_signature, revision_hash, comment_hash) AS (
                  VALUES ^{PG.toTable revsTable}
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
                    VALUES ^{PG.singleColumnTable revHashTable}
                  )
                  INSERT INTO history_comment_revisions_project_discovery(project_id, history_comment_revision_id)
                    SELECT #{projectId}, hcr.id
                    FROM new_discoveries nd
                    JOIN history_comments hcr
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
    insertDiscoveryInfo :: [HistoryCommentRevision] -> PG.Pipeline e ()
    insertDiscoveryInfo revs = do
      PG.execute_
        [PG.sql|
          WITH new_discoveries(project_id, history_comment_hash, discovered_at) AS (
            VALUES ^{PG.toTable discoveryTable}
          )
          INSERT INTO history_comment_revisions_project_discovery(project_id, history_comment_revision_id, discovered_at)
            SELECT #{projectId}, hcr.id, nd.discovered_at
            FROM new_discoveries nd
            JOIN history_comments hc
              ON hc.comment_hash = nd.history_comment_hash
            JOIN history_comment_revisions hcr
              ON hcr.history_comment_id = hc.id
          ON CONFLICT DO NOTHING
        |]
      where
        discoveryTable =
          revs <&> \HistoryCommentRevision {..} ->
            ( projectId,
              commentHash,
              utcTimeToMillis createdAt
            )
