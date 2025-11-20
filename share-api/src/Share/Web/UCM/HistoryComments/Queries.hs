{-# LANGUAGE RecordWildCards #-}

module Share.Web.UCM.HistoryComments.Queries (fetchProjectBranchCommentsSince) where

import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as PG
import Share.Postgres.IDs
import Share.Prelude
import Unison.Hash32 (Hash32)
import Unison.Server.HistoryComments.Types

fetchProjectBranchCommentsSince :: ProjectId -> CausalId -> UTCTime -> PG.Transaction e (PGCursor HistoryCommentChunk)
fetchProjectBranchCommentsSince projectId causalId sinceTime = do
  PG.newRowCursor @(Bool, Maybe Text, Maybe Text, Maybe UTCTime, Maybe Bool, Maybe ByteString, Maybe Hash32, Maybe Text, Maybe UTCTime, Maybe ByteString, Maybe Hash32, Maybe Hash32)
    "fetchProjectBranchCommentsSince"
    [PG.sql|
    WITH revisions(id, comment_id, subject, contents, created_at_ms, hidden, revision_hash, author_signature) AS (
      SELECT hcr.id, hc.id, hcr.subject, hcr.content, hcr.created_at_ms, hcr.is_hidden, hcr.revision_hash, hcr.author_signature
            history_comment_revisions_project_discovery pd
            JOIN history_comment_revisions hcr
              ON pd.history_comment_revision_id = hcr.id
            JOIN history_comments hc
              ON hcr.history_comment_id = hc.id
            WHERE
              pd.project_id = #{projectId}
              AND pd.discovered_at > #{sinceTime}
              AND hc.causal_id IN (SELECT causal_id FROM causal_history(#{causalId}))
    ) (SELECT true, NULL, (hc.author, hc.created_at_ms, key.thumbprint, causal.hash, hc.comment_hash)
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
        false, (rev.subject, rev.content, rev.created_at, rev.is_hidden, rev.author_signature, rev.revision_hash), NULL
      FROM revisions rev
      )
    |]
    <&> fmap \case
      (True, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just author, Just createdAt, Just authorThumbprint, Just causalHash, Just commentHash) ->
        HistoryCommentChunk $ HistoryComment {..}
      (False, Just subject, Just content, Just createdAt, Just isHidden, Just authorSignature, Just revisionHash, Nothing, Nothing, Nothing, Nothing, Nothing) ->
        HistoryCommentRevisionChunk $ HistoryCommentRevision {..}
      row -> error $ "fetchProjectBranchCommentsSince: Unexpected row format: " <> show row
