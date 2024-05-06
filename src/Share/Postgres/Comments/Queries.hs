module Share.Postgres.Comments.Queries
  ( getComment,
    createComment,
    updateComment,
    deleteComment,
    UpdateCommentResult (..),
    commentsByTicketOrContribution,
  )
where

import Data.List qualified as List
import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.Share.Comments

getComment ::
  CommentId ->
  PG.Transaction e (Maybe (Comment UserId))
getComment commentId = do
  PG.query1Row
    [PG.sql|
      SELECT
        comment.id,
        comment.author_id,
        comment.created_at,
        CASE WHEN revision.revision_number = 0 THEN NULL ELSE revision.created_at END AS edited_at,
        revision.content,
        revision.revision_number
      FROM comments AS comment
        JOIN comment_revisions AS revision ON revision.comment_id = comment.id
      WHERE comment.id = #{commentId}
        AND comment.deleted_at IS NULL
      ORDER BY revision.revision_number DESC
      LIMIT 1
    |]

createComment ::
  UserId ->
  Either ContributionId TicketId ->
  Text ->
  PG.Transaction e (Comment UserId)
createComment authorId thingId content = do
  let (contributionId, ticketId) = case thingId of
        Left contributionId -> (Just contributionId, Nothing)
        Right ticketId -> (Nothing, Just ticketId)
  (commentId, timestamp) <-
    PG.queryExpect1Row @(CommentId, UTCTime)
      [PG.sql|
        INSERT INTO comments(contribution_id, ticket_id, author_id)
          VALUES (#{contributionId}, #{ticketId}, #{authorId})
          RETURNING id, created_at
      |]
  PG.execute_
    [PG.sql|
      INSERT INTO comment_revisions(comment_id, revision_number, author_id, content, created_at)
        VALUES (#{commentId}, 0, #{authorId}, #{content}, #{timestamp})
  |]
  pure $
    Comment
      { commentId,
        actor = authorId,
        timestamp,
        editedAt = Nothing,
        content,
        revision = 0
      }

data UpdateCommentResult user
  = UpdateCommentNotFound
  | UpdateCommentSuccess (Comment user)
  | UpdateCommentConflict (Comment user)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

updateComment ::
  UserId ->
  CommentId ->
  RevisionNumber ->
  Text ->
  PG.Transaction e (UpdateCommentResult UserId)
updateComment authorId commentId expectedRevision content = do
  mayLatestRevision <- getComment commentId
  case mayLatestRevision of
    Nothing -> pure UpdateCommentNotFound
    Just currentComment@Comment {revision = latestRevisionNumber}
      | latestRevisionNumber /= expectedRevision -> pure (UpdateCommentConflict currentComment)
      | otherwise ->
          do
            PG.execute_
              [PG.sql|
                INSERT INTO comment_revisions(comment_id, revision_number, author_id, content)
                  VALUES (#{commentId}, #{latestRevisionNumber + 1}, #{authorId}, #{content})
            |]
            getComment commentId >>= \case
              Nothing -> error "updateComment: Failed to fetch comment we just inserted"
              Just comment -> pure (UpdateCommentSuccess comment)

-- | Deletes a comment, returning True if the comment was deleted, False if the comment was
-- not found.
deleteComment :: CommentId -> PG.Transaction e Bool
deleteComment commentId = do
  isJust
    <$> PG.query1Col @UTCTime
      [PG.sql|
            UPDATE comments
              -- Soft delete, keeping old timestamp if it was already deleted.
              SET deleted_at = COALESCE(deleted_at, NOW())
              WHERE id = #{commentId}
              RETURNING deleted_at
    |]

-- | Fetch comments within the given range, returning the latest revision of each comment, and
-- any comments in range that were deleted (without leaking information)
commentsByTicketOrContribution :: Either ContributionId TicketId -> Maybe UTCTime -> Maybe UTCTime -> PG.Transaction e [CommentEvent UserId]
commentsByTicketOrContribution thingId mayFromExclusive untilInclusive = do
  commentEvents <- fmap CommentEvent <$> getComments
  deletedCommentEvents <- fmap DeletedCommentEvent <$> getDeletedComments
  pure . List.sortOn commentEventTimestamp $ commentEvents <> deletedCommentEvents
  where
    commentFilter = case thingId of
      Left contributionId -> [PG.sql| comment.contribution_id = #{contributionId} |]
      Right ticketId -> [PG.sql| comment.ticket_id = #{ticketId} |]
    getComments :: PG.Transaction e [Comment UserId]
    getComments =
      PG.queryListRows @(Comment UserId)
        [PG.sql|
            WITH latest_revisions(comment_id, revision_number, content, created_at) AS (
              SELECT DISTINCT ON (revision.comment_id)
                revision.comment_id,
                revision.revision_number,
                revision.content,
                revision.created_at
              FROM comment_revisions AS revision
              ORDER BY revision.comment_id, revision.revision_number DESC
            )
            SELECT
              comment.id,
              comment.author_id,
              comment.created_at,
              -- NULL out the edited_at field if this is the first revision.
              CASE WHEN revision.revision_number = 0 THEN NULL ELSE revision.created_at END AS edited_at,
              revision.content,
              revision.revision_number
            FROM comments AS comment
            JOIN latest_revisions revision ON revision.comment_id = comment.id
            WHERE ^{commentFilter}
                  AND comment.deleted_at IS NULL
                  AND (#{mayFromExclusive} IS NULL OR comment.created_at > #{mayFromExclusive})
                  AND (#{untilInclusive} IS NULL OR comment.created_at <= #{untilInclusive})
            ORDER BY comment.created_at ASC
          |]
    getDeletedComments :: PG.Transaction e [DeletedComment]
    getDeletedComments =
      PG.queryListRows @DeletedComment
        [PG.sql|
            SELECT
              comment.id,
              comment.created_at,
              comment.deleted_at
            FROM comments AS comment
            WHERE ^{commentFilter}
                  AND comment.deleted_at IS NOT NULL
                  AND (#{mayFromExclusive} IS NULL OR comment.created_at > #{mayFromExclusive})
                  AND (#{untilInclusive} IS NULL OR comment.created_at <= #{untilInclusive})
            ORDER BY comment.created_at ASC
          |]
