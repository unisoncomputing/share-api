module Share.Postgres.Comments.Ops (createComment) where

import Data.List qualified as List
import Data.Time (UTCTime)
import Share.IDs
import Share.Notifications.Types (CommentData (..))
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.Share.Comments

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

  (projectResourceId, projectOwnerUserId, projectPrivate) <-
    PG.queryExpect1Row
      [PG.sql|
    SELECT p.resource_id, p.owner_user_id, p.private
    FROM projects p
    WHERE p.id = #{projectId}
    |]
  let commentEventData =
        CommentData
          { commentId,
            commentAuthorUserId = authorId
          }
  let projectData = _
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectTicketCreatedData ticketEventData,
            eventScope = projectOwnerUserId,
            eventActor = authorId
          }
  NotifsQ.recordEvent notifEvent
  pure $
    Comment
      { commentId,
        actor = authorId,
        timestamp,
        editedAt = Nothing,
        content,
        revision = 0
      }
