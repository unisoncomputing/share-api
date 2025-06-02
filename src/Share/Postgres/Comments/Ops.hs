module Share.Postgres.Comments.Ops (createComment) where

import Data.Time (UTCTime)
import Share.Contribution (Contribution (..))
import Share.IDs
import Share.Notifications.Queries qualified as NotifsQ
import Share.Notifications.Types (CommentData (..), ContributionData (..), NotificationEvent (..), NotificationEventData (..), TicketData (..))
import Share.Postgres qualified as PG
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Projects.Queries qualified as ProjectQ
import Share.Postgres.Tickets.Queries qualified as TicketQ
import Share.Prelude
import Share.Ticket (Ticket (..))
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

  let commentData =
        CommentData
          { commentId,
            commentAuthorUserId = authorId
          }
  (event, projectResourceId, projectOwnerUserId) <- case thingId of
    Left contributionId -> do
      Contribution {projectId, sourceBranchId, targetBranchId, author} <- ContributionQ.contributionById contributionId
      (projectData, projectResourceId, projectOwnerUserId) <- ProjectQ.projectNotificationData projectId
      let contributionData =
            ContributionData
              { contributionId,
                fromBranchId = sourceBranchId,
                toBranchId = targetBranchId,
                contributorUserId = author
              }
      pure (ProjectContributionCommentData projectData contributionData commentData, projectResourceId, projectOwnerUserId)
    Right ticketId -> do
      Ticket {projectId, author} <- TicketQ.ticketById ticketId
      (projectData, projectResourceId, projectOwnerUserId) <- ProjectQ.projectNotificationData projectId
      let ticketData =
            TicketData
              { ticketId,
                ticketAuthorUserId = author
              }
      pure (ProjectTicketCommentData projectData ticketData commentData, projectResourceId, projectOwnerUserId)
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = event,
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
