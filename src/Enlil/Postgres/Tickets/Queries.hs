{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Postgres.Tickets.Queries
  ( createTicket,
    ticketByProjectIdAndNumber,
    shareTicketByProjectIdAndNumber,
    listTicketsByProjectId,
    ticketById,
    updateTicket,
    insertTicketStatusChangeEvent,
    ticketStatusChangeEventsByTicketId,
    ticketCommentsByTicketId,
    listTicketsByUserId,
    getPagedShareTicketTimelineByProjectIdAndNumber,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.List qualified as List
import Data.Time (UTCTime)
import Enlil.IDs
import Enlil.Postgres qualified as PG
import Enlil.Prelude
import Enlil.Ticket (Ticket (..), TicketStatus)
import Enlil.Utils.API
import Enlil.Web.Errors
import Enlil.Web.Share.Comments
import Enlil.Web.Share.Tickets.API
import Enlil.Web.Share.Tickets.Types
import Safe (lastMay)

createTicket ::
  -- | Author
  UserId ->
  ProjectId ->
  -- | Title
  Text ->
  -- | Description
  Maybe Text ->
  TicketStatus ->
  PG.Transaction e (TicketId, TicketNumber)
createTicket authorId projectId title description status = do
  (ticketId, number) <-
    PG.queryExpect1Row
      [PG.sql|
        WITH new_ticket_number AS (
            SELECT (COALESCE(MAX(ticket_number), 0) + 1) AS new
            FROM tickets ticket
            WHERE ticket.project_id = #{projectId}
        )
        INSERT INTO tickets(
          author_id,
          project_id,
          title,
          description,
          status,
          ticket_number
        )
        SELECT #{authorId}, #{projectId}, #{title}, #{description}, #{status}::ticket_status, new_ticket_number.new
          FROM new_ticket_number
        RETURNING tickets.id, tickets.ticket_number
      |]
  insertTicketStatusChangeEvent ticketId authorId Nothing status
  pure (ticketId, number)

ticketByProjectIdAndNumber ::
  ProjectId ->
  TicketNumber ->
  PG.Transaction e (Maybe Ticket)
ticketByProjectIdAndNumber projectId ticketNumber = do
  PG.query1Row
    [PG.sql|
        SELECT
          ticket.id,
          ticket.project_id,
          ticket.ticket_number,
          ticket.title,
          ticket.description,
          ticket.status,
          ticket.created_at,
          ticket.updated_at,
          ticket.author_id
        FROM tickets ticket
        WHERE ticket.project_id = #{projectId}
              AND ticket.ticket_number = #{ticketNumber}
      |]

shareTicketByProjectIdAndNumber ::
  ProjectId ->
  TicketNumber ->
  PG.Transaction e (Maybe ShareTicket)
shareTicketByProjectIdAndNumber projectId ticketNumber = do
  PG.query1Row @ShareTicket
    [PG.sql|
        SELECT
          ticket.id,
          ticket.ticket_number,
          project_owner.handle,
          project.slug,
          ticket.title,
          ticket.description,
          ticket.status,
          ticket.created_at,
          ticket.updated_at,
          author.handle,
          (SELECT COUNT(*) FROM comments comment WHERE comment.ticket_id = ticket.id AND comment.deleted_at IS NULL) as num_comments
        FROM tickets AS ticket
             JOIN projects AS project ON project.id = ticket.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
             JOIN users AS author ON author.id = ticket.author_id
        WHERE ticket.project_id = #{projectId}
              AND ticket_number = #{ticketNumber}
      |]

-- | Lists all tickets for a project which match the provided filters.
--   Most recently updated first.
listTicketsByProjectId ::
  ProjectId ->
  Limit ->
  Maybe (Cursor ListTicketsCursor) ->
  Maybe UserId ->
  Maybe TicketStatus ->
  PG.Transaction e (Maybe (Cursor ListTicketsCursor), [ShareTicket])
listTicketsByProjectId projectId limit mayCursor mayUserFilter mayStatusFilter = do
  let cursorFilter = case mayCursor of
        Nothing -> "true"
        Just (Cursor (beforeTime, ticketId)) ->
          [PG.sql|
          (ticket.updated_at, ticket.id) < (#{beforeTime}, #{ticketId})
          |]
  addCursor
    <$> PG.queryListRows @ShareTicket
      [PG.sql|
        SELECT
          ticket.id,
          ticket.ticket_number,
          project_owner.handle,
          project.slug,
          ticket.title,
          ticket.description,
          ticket.status,
          ticket.created_at,
          ticket.updated_at,
          author.handle,
          (SELECT COUNT(*) FROM comments comment WHERE comment.ticket_id = ticket.id AND comment.deleted_at IS NULL) as num_comments
        FROM tickets AS ticket
             JOIN projects AS project ON project.id = ticket.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
             JOIN users AS author ON author.id = ticket.author_id
        WHERE
          ^{cursorFilter}
          AND project.id = #{projectId}
          AND (#{mayUserFilter} IS NULL OR ticket.author_id = #{mayUserFilter})
          AND (#{mayStatusFilter} IS NULL OR ticket.status = #{mayStatusFilter}::ticket_status)
        ORDER BY ticket.updated_at DESC, ticket.id DESC
        LIMIT #{limit}
      |]
  where
    addCursor :: [ShareTicket] -> (Maybe (Cursor ListTicketsCursor), [ShareTicket])
    addCursor xs =
      ( lastMay xs <&> \(ShareTicket {updatedAt, ticketId}) ->
          Cursor (updatedAt, ticketId),
        xs
      )

ticketById :: TicketId -> PG.Transaction e (Maybe Ticket)
ticketById ticketId = do
  PG.query1Row
    [PG.sql|
        SELECT
          ticket.id,
          ticket.project_id,
          ticket.ticket_number,
          ticket.title,
          ticket.description,
          ticket.status,
          ticket.created_at,
          ticket.updated_at,
          ticket.author_id
        FROM tickets AS ticket
        WHERE ticket.id = #{ticketId}
      |]

updateTicket :: UserId -> TicketId -> Maybe Text -> NullableUpdate Text -> Maybe TicketStatus -> PG.Transaction e Bool
updateTicket callerUserId ticketId newTitle newDescription newStatus = do
  isJust <$> runMaybeT do
    Ticket {..} <- MaybeT $ ticketById ticketId
    let updatedTitle = fromMaybe title newTitle
    let updatedDescription = fromNullableUpdate description newDescription
    let updatedStatus = fromMaybe status newStatus
    -- Add a status change event
    when (isJust newStatus && newStatus /= Just status) do
      lift $ insertTicketStatusChangeEvent ticketId callerUserId (Just status) updatedStatus
    lift $
      PG.execute_
        [PG.sql|
        UPDATE tickets
        SET
          title = #{updatedTitle},
          description = #{updatedDescription},
          status = #{updatedStatus}::ticket_status
        WHERE id = #{ticketId}
        |]

insertTicketStatusChangeEvent :: TicketId -> UserId -> Maybe TicketStatus -> TicketStatus -> PG.Transaction e ()
insertTicketStatusChangeEvent ticketId actorUserId oldStatus newStatus = do
  PG.execute_
    [PG.sql|
        INSERT INTO ticket_status_events
          (ticket_id, actor, old_status, new_status)
          VALUES (#{ticketId}, #{actorUserId}, #{oldStatus}::ticket_status, #{newStatus}::ticket_status)
      |]

ticketStatusChangeEventsByTicketId :: TicketId -> Maybe UTCTime -> Maybe UTCTime -> PG.Transaction e [StatusChangeEvent UserId]
ticketStatusChangeEventsByTicketId ticketId mayFromExclusive untilInclusive = do
  PG.queryListRows
    [PG.sql|
        SELECT
          event.old_status,
          event.new_status,
          event.actor,
          event.created_at
        FROM ticket_status_events AS event
        WHERE ticket_id = #{ticketId}
              AND (#{mayFromExclusive} IS NULL OR event.created_at > #{mayFromExclusive})
              AND (#{untilInclusive} IS NULL OR event.created_at <= #{untilInclusive})
        ORDER BY event.created_at ASC
      |]

-- | Fetch comments within the given range, returning the latest revision of each comment, and
-- any comments in range that were deleted (without leaking information)
ticketCommentsByTicketId :: TicketId -> Maybe UTCTime -> Maybe UTCTime -> PG.Transaction e [CommentEvent UserId]
ticketCommentsByTicketId ticketId mayFromExclusive untilInclusive = do
  commentEvents <- fmap CommentEvent <$> getComments
  deletedCommentEvents <- fmap DeletedCommentEvent <$> getDeletedComments
  pure . List.sortOn commentEventTimestamp $ commentEvents <> deletedCommentEvents
  where
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
            WHERE comment.ticket_id = #{ticketId}
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
            WHERE comment.ticket_id = #{ticketId}
                  AND comment.deleted_at IS NOT NULL
                  AND (#{mayFromExclusive} IS NULL OR comment.created_at > #{mayFromExclusive})
                  AND (#{untilInclusive} IS NULL OR comment.created_at <= #{untilInclusive})
            ORDER BY comment.created_at ASC
          |]

listTicketsByUserId ::
  Maybe UserId ->
  UserId ->
  Limit ->
  Maybe (Cursor (UTCTime, TicketId)) ->
  Maybe TicketStatus ->
  PG.Transaction e (Maybe (Cursor (UTCTime, TicketId)), [ShareTicket])
listTicketsByUserId callerUserId userId limit mayCursor mayStatusFilter = do
  let cursorFilter = case mayCursor of
        Nothing -> "true"
        Just (Cursor (beforeTime, ticketId)) ->
          [PG.sql|
          (ticket.updated_at, ticket.id) < (#{beforeTime}, #{ticketId})
          |]
  addCursor
    <$> PG.queryListRows @ShareTicket
      [PG.sql|
      SELECT
        ticket.id,
        ticket.ticket_number,
        project_owner.handle,
        project.slug,
        ticket.title,
        ticket.description,
        ticket.status,
        ticket.created_at,
        ticket.updated_at,
        author.handle,
        (SELECT COUNT(*) FROM comments comment WHERE comment.ticket_id = ticket.id AND comment.deleted_at IS NULL) as num_comments
      FROM tickets AS ticket
        JOIN projects AS project ON project.id = ticket.project_id
      WHERE
        ticket.author_id = #{userId}
        AND NOT project.private
          OR EXISTS (
            SELECT FROM accessible_private_projects ap
            WHERE
              ap.user_id = #{callerUserId}
              AND ap.project_id = project.id
          )
        AND (#{mayStatusFilter} IS NULL OR ticket.status = #{mayStatusFilter}::ticket_status)
        AND ^{cursorFilter}
      ORDER BY ticket.updated_at DESC, ticket.id DESC
      LIMIT #{limit}
      |]
  where
    addCursor :: [ShareTicket] -> (Maybe (Cursor ListTicketsCursor), [ShareTicket])
    addCursor xs =
      ( lastMay xs <&> \(ShareTicket {updatedAt, ticketId}) ->
          Cursor (updatedAt, ticketId),
        xs
      )

getPagedShareTicketTimelineByProjectIdAndNumber ::
  ProjectId ->
  TicketNumber ->
  Maybe TicketTimelineCursor ->
  Limit ->
  PG.Transaction SomeServerError (Maybe TicketTimelineCursor, [TicketTimelineEvent UserId])
getPagedShareTicketTimelineByProjectIdAndNumber projectId ticketNumber mayFromExclusive limit = do
  Ticket {ticketId} <- ticketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "ticket:missing") "Ticket not found")
  mayTillInclusiveTimestamp <- determineUpperDateBound ticketId
  statusChangeEvents <- fmap TicketTimelineStatusChange <$> ticketStatusChangeEventsByTicketId ticketId mayFromExclusive mayTillInclusiveTimestamp
  comments <- fmap TicketTimelineComment <$> ticketCommentsByTicketId ticketId mayFromExclusive mayTillInclusiveTimestamp
  let timeline = List.sortOn eventTimestamp (statusChangeEvents <> comments)
  pure (mayTillInclusiveTimestamp, timeline)
  where
    -- We want to page a set number of events, but are joining across many event sources.
    -- This converts a page size into a timestamp range which is easily applicable across all
    -- event sources.
    --
    -- Effectively we join all the events from all sources starting at the 'from' time, then
    -- take the first <page-size> events from that set, which allows us to determine the
    -- 'till' time to use when ACTUALLY fetching entities from each individual table.
    --
    -- We can't just grab 'at most N' elements from each table because it may result in some
    -- events appearing to be missing from the timeline.
    determineUpperDateBound :: TicketId -> PG.Transaction e (Maybe UTCTime)
    determineUpperDateBound ticketId = do
      PG.query1Col
        [PG.sql|
            WITH events(timestamp, ticket_id) AS (
              SELECT status_event.created_at, status_event.ticket_id FROM ticket_status_events status_event
              UNION ALL
              SELECT comment.created_at, comment.ticket_id FROM comments comment
            ), events_in_window (timestamp, ticket_id) AS (
              SELECT timestamp, ticket_id FROM events
                WHERE events.ticket_id = #{ticketId}
                  AND (#{mayFromExclusive} IS NULL OR timestamp > #{mayFromExclusive})
                ORDER BY timestamp ASC
                LIMIT #{limit}
            )
            SELECT MAX(events_in_window.timestamp) FROM events_in_window
        |]
