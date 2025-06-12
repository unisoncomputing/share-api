{-# LANGUAGE RecordWildCards #-}

module Share.Postgres.Tickets.Ops
  ( createTicket,
    updateTicket,
  )
where

import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types (NotificationEvent (..), NotificationEventData (..), TicketData (..))
import Share.Postgres qualified as PG
import Share.Postgres.Projects.Queries qualified as ProjectsQ
import Share.Postgres.Tickets.Queries qualified as TicketQ
import Share.Prelude
import Share.Ticket (Ticket (..), TicketStatus)
import Share.Utils.API (NullableUpdate, fromNullableUpdate)

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
  insertTicketStatusChangeEvent projectId ticketId authorId Nothing status

  (projectData, projectResourceId, projectOwnerUserId) <- ProjectsQ.projectNotificationData projectId
  let ticketData =
        TicketData
          { ticketId,
            ticketAuthorUserId = authorId
          }
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectTicketCreatedData projectData ticketData,
            eventScope = projectOwnerUserId,
            eventActor = authorId
          }
  NotifQ.recordEvent notifEvent
  pure (ticketId, number)

updateTicket :: UserId -> TicketId -> Maybe Text -> NullableUpdate Text -> Maybe TicketStatus -> PG.Transaction e ()
updateTicket callerUserId ticketId newTitle newDescription newStatus = do
  Ticket {..} <- TicketQ.ticketById ticketId
  let updatedTitle = fromMaybe title newTitle
  let updatedDescription = fromNullableUpdate description newDescription
  let updatedStatus = fromMaybe status newStatus
  -- Add a status change event
  when (isJust newStatus && newStatus /= Just status) do
    insertTicketStatusChangeEvent projectId ticketId callerUserId (Just status) updatedStatus
  PG.execute_
    [PG.sql|
        UPDATE tickets
        SET
          title = #{updatedTitle},
          description = #{updatedDescription},
          status = #{updatedStatus}::ticket_status
        WHERE id = #{ticketId}
        |]

insertTicketStatusChangeEvent :: ProjectId -> TicketId -> UserId -> Maybe TicketStatus -> TicketStatus -> PG.Transaction e ()
insertTicketStatusChangeEvent projectId ticketId actorUserId oldStatus newStatus = do
  PG.execute_
    [PG.sql|
        INSERT INTO ticket_status_events
          (ticket_id, actor, old_status, new_status)
          VALUES (#{ticketId}, #{actorUserId}, #{oldStatus}::ticket_status, #{newStatus}::ticket_status)
      |]

  -- Only record a notification event if it's a status change, not a creation
  case oldStatus of
    Nothing -> pure ()
    Just _ -> do
      (projectData, projectResourceId, projectOwnerUserId) <- ProjectsQ.projectNotificationData projectId
      -- Record the status update notification event
      let ticketData =
            TicketData
              { ticketId,
                ticketAuthorUserId = actorUserId
              }
      let notifEvent =
            NotificationEvent
              { eventId = (),
                eventOccurredAt = (),
                eventResourceId = projectResourceId,
                eventData = ProjectTicketUpdatedData projectData ticketData,
                eventScope = projectOwnerUserId,
                eventActor = actorUserId
              }
      NotifQ.recordEvent notifEvent
