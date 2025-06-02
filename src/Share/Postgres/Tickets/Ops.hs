module Share.Postgres.Tickets.Ops (createTicket) where

import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types (NotificationEvent (..), NotificationEventData (..), ProjectTicketData (..))
import Share.Postgres qualified as PG
import Share.Postgres.Tickets.Queries qualified as TicketQ
import Share.Prelude
import Share.Ticket (TicketStatus)

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
  TicketQ.insertTicketStatusChangeEvent ticketId authorId Nothing status

  (projectResourceId, projectOwnerUserId, projectPrivate) <-
    PG.queryExpect1Row
      [PG.sql|
    SELECT p.resource_id, p.owner_user_id, p.private
    FROM projects p
    WHERE p.id = #{projectId}
    |]

  let ticketEventData =
        ProjectTicketData
          { projectId,
            ticketId,
            ticketAuthorUserId = authorId,
            public = not projectPrivate
          }
  let notifEvent =
        NotificationEvent
          { eventId = (),
            eventOccurredAt = (),
            eventResourceId = projectResourceId,
            eventData = ProjectTicketCreatedData ticketEventData,
            eventScope = projectOwnerUserId,
            eventActor = authorId
          }
  NotifQ.recordEvent notifEvent
  pure (ticketId, number)
