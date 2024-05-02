{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.Tickets.API where

import Data.Time (UTCTime)
import Enlil.IDs
import Enlil.Ticket
import Enlil.Utils.API
import Enlil.Web.Share.Comments.API qualified as Comments
import Enlil.Web.Share.Tickets.Types
import Enlil.Web.Share.Types (UserDisplayInfo)
import Servant

type TicketsByUserAPI = ListTicketsByUserEndpoint

type TicketsByProjectAPI =
  ListTicketsByProjectEndpoint
    :<|> CreateTicketEndpoint
    :<|> (Capture "ticket_number" TicketNumber :> TicketResourceServer)

type TicketResourceServer =
  ( GetTicketByNumber
      :<|> UpdateTicketByNumber
      :<|> ( "timeline"
               :> ( GetTicketTimeline
                      :<|> ("comments" :> Comments.CommentsServer)
                  )
           )
  )

type ListTicketsCursor = (UTCTime, TicketId)

type ListTicketsByProjectEndpoint =
  QueryParam "cursor" (Cursor ListTicketsCursor)
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    -- Only return contributions by this author
    :> QueryParam "author" (PrefixedID "@" UserHandle)
    :> QueryParam "status" TicketStatus
    :> Get '[JSON] (Paged ListTicketsCursor ShareTicket)

type ListTicketsByUserEndpoint =
  QueryParam "cursor" (Cursor ListTicketsCursor)
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    :> QueryParam "status" TicketStatus
    :> Get '[JSON] (Paged ListTicketsCursor ShareTicket)

type CreateTicketEndpoint =
  ReqBody '[JSON] CreateTicketRequest
    :> Post '[JSON] ShareTicket

type GetTicketByNumber = Get '[JSON] ShareTicket

type UpdateTicketByNumber =
  ReqBody '[JSON] UpdateTicketRequest
    :> Patch '[JSON] ShareTicket

type TicketTimelineCursor = UTCTime

type GetTicketTimeline =
  QueryParam "cursor" (Cursor TicketTimelineCursor)
    :> QueryParam "limit" Limit
    :> Get '[JSON] (Paged TicketTimelineCursor (TicketTimelineEvent UserDisplayInfo))
