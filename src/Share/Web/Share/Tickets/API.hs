{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Tickets.API where

import Data.Time (UTCTime)
import Servant
import Share.IDs
import Share.Ticket
import Share.Utils.API
import Share.Web.Share.Comments.API qualified as Comments
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo (..))
import Share.Web.Share.Tickets.Types

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
    :> Get '[JSON] (Paged ListTicketsCursor (ShareTicket UserDisplayInfo))

type ListTicketsByUserEndpoint =
  QueryParam "cursor" (Cursor ListTicketsCursor)
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    :> QueryParam "status" TicketStatus
    :> Get '[JSON] (Paged ListTicketsCursor (ShareTicket UserDisplayInfo))

type CreateTicketEndpoint =
  ReqBody '[JSON] CreateTicketRequest
    :> Post '[JSON] (ShareTicket UserDisplayInfo)

type GetTicketByNumber = Get '[JSON] (ShareTicket UserDisplayInfo)

type UpdateTicketByNumber =
  ReqBody '[JSON] UpdateTicketRequest
    :> Patch '[JSON] (ShareTicket UserDisplayInfo)

type TicketTimelineCursor = UTCTime

type GetTicketTimeline =
  QueryParam "cursor" (Cursor TicketTimelineCursor)
    :> QueryParam "limit" Limit
    :> Get '[JSON] (Paged TicketTimelineCursor (TicketTimelineEvent UserDisplayInfo))
