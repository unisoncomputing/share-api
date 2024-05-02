{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Enlil.Web.Share.Tickets.Impl
  ( ticketsByProjectServer,
    ticketsByUserServer,
  )
where

import Enlil.IDs (ProjectSlug (..), UserHandle)
import Enlil.IDs qualified as IDs
import Enlil.OAuth.Session
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Queries qualified as Q
import Enlil.Postgres.Tickets.Queries qualified as TicketsQ
import Enlil.Postgres.Users.Queries (userDisplayInfoOf)
import Enlil.Prelude
import Enlil.Project
import Enlil.Ticket
import Enlil.User qualified as User
import Enlil.Utils.API
import Enlil.Web.App
import Enlil.Web.Authentication qualified as AuthN
import Enlil.Web.Authorization qualified as AuthZ
import Enlil.Web.Errors
import Enlil.Web.Share.Comments
import Enlil.Web.Share.Comments.Impl qualified as Comments
import Enlil.Web.Share.Comments.Types
import Enlil.Web.Share.Tickets.API
import Enlil.Web.Share.Tickets.API qualified as API
import Enlil.Web.Share.Tickets.Types
import Enlil.Web.Share.Types (UserDisplayInfo)
import Servant

ticketsByProjectServer :: Maybe Session -> UserHandle -> ProjectSlug -> ServerT API.TicketsByProjectAPI WebApp
ticketsByProjectServer session handle projectSlug =
  let commentResourceServer session commentId =
        Comments.updateCommentEndpoint session handle projectSlug commentId
          :<|> Comments.deleteCommentEndpoint session handle projectSlug commentId
      commentsServer contributionNumber =
        createCommentOnTicketEndpoint session handle projectSlug contributionNumber
          :<|> commentResourceServer session

      timelineServer contributionNumber =
        ( getTicketTimelineEndpoint session handle projectSlug contributionNumber
            :<|> commentsServer contributionNumber
        )
      ticketResourceServer ticketNumber =
        addServerTag (Proxy @API.TicketResourceServer) "ticket-number" (IDs.toText ticketNumber) $
          getTicketByNumberEndpoint session handle projectSlug ticketNumber
            :<|> updateTicketByNumberEndpoint session handle projectSlug ticketNumber
            :<|> timelineServer ticketNumber
   in listTicketsByProjectEndpoint session handle projectSlug
        :<|> createTicketEndpoint session handle projectSlug
        :<|> ticketResourceServer

ticketsByUserServer :: Maybe Session -> UserHandle -> ServerT API.TicketsByUserAPI WebApp
ticketsByUserServer session handle =
  listTicketsByUserEndpoint session handle

createCommentOnTicketEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.TicketNumber -> CreateCommentRequest -> WebApp (CommentEvent UserDisplayInfo)
createCommentOnTicketEndpoint session handle slug ticketNumber createCommentReq = do
  Ticket {ticketId} <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    TicketsQ.ticketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
  Comments.createCommentEndpoint session handle slug (Right ticketId) createCommentReq
  where
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug = slug}

listTicketsByProjectEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  Maybe (Cursor ListTicketsCursor) ->
  Maybe Limit ->
  Maybe (IDs.PrefixedID "@" UserHandle) ->
  Maybe TicketStatus ->
  WebApp (Paged ListTicketsCursor ShareTicket)
listTicketsByProjectEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) handle projectSlug cursor mayLimit authorFilter statusFilter = do
  (project@Project {projectId}, authorUserId) <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    authorFilterID <- for authorFilter \(IDs.PrefixedID authorHandle) -> do
      User.user_id <$> Q.userByHandle authorHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    pure (project, authorFilterID)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketListByProject mayCallerUserId project
  (nextCursor, tickets) <- PG.runTransaction $ TicketsQ.listTicketsByProjectId projectId limit cursor authorUserId statusFilter
  pure $ Paged {items = tickets, cursor = nextCursor}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

createTicketEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  CreateTicketRequest ->
  WebApp ShareTicket
createTicketEndpoint session userHandle projectSlug (CreateTicketRequest {title, description}) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  project@Project {projectId} <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    pure project
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketCreate callerUserId project
  PG.runTransactionOrRespondError $ do
    (_, ticketNumber) <- TicketsQ.createTicket callerUserId projectId title description Open
    TicketsQ.shareTicketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (InternalServerError "create-ticket-error" internalServerError)
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

getTicketByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.TicketNumber ->
  WebApp ShareTicket
getTicketByNumberEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug ticketNumber = do
  (project, shareTicket) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    shareTicket <- TicketsQ.shareTicketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (EntityMissing (ErrorID "ticket:missing") "Ticket not found")
    pure (project, shareTicket)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketRead mayCallerUserId project
  pure shareTicket
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

updateTicketByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.TicketNumber ->
  UpdateTicketRequest ->
  WebApp ShareTicket
updateTicketByNumberEndpoint session handle projectSlug ticketNumber updateRequest@UpdateTicketRequest {title, description, status} = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  (ticket@Ticket {ticketId, projectId, number = ticketNumber}) <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    ticket <- TicketsQ.ticketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (EntityMissing (ErrorID "ticket:missing") "Ticket not found")
    pure ticket
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketUpdate callerUserId ticket updateRequest
  PG.runTransactionOrRespondError $ do
    _ <- TicketsQ.updateTicket callerUserId ticketId title description status
    TicketsQ.shareTicketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (EntityMissing (ErrorID "ticket:missing") "Ticket not found")
  where
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

getTicketTimelineEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.TicketNumber ->
  Maybe (Cursor TicketTimelineCursor) ->
  Maybe Limit ->
  WebApp (Paged TicketTimelineCursor (TicketTimelineEvent UserDisplayInfo))
getTicketTimelineEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug ticketNumber mayCursor mayLimit = do
  (project, shareTicketTimeline, nextCursor) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "project:missing") "Project not found")
    (nextCursor, shareTicketTimeline) <- TicketsQ.getPagedShareTicketTimelineByProjectIdAndNumber projectId ticketNumber (unCursor <$> mayCursor) limit
    shareTicketsTimelineWithUserInfo <-
      shareTicketTimeline
        & userDisplayInfoOf (traverse . traverse)
    pure (project, shareTicketsTimelineWithUserInfo, nextCursor)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketRead mayCallerUserId project
  pure $ Paged {items = shareTicketTimeline, cursor = Cursor <$> nextCursor}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

listTicketsByUserEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe (Cursor ListTicketsCursor) ->
  Maybe Limit ->
  Maybe TicketStatus ->
  WebApp (Paged ListTicketsCursor ShareTicket)
listTicketsByUserEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle mayCursor mayLimit statusFilter = do
  (tickets, nextCursor) <- PG.runTransactionOrRespondError $ do
    user <- Q.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    (nextCursor, tickets) <- TicketsQ.listTicketsByUserId mayCallerUserId (User.user_id user) limit mayCursor statusFilter
    pure (tickets, nextCursor)
  pure $ Paged {items = tickets, cursor = nextCursor}
  where
    limit = fromMaybe 20 mayLimit
