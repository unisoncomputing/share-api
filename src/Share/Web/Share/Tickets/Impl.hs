{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Share.Web.Share.Tickets.Impl
  ( ticketsByProjectServer,
    ticketsByUserServer,
  )
where

import Control.Lens
import Servant
import Share.IDs (ProjectSlug (..), UserHandle)
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Tickets.Queries qualified as TicketsQ
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project
import Share.Ticket
import Share.User qualified as User
import Share.Utils.API
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.Comments
import Share.Web.Share.Comments.Impl qualified as Comments
import Share.Web.Share.Comments.Types
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo (..))
import Share.Web.Share.Tickets.API
import Share.Web.Share.Tickets.API qualified as API
import Share.Web.Share.Tickets.Types

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
  WebApp (Paged ListTicketsCursor (ShareTicket UserDisplayInfo))
listTicketsByProjectEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) handle projectSlug cursor mayLimit authorFilter statusFilter = do
  (Project {projectId}, authorUserId) <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    authorFilterID <- for authorFilter \(IDs.PrefixedID authorHandle) -> do
      User.user_id <$> UserQ.userByHandle authorHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    pure (project, authorFilterID)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketListByProject mayCallerUserId projectId
  tickets <- PG.runTransaction do
    TicketsQ.listTicketsByProjectId projectId limit cursor authorUserId statusFilter
      >>= UserQ.userDisplayInfoOf (traversed . traversed)
  pure tickets
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

createTicketEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  CreateTicketRequest ->
  WebApp (ShareTicket UserDisplayInfo)
createTicketEndpoint session userHandle projectSlug (CreateTicketRequest {title, description}) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  Project {projectId} <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    pure project
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketCreate callerUserId projectId
  PG.runTransactionOrRespondError $ do
    (_, ticketNumber) <- TicketsQ.createTicket callerUserId projectId title description Open
    TicketsQ.shareTicketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (InternalServerError "create-ticket-error" internalServerError)
      >>= UserQ.userDisplayInfoOf traverse
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

getTicketByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.TicketNumber ->
  WebApp (ShareTicket UserDisplayInfo)
getTicketByNumberEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug ticketNumber = do
  (projectId, shareTicket) <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    shareTicket <-
      TicketsQ.shareTicketByProjectIdAndNumber projectId ticketNumber `whenNothingM` throwError (EntityMissing (ErrorID "ticket:missing") "Ticket not found")
        >>= UserQ.userDisplayInfoOf traversed
    pure (projectId, shareTicket)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketRead mayCallerUserId projectId
  pure shareTicket
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

updateTicketByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.TicketNumber ->
  UpdateTicketRequest ->
  WebApp (ShareTicket UserDisplayInfo)
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
      >>= UserQ.userDisplayInfoOf traversed
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
  (projectId, shareTicketTimeline, nextCursor) <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "project:missing") "Project not found")
    (nextCursor, shareTicketTimeline) <- TicketsQ.getPagedShareTicketTimelineByProjectIdAndNumber projectId ticketNumber (location <$> mayCursor) limit
    shareTicketsTimelineWithUserInfo <-
      shareTicketTimeline
        & UserQ.userDisplayInfoOf (traverse . traverse)
    pure (projectId, shareTicketsTimelineWithUserInfo, nextCursor)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkTicketRead mayCallerUserId projectId
  -- We don't currently support backwards pagination on timelines.
  pure $ Paged {items = shareTicketTimeline, nextCursor = Cursor <$> nextCursor <*> pure Next, prevCursor = Nothing}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

listTicketsByUserEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe (Cursor ListTicketsCursor) ->
  Maybe Limit ->
  Maybe TicketStatus ->
  WebApp (Paged ListTicketsCursor (ShareTicket UserDisplayInfo))
listTicketsByUserEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle mayCursor mayLimit statusFilter = do
  tickets <- PG.runTransactionOrRespondError $ do
    user <- UserQ.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    tickets <-
      TicketsQ.listTicketsByUserId mayCallerUserId (User.user_id user) limit mayCursor statusFilter
        >>= UserQ.userDisplayInfoOf (traversed . traversed)
    pure tickets
  pure tickets
  where
    limit = fromMaybe 20 mayLimit
