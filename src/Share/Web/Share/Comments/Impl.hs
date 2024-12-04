module Share.Web.Share.Comments.Impl
  ( createCommentEndpoint,
    updateCommentEndpoint,
    deleteCommentEndpoint,
  )
where

import Control.Lens
import Share.IDs
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Comments.Queries qualified as CommentsQ
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.Comments
import Share.Web.Share.Comments.Types
import Share.Web.Share.Types

createCommentEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  Either IDs.ContributionId IDs.TicketId ->
  CreateCommentRequest ->
  WebApp (CommentEvent UserDisplayInfo)
createCommentEndpoint session userHandle projectSlug contributionOrTicketId (CreateCommentRequest {content}) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  project <- PG.runTransactionOrRespondError $ do
    Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkCommentCreate callerUserId project

  PG.runTransaction $ do
    commentEvent <- CommentsQ.createComment callerUserId contributionOrTicketId content
    UserQ.userDisplayInfoOf traversed (CommentEvent commentEvent)
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

updateCommentEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.CommentId -> UpdateCommentRequest -> WebApp UpdateCommentResponse
updateCommentEndpoint session _userHandle _projectSlug commentId (UpdateCommentRequest {content, expectedRevision}) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  existingComment <- PG.runTransactionOrRespondError $ do
    CommentsQ.getComment commentId `whenNothingM` throwError (EntityMissing (ErrorID "comment:missing") "Comment not found")

  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkCommentUpdate callerUserId existingComment

  PG.runTransactionOrRespondError $ do
    updateResult <- CommentsQ.updateComment callerUserId commentId expectedRevision content
    UserQ.userDisplayInfoOf traversed updateResult >>= \case
      CommentsQ.UpdateCommentNotFound -> throwError (EntityMissing (ErrorID "comment:missing") "Comment not found")
      (CommentsQ.UpdateCommentConflict newComment) ->
        pure $ UpdateCommentConflict . CommentEvent $ newComment
      (CommentsQ.UpdateCommentSuccess commentEvent) ->
        pure $ UpdateCommentSuccess . CommentEvent $ commentEvent

deleteCommentEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.CommentId -> WebApp ()
deleteCommentEndpoint session _userHandle _projectSlug commentId = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  existingComment <- PG.runTransactionOrRespondError $ do
    CommentsQ.getComment commentId `whenNothingM` throwError (EntityMissing (ErrorID "comment:missing") "Comment not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkCommentDelete callerUserId existingComment
  PG.runTransactionOrRespondError $ do
    CommentsQ.deleteComment commentId >>= \case
      True -> pure ()
      False -> throwError (EntityMissing (ErrorID "comment:missing") "Comment not found")
