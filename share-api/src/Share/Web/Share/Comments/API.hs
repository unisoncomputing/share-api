{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Comments.API (CommentsServer) where

import Servant
import Share.IDs
import Share.Web.Share.Comments
import Share.Web.Share.Comments.Types
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)

type CommentResourceServer = UpdateComment :<|> DeleteComment

type CommentsServer =
  CreateComment
    :<|> (Capture "comment_id" CommentId :> CommentResourceServer)

type CreateComment =
  ReqBody '[JSON] CreateCommentRequest
    :> Post '[JSON] (CommentEvent UserDisplayInfo)

type UpdateComment =
  ReqBody '[JSON] UpdateCommentRequest
    :> Patch '[JSON] UpdateCommentResponse

type DeleteComment =
  Delete '[JSON] ()
