{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.Comments.API (CommentsServer) where

import Enlil.IDs
import Enlil.Web.Share.Comments
import Enlil.Web.Share.Comments.Types
import Enlil.Web.Share.Types (UserDisplayInfo)
import Servant

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
