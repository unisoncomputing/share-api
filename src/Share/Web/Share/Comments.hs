{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Comments
  ( Comment (..),
    RevisionNumber (..),
    CommentEvent (..),
    DeletedComment (..),
    commentEventTimestamp,
  )
where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

newtype RevisionNumber = RevisionNumber Int64
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral, PG.DecodeValue, PG.EncodeValue, Aeson.ToJSON, Aeson.FromJSON)

data Comment user = Comment
  { commentId :: CommentId,
    actor :: user,
    timestamp :: UTCTime,
    editedAt :: Maybe UTCTime,
    content :: Text,
    revision :: RevisionNumber
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (PG.DecodeField user) => PG.DecodeRow (Comment user) where
  decodeRow = do
    commentId <- PG.decodeField
    actor <- PG.decodeField
    timestamp <- PG.decodeField
    editedAt <- PG.decodeField
    content <- PG.decodeField
    revision <- PG.decodeField
    pure Comment {..}

data DeletedComment = DeletedComment
  { commentId :: CommentId,
    -- Timestamp of the original comment
    timestamp :: UTCTime,
    deletedAt :: UTCTime
  }
  deriving stock (Show, Eq, Ord)

instance PG.DecodeRow DeletedComment where
  decodeRow = do
    commentId <- PG.decodeField
    timestamp <- PG.decodeField
    deletedAt <- PG.decodeField
    pure DeletedComment {..}

data CommentEvent user
  = CommentEvent (Comment user)
  | DeletedCommentEvent (DeletedComment)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

commentEventTimestamp :: CommentEvent user -> UTCTime
commentEventTimestamp = \case
  CommentEvent Comment {timestamp} -> timestamp
  DeletedCommentEvent DeletedComment {timestamp} -> timestamp

instance (Aeson.ToJSON user) => Aeson.ToJSON (CommentEvent user) where
  toJSON =
    \case
      CommentEvent Comment {..} ->
        Aeson.object
          [ "kind" Aeson..= ("comment" :: Text),
            "id" Aeson..= commentId,
            "actor" Aeson..= actor,
            "timestamp" Aeson..= timestamp,
            "editedAt" Aeson..= editedAt,
            "content" Aeson..= content,
            "revision" Aeson..= revision
          ]
      DeletedCommentEvent DeletedComment {..} ->
        Aeson.object
          [ "kind" Aeson..= ("comment" :: Text),
            "id" Aeson..= commentId,
            "timestamp" Aeson..= timestamp,
            "deletedAt" Aeson..= deletedAt
          ]

instance (Aeson.FromJSON user) => Aeson.FromJSON (CommentEvent user) where
  parseJSON =
    Aeson.withObject "CommentEvent" $ \o -> do
      mayDeleted <- o Aeson..: "deletedAt"
      case mayDeleted of
        Just (deletedAt :: UTCTime) -> do
          commentId <- o Aeson..: "id"
          timestamp <- o Aeson..: "timestamp"
          pure $ DeletedCommentEvent DeletedComment {..}
        Nothing -> do
          commentId <- o Aeson..: "id"
          actor <- o Aeson..: "actor"
          timestamp <- o Aeson..: "timestamp"
          editedAt <- o Aeson..:? "editedAt"
          content <- o Aeson..: "content"
          revision <- o Aeson..: "revision"
          pure $ CommentEvent Comment {..}
