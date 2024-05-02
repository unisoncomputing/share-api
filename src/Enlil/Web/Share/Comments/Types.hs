{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Enlil.Web.Share.Comments.Types where

import Data.Aeson
import Enlil.Prelude
import Enlil.Web.Share.Comments (CommentEvent (..), RevisionNumber)
import Enlil.Web.Share.Types (UserDisplayInfo)

data CreateCommentRequest = CreateCommentRequest
  { content :: Text
  }
  deriving (Show)

instance FromJSON CreateCommentRequest where
  parseJSON = withObject "CreateCommentRequest" \o -> do
    content <- o .: "content"
    pure CreateCommentRequest {..}

data UpdateCommentRequest = UpdateCommentRequest
  { content :: Text,
    -- The revision number of the comment that the update was based on.
    -- This is used to determine if the comment has been updated since the
    -- client last fetched it.
    expectedRevision :: RevisionNumber
  }
  deriving (Show)

instance FromJSON UpdateCommentRequest where
  parseJSON = withObject "UpdateCommentRequest" \o -> do
    content <- o .: "content"
    expectedRevision <- o .: "expectedRevision"
    pure UpdateCommentRequest {..}

data UpdateCommentResponse
  = UpdateCommentSuccess (CommentEvent UserDisplayInfo)
  | UpdateCommentConflict (CommentEvent UserDisplayInfo)
  deriving (Show)

instance ToJSON UpdateCommentResponse where
  toJSON = \case
    UpdateCommentSuccess event -> object ["kind" .= ("success" :: Text), "comment" .= event]
    UpdateCommentConflict event -> object ["kind" .= ("conflict" :: Text), "comment" .= event]
