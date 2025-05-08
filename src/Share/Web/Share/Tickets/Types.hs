{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Share.Web.Share.Tickets.Types where

import Data.Aeson
import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Ticket (TicketStatus)
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Web.Share.Comments
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo (..))

data ShareTicket user = ShareTicket
  { ticketId :: TicketId,
    projectShortHand :: ProjectShortHand,
    number :: TicketNumber,
    title :: Text,
    description :: Maybe Text,
    status :: TicketStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    -- This is optional so we can delete users without deleting ALL their tickets.
    author :: Maybe user,
    numComments :: Int32
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance PG.DecodeRow (ShareTicket UserId) where
  decodeRow = do
    ticketId <- PG.decodeField
    number <- PG.decodeField
    projectOwnerHandle <- PG.decodeField
    projectSlug <- PG.decodeField
    title <- PG.decodeField
    description <- PG.decodeField
    status <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    author <- PG.decodeField
    let projectShortHand = ProjectShortHand {userHandle = projectOwnerHandle, projectSlug}
    numComments <- PG.decodeField @Int32
    pure ShareTicket {..}

instance ToJSON (ShareTicket UserDisplayInfo) where
  toJSON ShareTicket {..} =
    object
      [ "id" .= ticketId,
        "projectRef" .= projectShortHand,
        "number" .= number,
        "title" .= title,
        "description" .= description,
        "status" .= status,
        "createdAt" .= createdAt,
        "updatedAt" .= updatedAt,
        "author" .= author,
        "numComments" .= numComments
      ]

data StatusChangeEvent user = StatusChangeEvent
  { oldStatus :: Maybe TicketStatus,
    newStatus :: TicketStatus,
    actor :: user,
    timestamp :: UTCTime
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (PG.DecodeField user) => PG.DecodeRow (StatusChangeEvent user) where
  decodeRow = do
    oldStatus <- PG.decodeField
    newStatus <- PG.decodeField
    actor <- PG.decodeField
    timestamp <- PG.decodeField
    pure StatusChangeEvent {..}

data TicketTimelineEvent user
  = TicketTimelineStatusChange (StatusChangeEvent user)
  | TicketTimelineComment (CommentEvent user)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

eventTimestamp :: TicketTimelineEvent user -> UTCTime
eventTimestamp = \case
  TicketTimelineStatusChange StatusChangeEvent {timestamp} -> timestamp
  TicketTimelineComment commentEvent -> commentEventTimestamp commentEvent

instance (ToJSON user) => ToJSON (TicketTimelineEvent user) where
  toJSON = \case
    TicketTimelineStatusChange StatusChangeEvent {..} ->
      object
        [ "kind" .= ("statusChange" :: Text),
          "newStatus" .= newStatus,
          "oldStatus" .= oldStatus,
          "timestamp" .= timestamp,
          "actor" .= actor
        ]
    TicketTimelineComment commentEvent -> toJSON commentEvent

data CreateTicketRequest = CreateTicketRequest
  { title :: Text,
    description :: Maybe Text
  }
  deriving (Show)

instance FromJSON CreateTicketRequest where
  parseJSON = withObject "CreateContributionRequest" \o -> do
    title <- o .: "title"
    description <- o .:? "description"
    pure CreateTicketRequest {..}

data UpdateTicketRequest = UpdateTicketRequest
  { title :: Maybe Text,
    description :: NullableUpdate Text,
    status :: Maybe TicketStatus
  }
  deriving (Show)

instance FromJSON UpdateTicketRequest where
  parseJSON = withObject "UpdateTicketRequest" \o -> do
    title <- o .:? "title"
    description <- parseNullableUpdate o "description"
    status <- o .:? "status"
    pure UpdateTicketRequest {..}
