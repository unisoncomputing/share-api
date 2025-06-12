{-# LANGUAGE RecordWildCards #-}

module Share.Ticket where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData (..))
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

data TicketStatus
  = Open
  | Closed
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON TicketStatus where
  toJSON =
    Aeson.toJSON . \case
      Open -> ("open" :: Text)
      Closed -> ("closed" :: Text)

instance Aeson.FromJSON TicketStatus where
  parseJSON =
    Aeson.withText "TicketStatus" \case
      "open" -> pure Open
      "closed" -> pure Closed
      _ -> fail "Invalid ticket status"

instance Hasql.EncodeValue TicketStatus where
  encodeValue =
    Encoders.enum
      ( \case
          Open -> "open"
          Closed -> "closed"
      )

instance FromHttpApiData TicketStatus where
  parseQueryParam = \case
    "open" -> Right Open
    "closed" -> Right Closed
    _ -> Left "Invalid ticket status"

instance Hasql.DecodeValue TicketStatus where
  decodeValue = do
    Decoders.enum
      ( \case
          "open" -> Just Open
          "closed" -> Just Closed
          _ -> Nothing
      )

displayTicketStatus :: TicketStatus -> Text
displayTicketStatus = \case
  Open -> "Open"
  Closed -> "Closed"

data Ticket = Ticket
  { ticketId :: TicketId,
    projectId :: ProjectId,
    number :: TicketNumber,
    title :: Text,
    description :: Maybe Text,
    status :: TicketStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    author :: UserId
  }
  deriving (Show, Eq, Ord)

instance Hasql.DecodeRow Ticket where
  decodeRow = do
    ticketId <- PG.decodeField
    projectId <- PG.decodeField
    number <- PG.decodeField
    title <- PG.decodeField
    description <- PG.decodeField
    status <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    author <- PG.decodeField
    pure Ticket {..}
