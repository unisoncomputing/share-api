{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Support.Types where

import Data.Aeson
import Data.Text qualified as Text
import Share.Prelude
import Servant

data Priority
  = Low
  | Normal
  | High
  | Urgent
  deriving stock (Show, Eq, Ord)

instance ToJSON Priority where
  toJSON = \case
    Low -> "low"
    Normal -> "normal"
    High -> "high"
    Urgent -> "urgent"

instance FromJSON Priority where
  parseJSON = withText "Priority" \case
    "low" -> pure Low
    "normal" -> pure Normal
    "high" -> pure High
    "urgent" -> pure Urgent
    txt -> fail . Text.unpack $ "Unknown ticket priority: " <> txt

instance FromHttpApiData Priority where
  parseQueryParam = \case
    "low" -> pure Low
    "normal" -> pure Normal
    "high" -> pure High
    "urgent" -> pure Urgent
    txt -> Left $ "Unknown ticket priority: " <> txt

data SupportTicketRequest = SupportTicketRequest
  { subject :: Text,
    body :: Text,
    priority :: Priority
  }
  deriving stock (Show)

instance FromJSON SupportTicketRequest where
  parseJSON = withObject "SupportTicketRequest" \v -> do
    subject <- v .: "subject"
    body <- (fromMaybe "" <$> v .:? "body")
    priority <- (fromMaybe Normal <$> v .:? "priority")
    pure $ SupportTicketRequest {..}
