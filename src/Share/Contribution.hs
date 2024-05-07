{-# LANGUAGE RecordWildCards #-}

module Share.Contribution where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import Hasql.Decoders qualified as Hasql
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData (..))

data ContributionStatus
  = Draft
  | InReview
  | Closed
  | Merged
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON ContributionStatus where
  toJSON =
    Aeson.toJSON . \case
      Draft -> ("draft" :: Text)
      InReview -> ("in_review" :: Text)
      Closed -> ("closed" :: Text)
      Merged -> ("merged" :: Text)

instance Aeson.FromJSON ContributionStatus where
  parseJSON =
    Aeson.withText "ContributionStatus" \case
      "draft" -> pure Draft
      "in_review" -> pure InReview
      "closed" -> pure Closed
      "merged" -> pure Merged
      _ -> fail "Invalid contribution status"

instance Hasql.EncodeValue ContributionStatus where
  encodeValue =
    PG.encodeValue & contramap \case
      Draft -> ("draft" :: Text)
      InReview -> ("in_review" :: Text)
      Closed -> ("closed" :: Text)
      Merged -> ("merged" :: Text)

instance FromHttpApiData ContributionStatus where
  parseQueryParam = \case
    "draft" -> Right Draft
    "in_review" -> Right InReview
    "closed" -> Right Closed
    "merged" -> Right Merged
    _ -> Left "Invalid contribution status"

instance Hasql.DecodeValue ContributionStatus where
  decodeValue = do
    Hasql.decodeValue @Text & Hasql.refine \case
      "draft" -> Right Draft
      "in_review" -> Right InReview
      "closed" -> Right Closed
      "merged" -> Right Merged
      _ -> Left "Invalid contribution status"

data Contribution = Contribution
  { contributionId :: ContributionId,
    projectId :: ProjectId,
    number :: ContributionNumber,
    title :: Text,
    description :: Maybe Text,
    status :: ContributionStatus,
    sourceBranchId :: BranchId,
    targetBranchId :: BranchId,
    bestCommonAncestorCausalId :: Maybe CausalId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    author :: UserId
  }
  deriving (Show, Eq, Ord)

instance Hasql.DecodeRow Contribution where
  decodeRow = do
    contributionId <- PG.decodeField
    projectId <- PG.decodeField
    number <- PG.decodeField
    title <- PG.decodeField
    description <- PG.decodeField
    status <- PG.decodeField
    sourceBranchId <- PG.decodeField
    targetBranchId <- PG.decodeField
    bestCommonAncestorCausalId <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    author <- PG.decodeField
    pure Contribution {..}
