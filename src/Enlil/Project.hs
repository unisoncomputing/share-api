{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Enlil.Project where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Enlil.IDs (ProjectId, ProjectSlug (..), UserId)
import Enlil.Postgres qualified as PG
import Enlil.Prelude

data ProjectVisibility = ProjectPrivate | ProjectPublic
  deriving (Show, Eq, Ord)

instance FromJSON ProjectVisibility where
  parseJSON = Aeson.withText "ProjectVisibility" $ \case
    "private" -> pure ProjectPrivate
    "public" -> pure ProjectPublic
    _ -> fail "Invalid project visibility"

instance ToJSON ProjectVisibility where
  toJSON = \case
    ProjectPrivate -> "private"
    ProjectPublic -> "public"

instance PG.DecodeValue ProjectVisibility where
  decodeValue =
    PG.decodeValue
      <&> \case
        True -> ProjectPrivate
        False -> ProjectPublic

instance PG.EncodeValue ProjectVisibility where
  encodeValue =
    PG.encodeValue
      & contramap \case
        ProjectPrivate -> True
        ProjectPublic -> False

data Project = Project
  { projectId :: ProjectId,
    ownerUserId :: UserId,
    slug :: ProjectSlug,
    summary :: Maybe Text,
    tags :: Set ProjectTag,
    visibility :: ProjectVisibility,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Ord, Generic)

instance PG.DecodeRow Project where
  decodeRow = do
    projectId <- PG.decodeField
    ownerUserId <- PG.decodeField
    slug <- PG.decodeField
    summary <- PG.decodeField
    tags <- Set.fromList <$> PG.decodeField
    visibility <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    pure $ Project {..}

data ProjectTag
  deriving stock (Show, Eq, Ord)

instance FromJSON ProjectTag where
  parseJSON =
    Aeson.withText "ProjectTag" $
      Text.toLower >>> \case
        other -> fail $ "Unknown project tag: " <> show other

instance ToJSON ProjectTag where
  toJSON = \case {}

instance PG.EncodeValue ProjectTag where
  encodeValue =
    PG.encodeValue @Text
      & contramap \case {}

instance PG.DecodeValue ProjectTag where
  decodeValue =
    PG.decodeValue @Text <&> \case
      _ -> error "Invalid ProjectTag"
