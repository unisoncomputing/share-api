{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Share.Web.Share.Releases.Types where

import Data.Aeson
import Data.Time (UTCTime)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Share.IDs
import Share.Postgres.IDs
import Share.Prelude
import Share.Release

data ReleaseStatusFilter
  = OnlyPublished
  | OnlyDeprecated
  | AllReleases
  deriving stock (Eq, Show)

instance FromHttpApiData ReleaseStatusFilter where
  parseQueryParam = \case
    "published" -> Right OnlyPublished
    "deprecated" -> Right OnlyDeprecated
    "any" -> Right AllReleases
    _ -> Left "Invalid release status filter"

instance ToHttpApiData ReleaseStatusFilter where
  toQueryParam = \case
    OnlyPublished -> "published"
    OnlyDeprecated -> "deprecated"
    AllReleases -> "any"

data APIReleaseStatus
  = APIPublishedRelease UTCTime (Maybe (PrefixedID "@" UserHandle))
  | APIDeprecatedRelease UTCTime (Maybe (PrefixedID "@" UserHandle))
  deriving stock (Show, Eq, Ord)

instance ToJSON APIReleaseStatus where
  toJSON = \case
    APIPublishedRelease publishedAt publishedBy ->
      object
        [ "status" .= ("published" :: Text),
          "publishedAt" .= publishedAt,
          "publishedBy" .= publishedBy
        ]
    APIDeprecatedRelease deprecatedAt deprecatedBy ->
      object
        [ "status" .= ("deprecated" :: Text),
          "deprecatedAt" .= deprecatedAt,
          "deprecatedBy" .= deprecatedBy
        ]

-- | The api format for a release.
data APIRelease = APIRelease
  { version :: ReleaseVersion,
    causalHashUnsquashed :: PrefixedHash "#" CausalHash,
    causalHashSquashed :: PrefixedHash "#" CausalHash,
    projectRef :: ProjectShortHand,
    createdAt :: UTCTime,
    createdBy :: PrefixedID "@" UserHandle,
    updatedAt :: UTCTime,
    status :: APIReleaseStatus
  }
  deriving stock (Eq, Show)

instance ToJSON APIRelease where
  toJSON APIRelease {..} =
    object
      [ "version" .= version,
        "causalHashUnsquashed" .= causalHashUnsquashed,
        "causalHashSquashed" .= causalHashSquashed,
        "projectRef" .= projectRef,
        "createdAt" .= createdAt,
        "createdBy" .= createdBy,
        "updatedAt" .= updatedAt,
        "status" .= status
      ]

releaseToAPIRelease :: ProjectShortHand -> Release CausalHash UserHandle -> APIRelease
releaseToAPIRelease projectSH Release {..} =
  APIRelease
    { version,
      causalHashUnsquashed = PrefixedHash unsquashedCausal,
      causalHashSquashed = PrefixedHash squashedCausal,
      projectRef = projectSH,
      createdAt,
      createdBy = PrefixedID createdBy,
      updatedAt,
      status = releaseStatusToAPIReleaseStatus status
    }

releaseStatusToAPIReleaseStatus :: ReleaseStatus UserHandle -> APIReleaseStatus
releaseStatusToAPIReleaseStatus = \case
  PublishedRelease publishedAt publishedBy -> APIPublishedRelease publishedAt (PrefixedID <$> publishedBy)
  DeprecatedRelease deprecatedAt deprecatedBy -> APIDeprecatedRelease deprecatedAt (PrefixedID <$> deprecatedBy)

data StatusUpdate
  = MakePublished
  | MakeDeprecated
  deriving stock (Eq, Show)

instance FromJSON StatusUpdate where
  parseJSON = withText "StatusUpdate" $ \case
    "published" -> pure MakePublished
    "deprecated" -> pure MakeDeprecated
    txt -> fail $ "Invalid status update: " <> show txt

data UpdateReleaseRequest = UpdateReleaseRequest
  { status :: Maybe StatusUpdate
  }

instance FromJSON UpdateReleaseRequest where
  parseJSON = withObject "UpdateReleaseRequest" $ \obj -> do
    status <- obj .:? "status"
    pure UpdateReleaseRequest {..}

data CreateReleaseRequest = CreateReleaseRequest
  { causalHash :: PrefixedHash "#" CausalHash,
    releaseVersion :: ReleaseVersion
  }

instance FromJSON CreateReleaseRequest where
  parseJSON = withObject "CreateReleaseRequest" $ \obj ->
    CreateReleaseRequest
      <$> obj
        .: "causalHash"
      <*> (ReleaseVersion <$> obj .: "major" <*> obj .: "minor" <*> obj .: "patch")
