{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Share.Web.Share.Projects.Types
  ( ProjectOwner (..),
    projectToAPI,
    APIProject (..),
    CreateProjectRequest (..),
    CreateProjectResponse (..),
    GetProjectResponse,
    ListProjectsResponse (..),
    UpdateProjectRequest (..),
    UpdateProjectResponse (..),
    FavProjectRequest (..),
    FavData (..),
    CatalogCategory (..),
    DownloadStats (..),
    ReleaseDownloadStats (..),
    ContributionStats (..),
    TicketStats (..),
    IsPremiumProject (..),
    APIProjectBranchAndReleaseDetails (..),
    PermissionsInfo (..),
    IsSubscribed (..),
  )
where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Project (Project (..), ProjectTag, ProjectVisibility (..))
import Share.Utils.API
import Share.Web.Authorization.Types (PermissionsInfo (PermissionsInfo))

projectToAPI :: ProjectOwner -> Project -> APIProject
projectToAPI projectOwner Project {slug, visibility, createdAt, updatedAt, summary, tags} =
  APIProject
    { owner = projectOwner,
      summary,
      tags,
      slug,
      visibility,
      createdAt,
      updatedAt
    }

data ProjectOwner
  = OrganizationOwner {ownerHandle :: UserHandle, ownerName :: Maybe Text}
  | UserOwner {ownerHandle :: UserHandle, ownerName :: Maybe Text}
  deriving (Show, Eq)

instance PG.DecodeRow ProjectOwner where
  decodeRow = do
    ownerHandle <- PG.decodeField
    ownerName <- PG.decodeField
    isOrg <- PG.decodeField
    pure $
      if isOrg
        then OrganizationOwner {..}
        else UserOwner {..}

instance ToJSON ProjectOwner where
  toJSON (OrganizationOwner {..}) =
    Aeson.object
      [ "handle" .= (IDs.toText $ PrefixedID @"@" ownerHandle),
        "name" .= ownerName,
        "type" .= ("organization" :: Text)
      ]
  toJSON (UserOwner {..}) =
    Aeson.object
      [ "handle" .= (IDs.toText $ PrefixedID @"@" ownerHandle),
        "name" .= ownerName,
        "type" .= ("user" :: Text)
      ]

instance FromJSON ProjectOwner where
  parseJSON = Aeson.withObject "ProjectOwner" $ \o -> do
    typ <- o .: "type"
    case typ of
      ("organization" :: Text) -> do
        ownerHandle <- o .: "handle"
        ownerName <- o .:? "name"
        pure $ OrganizationOwner {..}
      ("user" :: Text) -> do
        ownerHandle <- o .: "handle"
        ownerName <- o .:? "name"
        pure $ UserOwner {..}
      _ -> fail $ "Unknown ProjectOwner type: " <> show typ

data FavData = FavData
  { numFavs :: Int64,
    isFaved :: Bool
  }
  deriving stock (Eq, Show)

instance PG.DecodeRow FavData where
  decodeRow = do
    numFavs <- PG.decodeField
    isFaved <- PG.decodeField
    pure FavData {..}

instance ToJSON FavData where
  toJSON FavData {..} =
    object
      [ "numFavs" .= numFavs,
        "isFaved" .= isFaved
      ]

instance FromJSON FavData where
  parseJSON = Aeson.withObject "FavData" $ \o -> do
    numFavs <- o .: "numFavs"
    isFaved <- o .: "isFaved"
    pure FavData {..}

data APIProjectBranchAndReleaseDetails = APIProjectBranchAndReleaseDetails
  { defaultBranch :: Maybe BranchName,
    latestRelease :: Maybe ReleaseVersion
  }
  deriving stock (Eq, Show)

instance ToJSON APIProjectBranchAndReleaseDetails where
  toJSON APIProjectBranchAndReleaseDetails {..} =
    object
      [ "defaultBranch" .= defaultBranch,
        "latestRelease" .= latestRelease
      ]

instance FromJSON APIProjectBranchAndReleaseDetails where
  parseJSON = Aeson.withObject "APIProjectBranchAndReleaseDetails" $ \o -> do
    defaultBranch <- o .:? "defaultBranch"
    latestRelease <- o .:? "latestRelease"
    pure APIProjectBranchAndReleaseDetails {..}

data APIProject = APIProject
  { owner :: ProjectOwner,
    slug :: ProjectSlug,
    summary :: Maybe Text,
    visibility :: ProjectVisibility,
    tags :: Set ProjectTag,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

instance ToJSON APIProject where
  toJSON APIProject {..} =
    object $
      [ "owner" .= owner,
        "slug" .= slug,
        "summary" .= summary,
        "visibility" .= visibility,
        "tags" .= tags,
        "createdAt" .= createdAt,
        "updatedAt" .= updatedAt
      ]

instance FromJSON APIProject where
  parseJSON = Aeson.withObject "APIProject" $ \o -> do
    owner <- o .: "owner"
    slug <- o .: "slug"
    summary <- o .:? "summary"
    visibility <- o .: "visibility"
    tags <- o .:? "tags" .!= Set.empty
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"
    pure APIProject {..}

data CreateProjectRequest = CreateProjectRequest
  { summary :: Maybe Text,
    visibility :: ProjectVisibility,
    tags :: Set ProjectTag
  }

instance Aeson.ToJSON CreateProjectRequest where
  toJSON CreateProjectRequest {..} =
    object
      [ "summary" .= summary,
        "visibility" .= visibility,
        "tags" .= tags
      ]

instance Aeson.FromJSON CreateProjectRequest where
  parseJSON = Aeson.withObject "CreateProjectRequest" $ \o -> do
    summary <- o .:? "summary"
    visibility <- o .:? "visibility" .!= ProjectPrivate
    tags <- o .:? "tags" .!= Set.empty
    pure CreateProjectRequest {..}

data CreateProjectResponse = CreateProjectResponse

instance Aeson.ToJSON CreateProjectResponse where
  toJSON CreateProjectResponse = Aeson.object []

instance Aeson.FromJSON CreateProjectResponse where
  parseJSON _ = pure CreateProjectResponse

-- | A list of daily downloads for a project, limited to the last 28 days (4 weeks)
-- Listed from [most recent -> least recent]
newtype DownloadStats = DownloadStats [Int64]
  deriving stock (Eq, Show)

instance Aeson.ToJSON DownloadStats where
  toJSON (DownloadStats stats) = toJSON stats

instance Aeson.FromJSON DownloadStats where
  parseJSON arr = do
    stats <- parseJSON arr
    pure $ DownloadStats stats

data ReleaseDownloadStats = ReleaseDownloadStats
  { releaseDownloads :: DownloadStats
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ReleaseDownloadStats where
  toJSON ReleaseDownloadStats {..} =
    object
      [ "releaseDownloads" .= releaseDownloads
      ]

instance Aeson.FromJSON ReleaseDownloadStats where
  parseJSON = Aeson.withObject "ReleaseDownloadStats" $ \o -> do
    releaseDownloads <- o .: "releaseDownloads"
    pure ReleaseDownloadStats {..}

data ContributionStats = ContributionStats
  { inReview :: Int,
    draft :: Int,
    closed :: Int,
    merged :: Int
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ContributionStats where
  toJSON ContributionStats {..} =
    object
      [ "numActiveContributions" .= inReview,
        "numDraftContributions" .= draft,
        "numClosedContributions" .= closed,
        "numMergedContributions" .= merged
      ]

instance Aeson.FromJSON ContributionStats where
  parseJSON = Aeson.withObject "ContributionStats" $ \o -> do
    inReview <- o .: "numActiveContributions"
    draft <- o .: "numDraftContributions"
    closed <- o .: "numClosedContributions"
    merged <- o .: "numMergedContributions"
    pure ContributionStats {..}

instance PG.DecodeRow ContributionStats where
  decodeRow = do
    inReview <- fromIntegral @Int64 <$> PG.decodeField
    draft <- fromIntegral @Int64 <$> PG.decodeField
    closed <- fromIntegral @Int64 <$> PG.decodeField
    merged <- fromIntegral @Int64 <$> PG.decodeField
    pure $ ContributionStats {..}

data TicketStats = TicketStats
  { numOpenTickets :: Int,
    numClosedTickets :: Int
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON TicketStats where
  toJSON TicketStats {..} =
    object
      [ "numOpenTickets" .= numOpenTickets,
        "numClosedTickets" .= numClosedTickets
      ]

instance Aeson.FromJSON TicketStats where
  parseJSON = Aeson.withObject "TicketStats" $ \o -> do
    numOpenTickets <- o .: "numOpenTickets"
    numClosedTickets <- o .: "numClosedTickets"
    pure TicketStats {..}

instance PG.DecodeRow TicketStats where
  decodeRow = do
    numOpenTickets <- fromIntegral @Int64 <$> PG.decodeField
    numClosedTickets <- fromIntegral @Int64 <$> PG.decodeField
    pure $ TicketStats {..}

newtype IsPremiumProject = IsPremiumProject
  { isPremiumProject :: Bool
  }
  deriving (Show)
  deriving (ToJSON, FromJSON) via (AtKey "isPremiumProject" Bool)

newtype IsSubscribed = IsSubscribed
  { isSubscribed :: Bool
  }
  deriving (Show)
  deriving (ToJSON, FromJSON) via (AtKey "isSubscribed" Bool)

type GetProjectResponse =
  APIProject
    :++ FavData
    :++ APIProjectBranchAndReleaseDetails
    :++ ReleaseDownloadStats
    :++ ContributionStats
    :++ TicketStats
    :++ PermissionsInfo
    :++ IsPremiumProject
    :++ IsSubscribed

data ListProjectsResponse = ListProjectsResponse
  { projects :: [APIProject :++ FavData]
  }

instance Aeson.ToJSON ListProjectsResponse where
  toJSON (ListProjectsResponse projects) = toJSON projects

instance Aeson.FromJSON ListProjectsResponse where
  parseJSON v = ListProjectsResponse <$> Aeson.parseJSON v

data UpdateProjectRequest = UpdateProjectRequest
  { summary :: NullableUpdate Text,
    visibility :: Maybe ProjectVisibility,
    tags :: SetUpdate ProjectTag
  }

instance Aeson.ToJSON UpdateProjectRequest where
  toJSON UpdateProjectRequest {..} =
    object
      [ "summary" .= nullableUpdateToJSON summary,
        "visibility" .= visibility,
        "tags" .= tags
      ]

instance Aeson.FromJSON UpdateProjectRequest where
  parseJSON = Aeson.withObject "UpdateProjectRequest" $ \obj -> do
    summary <- parseNullableUpdate obj "summary"
    visibility <- obj .:? "visibility"
    tags <- obj .:? "tags" .!= emptySetUpdate
    pure UpdateProjectRequest {..}

data UpdateProjectResponse = UpdateProjectResponse

instance Aeson.ToJSON UpdateProjectResponse where
  toJSON UpdateProjectResponse = Aeson.object []

data FavProjectRequest = FavProjectRequest
  { isFaved :: Bool
  }
  deriving (Show)

instance Aeson.ToJSON FavProjectRequest where
  toJSON FavProjectRequest {..} =
    Aeson.object
      [ "isFaved" .= isFaved
      ]

instance Aeson.FromJSON FavProjectRequest where
  parseJSON = Aeson.withObject "FavProjectRequest" $ \o ->
    FavProjectRequest <$> o Aeson..: "isFaved"

data CatalogCategory = CatalogCategory
  { name :: CategoryName,
    projects :: [APIProject :++ FavData]
  }
  deriving (Show)

instance Aeson.ToJSON CatalogCategory where
  toJSON CatalogCategory {..} =
    Aeson.object
      [ "name" .= name,
        "projects" .= projects
      ]
