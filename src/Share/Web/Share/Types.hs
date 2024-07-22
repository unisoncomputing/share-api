{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Types where

import Data.Aeson (KeyValue ((.=)), ToJSON (..))
import Data.Aeson qualified as Aeson
import Network.URI (URI)
import Share.BackgroundJobs.Search.DefinitionSync.Types qualified as DefSync
import Share.IDs
import Share.Prelude
import Share.Project (ProjectVisibility)
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Utils.URI
import Unison.Name (Name)
import Unison.Server.Doc (Doc)

data UpdateUserRequest = UpdateUserRequest
  { name :: NullableUpdate Text,
    avatarUrl :: NullableUpdate URIParam,
    bio :: NullableUpdate Text,
    website :: NullableUpdate Text,
    location :: NullableUpdate Text,
    twitterHandle :: NullableUpdate Text,
    pronouns :: NullableUpdate Text
  }
  deriving (Show)

instance Aeson.FromJSON UpdateUserRequest where
  parseJSON = Aeson.withObject "UpdateUserRequest" $ \o -> do
    name <- parseNullableUpdate o "name"
    avatarUrl <- parseNullableUpdate o "avatarUrl"
    bio <- parseNullableUpdate o "bio"
    website <- parseNullableUpdate o "website"
    location <- parseNullableUpdate o "location"
    twitterHandle <- parseNullableUpdate o "twitterHandle"
    pronouns <- parseNullableUpdate o "pronouns"
    pure UpdateUserRequest {..}

data DescribeUserProfile = DescribeUserProfile
  { handle :: UserHandle,
    name :: Maybe Text,
    avatarUrl :: Maybe URIParam,
    bio :: Maybe Text,
    website :: Maybe Text,
    location :: Maybe Text,
    twitterHandle :: Maybe Text,
    pronouns :: Maybe Text
  }
  deriving (Show)

instance ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile {..} =
    Aeson.object
      [ "handle" .= fromId @UserHandle @Text handle,
        "name" .= name,
        "avatarUrl" .= avatarUrl,
        "bio" .= bio,
        "website" .= website,
        "location" .= location,
        "twitterHandle" .= twitterHandle,
        "pronouns" .= pronouns
      ]

data ReadmeResponse = ReadmeResponse
  { readMe :: Maybe Doc
  }
  deriving (Show)

instance ToJSON ReadmeResponse where
  toJSON ReadmeResponse {..} =
    Aeson.object
      [ "readMe" .= readMe
      ]

-- | A reponse for rendering docs.
data DocResponse = DocResponse
  { doc :: Maybe Doc
  }
  deriving (Show)

instance ToJSON DocResponse where
  toJSON DocResponse {..} =
    Aeson.object
      [ "doc" .= doc
      ]

data SearchResult
  = -- | handle displayName avatarUrl
    SearchResultUser UserHandle (Maybe Text) (Maybe URIParam)
  | -- | shorthand summary visibility
    SearchResultProject ProjectShortHand (Maybe Text) ProjectVisibility
  deriving (Show)

instance ToJSON SearchResult where
  toJSON = \case
    SearchResultUser handle name avatarUrl ->
      Aeson.object
        [ "handle" .= fromId @UserHandle @Text handle,
          "name" .= name,
          "avatarUrl" .= avatarUrl,
          "tag" .= ("User" :: Text)
        ]
    SearchResultProject shorthand summary visibility ->
      Aeson.object
        [ "projectRef" .= shorthand,
          "summary" .= summary,
          "tag" .= ("Project" :: Text),
          "visibility" .= visibility
        ]

data UserAccountInfo = UserAccountInfo
  { handle :: UserHandle,
    name :: Maybe Text,
    avatarUrl :: Maybe URIParam,
    userId :: UserId,
    primaryEmail :: Text,
    -- List of tours which the user has completed.
    completedTours :: [TourId],
    organizationMemberships :: [UserHandle]
  }
  deriving (Show)

instance ToJSON UserAccountInfo where
  toJSON UserAccountInfo {..} =
    Aeson.object
      [ "handle" .= fromId @UserHandle @Text handle,
        "name" .= name,
        "avatarUrl" .= avatarUrl,
        "primaryEmail" .= primaryEmail,
        "userId" .= userId,
        "completedTours" .= completedTours,
        "organizationMemberships" .= organizationMemberships
      ]

type PathSegment = Text

-- | Common type for displaying a user.
data UserDisplayInfo = UserDisplayInfo
  { handle :: UserHandle,
    name :: Maybe Text,
    avatarUrl :: Maybe URI,
    userId :: UserId
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON UserDisplayInfo where
  toJSON UserDisplayInfo {handle, name, avatarUrl, userId} =
    Aeson.object
      [ "handle" Aeson..= handle,
        "name" Aeson..= name,
        "avatarUrl" Aeson..= avatarUrl,
        "userId" Aeson..= userId
      ]

data DefinitionNameSearchResult
  = DefinitionNameSearchResult
  { token :: Text,
    kind :: Text
  }

data DefinitionSearchResult
  = DefinitionSearchResult
  { fqn :: Name,
    summary :: DefSync.TermOrTypeSummary,
    project :: ProjectShortHand,
    release :: ReleaseShortHand
  }
