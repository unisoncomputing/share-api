{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Types where

import Data.Aeson (KeyValue ((.=)), ToJSON (..))
import Data.Aeson qualified as Aeson
import Share.BackgroundJobs.Search.DefinitionSync.Types (TermOrTypeTag)
import Share.BackgroundJobs.Search.DefinitionSync.Types qualified as DefSync
import Share.IDs
import Share.Prelude
import Share.Project (ProjectVisibility)
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Utils.URI
import Share.Web.Authorization.Types (RolePermission)
import Share.Web.Share.DisplayInfo (OrgDisplayInfo (..), UserDisplayInfo (..))
import Unison.Name (Name)
import Unison.Server.Doc (Doc)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary (..), TypeSummary (..))

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

data UserKind = UserKind | OrgKind
  deriving (Show, Eq, Ord)

instance ToJSON UserKind where
  toJSON = \case
    UserKind -> "user"
    OrgKind -> "org"

data DescribeUserProfile = DescribeUserProfile
  { handle :: UserHandle,
    name :: Maybe Text,
    avatarUrl :: Maybe URIParam,
    bio :: Maybe Text,
    website :: Maybe Text,
    location :: Maybe Text,
    twitterHandle :: Maybe Text,
    pronouns :: Maybe Text,
    kind :: UserKind,
    permissions :: Set RolePermission
  }
  deriving (Show)

instance ToJSON DescribeUserProfile where
  toJSON (DescribeUserProfile handle name avatarUrl bio website location twitterHandle pronouns kind permissions) =
    Aeson.object
      [ "handle" .= fromId @UserHandle @Text handle,
        "name" .= name,
        "avatarUrl" .= avatarUrl,
        "bio" .= bio,
        "website" .= website,
        "location" .= location,
        "twitterHandle" .= twitterHandle,
        "pronouns" .= pronouns,
        "kind" .= kind,
        "permissions" Aeson..= permissions
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
  = SearchResultUser UserDisplayInfo
  | -- | shorthand summary visibility
    SearchResultProject ProjectShortHand (Maybe Text) ProjectVisibility
  | SearchResultOrg OrgDisplayInfo
  deriving (Show)

instance ToJSON SearchResult where
  toJSON = \case
    SearchResultUser (UserDisplayInfo {handle, name, avatarUrl, userId}) ->
      Aeson.object
        [ "handle" .= fromId @UserHandle @Text handle,
          "name" .= name,
          "avatarUrl" .= avatarUrl,
          "userId" .= userId,
          "tag" .= ("User" :: Text)
        ]
    SearchResultOrg (OrgDisplayInfo {user = UserDisplayInfo {handle, name, avatarUrl, userId}, orgId}) ->
      Aeson.object
        [ "handle" .= fromId @UserHandle @Text handle,
          "name" .= name,
          "avatarUrl" .= avatarUrl,
          "userId" .= userId,
          "orgId" .= orgId,
          "tag" .= ("Org" :: Text)
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
    primaryEmail :: Maybe Email,
    -- List of tours which the user has completed.
    completedTours :: [TourId],
    organizationMemberships :: [UserHandle],
    isSuperadmin :: Bool
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
        "organizationMemberships" .= organizationMemberships,
        "isSuperadmin" .= isSuperadmin
      ]

type PathSegment = Text

data DefinitionNameSearchResult = DefinitionNameSearchResult
  { token :: Name,
    tag :: TermOrTypeTag
  }

instance ToJSON DefinitionNameSearchResult where
  toJSON DefinitionNameSearchResult {..} =
    Aeson.object
      [ "token" .= token,
        "tag" .= tag
      ]

newtype DefinitionSearchResults = DefinitionSearchResults
  { results :: [DefinitionSearchResult]
  }

instance ToJSON DefinitionSearchResults where
  toJSON DefinitionSearchResults {..} =
    Aeson.object
      [ "results" .= results
      ]

data DefinitionSearchResult = DefinitionSearchResult
  { fqn :: Name,
    summary :: DefSync.TermOrTypeSummary,
    project :: ProjectShortHand,
    release :: ReleaseShortHand
  }

instance ToJSON DefinitionSearchResult where
  toJSON DefinitionSearchResult {..} =
    Aeson.object
      [ "fqn" Aeson..= fqn,
        "projectRef" Aeson..= project,
        "branchRef" Aeson..= release,
        "kind" Aeson..= kind,
        "definition" Aeson..= definition
      ]
    where
      (kind, definition) = case summary of
        DefSync.ToTTermSummary TermSummary {displayName, hash, summary, tag} ->
          ( Aeson.String "term",
            Aeson.object
              [ "displayName" Aeson..= displayName,
                "hash" Aeson..= hash,
                "summary" Aeson..= summary,
                "tag" Aeson..= tag
              ]
          )
        DefSync.ToTTypeSummary TypeSummary {displayName, hash, summary, tag} ->
          ( Aeson.String "type",
            Aeson.object
              [ "displayName" Aeson..= displayName,
                "hash" Aeson..= hash,
                "summary" Aeson..= summary,
                "tag" Aeson..= tag
              ]
          )
