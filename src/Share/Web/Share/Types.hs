{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Types where

import Data.Aeson (KeyValue ((.=)), ToJSON (..))
import Data.Aeson qualified as Aeson
import Share.BackgroundJobs.Search.DefinitionSync.Types (TermOrTypeTag)
import Share.BackgroundJobs.Search.DefinitionSync.Types qualified as DefSync
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Project (ProjectVisibility)
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Utils.URI
import Share.Web.Authorization.Types (RolePermission)
import Share.Web.Share.DisplayInfo.Types (OrgDisplayInfo (..), UnifiedDisplayInfo, UserDisplayInfo (..), UserLike (..))
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
  { bio :: Maybe Text,
    website :: Maybe Text,
    location :: Maybe Text,
    twitterHandle :: Maybe Text,
    pronouns :: Maybe Text,
    permissions :: Set RolePermission,
    displayInfo :: UnifiedDisplayInfo
  }
  deriving (Show)

instance ToJSON DescribeUserProfile where
  toJSON (DescribeUserProfile {bio, website, location, twitterHandle, pronouns, permissions, displayInfo}) =
    case displayInfo of
      UnifiedUser (UserDisplayInfo {handle, name, avatarUrl, userId}) ->
        Aeson.object
          [ "kind" .= ("user" :: Text),
            "avatarUrl" .= avatarUrl,
            "handle" .= handle,
            "name" .= name,
            "userId" .= userId,
            "pronouns" .= pronouns,
            "twitterHandle" .= twitterHandle,
            "bio" .= bio,
            "website" .= website,
            "location" .= location
          ]
      UnifiedOrg (OrgDisplayInfo {orgId, isCommercial, user = UserDisplayInfo {handle, name, avatarUrl, userId}}) ->
        Aeson.object
          [ "kind" .= ("org" :: Text),
            "user"
              .= Aeson.object
                [ "avatarUrl" .= avatarUrl,
                  "handle" .= handle,
                  "name" .= name,
                  "userId" .= userId,
                  "website" .= website,
                  "twitterHandle" .= twitterHandle
                ],
            "isCommercial" .= isCommercial,
            "permissions" .= permissions,
            "orgId" .= orgId
          ]

data ReadmeResponse = ReadmeResponse
  { readMe :: Maybe Doc,
    markdownReadMe :: Maybe Text
  }
  deriving (Show)

instance ToJSON ReadmeResponse where
  toJSON ReadmeResponse {..} =
    Aeson.object
      [ "readMe" .= readMe,
        "markdownReadMe" .= markdownReadMe
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
  = SearchResultUserLike UnifiedDisplayInfo
  | -- | shorthand summary visibility
    SearchResultProject ProjectShortHand (Maybe Text) ProjectVisibility
  deriving (Show)

instance ToJSON SearchResult where
  toJSON = \case
    SearchResultUserLike userLike ->
      case userLike of
        UnifiedUser (UserDisplayInfo {handle, name, avatarUrl, userId}) ->
          Aeson.object
            [ "tag" .= ("user" :: Text),
              "avatarUrl" .= avatarUrl,
              "handle" .= handle,
              "name" .= name,
              "userId" .= userId
            ]
        UnifiedOrg (OrgDisplayInfo {user = UserDisplayInfo {handle, name, avatarUrl, userId}, orgId, isCommercial}) ->
          Aeson.object
            [ "tag" .= ("org" :: Text),
              "orgId" .= orgId,
              "isCommercial" .= isCommercial,
              "user"
                .= Aeson.object
                  [ "avatarUrl" .= avatarUrl,
                    "handle" .= handle,
                    "name" .= name,
                    "userId" .= userId
                  ]
            ]
    SearchResultProject shorthand summary visibility ->
      Aeson.object
        [ "projectRef" .= shorthand,
          "summary" .= summary,
          "tag" .= ("project" :: Text),
          "visibility" .= visibility
        ]

-- | Cloud/Unison Subscription plan tier
data PlanTier = Free | Starter | Pro
  deriving (Show, Eq, Ord)

instance ToJSON PlanTier where
  toJSON = \case
    Free -> "Free"
    Starter -> "Starter"
    Pro -> "Pro"

instance PG.DecodeValue PlanTier where
  decodeValue =
    PG.decodeValue @Text
      <&> \case
        "Free" -> Free
        "Starter" -> Starter
        "Pro" -> Pro
        _ -> Free

data UserAccountInfo = UserAccountInfo
  { primaryEmail :: Maybe Email,
    -- List of tours which the user has completed.
    completedTours :: [TourId],
    organizationMemberships :: [UserHandle],
    isSuperadmin :: Bool,
    planTier :: PlanTier,
    displayInfo :: UnifiedDisplayInfo,
    hasUnreadNotifications :: Bool
  }
  deriving (Show)

instance ToJSON UserAccountInfo where
  toJSON UserAccountInfo {..} =
    case displayInfo of
      UnifiedUser (UserDisplayInfo {handle, name, avatarUrl, userId}) ->
        Aeson.object
          [ "kind" .= ("user" :: Text),
            "avatarUrl" .= avatarUrl,
            "handle" .= handle,
            "name" .= name,
            "userId" .= userId,
            "isSuperadmin" .= isSuperadmin,
            "organizationMemberships" .= organizationMemberships,
            "completedTours" .= completedTours,
            "primaryEmail" .= primaryEmail,
            "planTier" .= planTier,
            "hasUnreadNotifications" .= hasUnreadNotifications
          ]
      UnifiedOrg (OrgDisplayInfo {orgId, isCommercial, user = UserDisplayInfo {handle, name, avatarUrl, userId}}) ->
        Aeson.object
          [ "kind" .= ("org" :: Text),
            "user"
              .= Aeson.object
                [ "avatarUrl" .= avatarUrl,
                  "handle" .= handle,
                  "name" .= name,
                  "userId" .= userId
                ],
            "isCommercial" .= isCommercial,
            "organizationMemberships" .= organizationMemberships,
            "orgId" .= orgId,
            "planTier" .= planTier,
            "hasUnreadNotifications" .= hasUnreadNotifications
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
