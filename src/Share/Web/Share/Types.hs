{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Types where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NEL
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Servant.API
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

instance FromJSON UserKind where
  parseJSON = Aeson.withText "UserKind" $ \case
    "user" -> pure UserKind
    "org" -> pure OrgKind
    t -> fail $ "Invalid UserKind: " <> Text.unpack t

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

instance FromJSON DescribeUserProfile where
  parseJSON = Aeson.withObject "DescribeUserProfile" $ \o -> do
    kind <- o .: "kind"
    case (kind :: Text) of
      "user" -> do
        handle <- o .: "handle"
        name <- o .:? "name"
        avatarUrl <- fmap unpackURI <$> o .:? "avatarUrl"
        userId <- o .: "userId"
        bio <- o .:? "bio"
        website <- o .:? "website"
        location <- o .:? "location"
        twitterHandle <- o .:? "twitterHandle"
        pronouns <- o .:? "pronouns"
        permissions <- o .: "permissions"
        pure $ DescribeUserProfile {bio, website, location, twitterHandle, pronouns, permissions, displayInfo = UnifiedUser (UserDisplayInfo {handle, name, avatarUrl, userId})}
      "org" -> do
        userInfo <- o .: "user"
        handle <- userInfo Aeson..: "handle"
        name <- userInfo Aeson..:? "name"
        avatarUrl <- fmap unpackURI <$> userInfo Aeson..:? "avatarUrl"
        userId <- userInfo Aeson..: "userId"
        let user = UserDisplayInfo {handle, name, avatarUrl, userId}
        orgId <- o .: "orgId"
        isCommercial <- o .: "isCommercial"
        bio <- userInfo Aeson..:? "bio"
        website <- userInfo Aeson..:? "website"
        twitterHandle <- userInfo Aeson..:? "twitterHandle"
        pronouns <- userInfo Aeson..:? "pronouns"
        permissions <- o .: "permissions"
        pure $ DescribeUserProfile {bio, website, location = Nothing, twitterHandle, pronouns, permissions, displayInfo = UnifiedOrg (OrgDisplayInfo {user, orgId, isCommercial})}
      _ -> fail $ "Unknown kind for DescribeUserProfile: " <> show kind

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

instance FromJSON ReadmeResponse where
  parseJSON = Aeson.withObject "ReadmeResponse" $ \o -> do
    readMe <- o Aeson..: "readMe"
    markdownReadMe <- o Aeson..: "markdownReadMe"
    pure ReadmeResponse {readMe, markdownReadMe}

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

instance FromJSON DocResponse where
  parseJSON = Aeson.withObject "DocResponse" $ \o -> do
    doc <- o Aeson..: "doc"
    pure DocResponse {doc}

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

instance FromJSON SearchResult where
  parseJSON = Aeson.withObject "SearchResult" $ \o -> do
    tag <- o Aeson..: "tag"
    case (tag :: Text) of
      "user" -> do
        handle <- o Aeson..: "handle"
        name <- o Aeson..:? "name"
        avatarUrl <- o Aeson..:? "avatarUrl"
        userId <- o Aeson..: "userId"
        pure $ SearchResultUserLike $ UnifiedUser $ UserDisplayInfo {handle, name, avatarUrl, userId}
      "org" -> do
        orgId <- o Aeson..: "orgId"
        isCommercial <- o Aeson..: "isCommercial"
        user <- o Aeson..: "user"
        handle <- user Aeson..: "handle"
        name <- user Aeson..:? "name"
        avatarUrl <- user Aeson..:? "avatarUrl"
        userId <- user Aeson..: "userId"
        pure $ SearchResultUserLike $ UnifiedOrg $ OrgDisplayInfo {user = UserDisplayInfo {handle, name, avatarUrl, userId}, orgId, isCommercial}
      "project" -> do
        projectRef <- o Aeson..: "projectRef"
        summary <- o Aeson..:? "summary"
        visibility <- o Aeson..: "visibility"
        pure $ SearchResultProject projectRef summary visibility
      t -> fail $ "Invalid SearchResult tag: " <> Text.unpack t

-- | Cloud/Unison Subscription plan tier
data PlanTier = Free | Starter | Pro
  deriving (Show, Eq, Ord)

instance ToJSON PlanTier where
  toJSON = \case
    Free -> "Free"
    Starter -> "Starter"
    Pro -> "Pro"

instance FromJSON PlanTier where
  parseJSON = Aeson.withText "PlanTier" $ \case
    "Free" -> pure Free
    "Starter" -> pure Starter
    "Pro" -> pure Pro
    t -> fail $ "Invalid PlanTier: " <> Text.unpack t

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

instance FromJSON UserAccountInfo where
  parseJSON = Aeson.withObject "UserAccountInfo" $ \o -> do
    kind <- o Aeson..: "kind"
    case (kind :: Text) of
      "user" -> do
        handle <- o Aeson..: "handle"
        name <- o Aeson..:? "name"
        avatarUrl <- o Aeson..:? "avatarUrl"
        userId <- o Aeson..: "userId"
        isSuperadmin <- o Aeson..: "isSuperadmin"
        organizationMemberships <- o Aeson..: "organizationMemberships"
        completedTours <- o Aeson..: "completedTours"
        primaryEmail <- o Aeson..:? "primaryEmail"
        planTier <- o Aeson..: "planTier"
        hasUnreadNotifications <- o Aeson..: "hasUnreadNotifications"
        pure $ UserAccountInfo
          { primaryEmail,
            completedTours,
            organizationMemberships,
            isSuperadmin,
            planTier,
            displayInfo = UnifiedUser $ UserDisplayInfo {handle, name, avatarUrl, userId},
            hasUnreadNotifications
          }
      "org" -> do
        userInfo <- o Aeson..: "user"
        handle <- userInfo Aeson..: "handle"
        name <- userInfo Aeson..:? "name"
        avatarUrl <- userInfo Aeson..:? "avatarUrl"
        userId <- userInfo Aeson..: "userId"
        let user = UserDisplayInfo {handle, name, avatarUrl, userId}
        orgId <- o Aeson..: "orgId"
        isCommercial <- o Aeson..: "isCommercial"
        organizationMemberships <- o Aeson..: "organizationMemberships"
        planTier <- o Aeson..: "planTier"
        hasUnreadNotifications <- o Aeson..: "hasUnreadNotifications"
        pure $ UserAccountInfo
          { primaryEmail = Nothing,
            completedTours = [],
            organizationMemberships,
            isSuperadmin = False,
            planTier,
            displayInfo = UnifiedOrg $ OrgDisplayInfo {orgId, isCommercial, user},
            hasUnreadNotifications
          }
      t -> fail $ "Invalid UserAccountInfo kind: " <> Text.unpack t

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

instance FromJSON DefinitionNameSearchResult where
  parseJSON = Aeson.withObject "DefinitionNameSearchResult" $ \o -> do
    token <- o Aeson..: "token"
    tag <- o Aeson..: "tag"
    pure DefinitionNameSearchResult {token, tag}

newtype DefinitionSearchResults = DefinitionSearchResults
  { results :: [DefinitionSearchResult]
  }

instance ToJSON DefinitionSearchResults where
  toJSON DefinitionSearchResults {..} =
    Aeson.object
      [ "results" .= results
      ]

instance FromJSON DefinitionSearchResults where
  parseJSON = Aeson.withObject "DefinitionSearchResults" $ \o -> do
    results <- o Aeson..: "results"
    pure DefinitionSearchResults {results}

data DefinitionSearchResult = DefinitionSearchResult
  { fqn :: Name,
    summary :: DefSync.TermOrTypeSummary,
    project :: ProjectShortHand,
    branchRef :: BranchOrReleaseShortHand
  }

instance ToJSON DefinitionSearchResult where
  toJSON DefinitionSearchResult {..} =
    Aeson.object
      [ "fqn" Aeson..= fqn,
        "projectRef" Aeson..= project,
        "branchRef" Aeson..= branchRef,
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

instance FromJSON DefinitionSearchResult where
  parseJSON = Aeson.withObject "DefinitionSearchResult" $ \o -> do
    fqn <- o Aeson..: "fqn"
    project <- o Aeson..: "projectRef"
    branchRef <- o Aeson..: "branchRef"
    kind <- o Aeson..: "kind"
    definition <- o Aeson..: "definition"
    summary <- case kind of
      Aeson.String "term" -> do
        definitionObj <- case definition of
          Aeson.Object obj -> pure obj
          _ -> fail "Expected object for term definition"
        displayName <- definitionObj Aeson..: "displayName"
        hash <- definitionObj Aeson..: "hash"
        summaryText <- definitionObj Aeson..: "summary"
        tag <- definitionObj Aeson..: "tag"
        pure $ DefSync.ToTTermSummary $ TermSummary {displayName, hash, summary = summaryText, tag}
      Aeson.String "type" -> do
        definitionObj <- case definition of
          Aeson.Object obj -> pure obj
          _ -> fail "Expected object for type definition"
        displayName <- definitionObj Aeson..: "displayName"
        hash <- definitionObj Aeson..: "hash"
        summaryText <- definitionObj Aeson..: "summary"
        tag <- definitionObj Aeson..: "tag"
        pure $ DefSync.ToTTypeSummary $ TypeSummary {displayName, hash, summary = summaryText, tag}
      _ -> fail "Invalid definition kind"
    pure DefinitionSearchResult {fqn, summary, project, branchRef}

data SearchKind
  = SearchKindProjects
  | SearchKindUsers
  deriving (Eq, Show, Ord, Bounded, Enum)

newtype SearchKinds = SearchKinds (NESet.NESet SearchKind)

allSearchKinds :: SearchKinds
allSearchKinds = SearchKinds $ NESet.fromList $ NEL.fromList [minBound .. maxBound :: SearchKind]

instance FromHttpApiData SearchKinds where
  parseQueryParam q = do
    let parts =
          q
            & Text.splitOn ","
            & fmap Text.strip
    parsed <- for parts $ \part ->
      case parseQueryParam @SearchKind part of
        Right kind -> Right kind
        Left err -> Left $ "Invalid search kind: " <> err
    case NESet.nonEmptySet (Set.fromList parsed) of
      Just kinds -> Right $ SearchKinds kinds
      Nothing -> do
        Left $ "Invalid search kinds: " <> q

instance ToHttpApiData SearchKinds where
  toQueryParam (SearchKinds kinds) =
    kinds
      & toList
      <&> toQueryParam
      & Text.intercalate ","

instance FromHttpApiData SearchKind where
  parseQueryParam = \case
    "projects" -> Right SearchKindProjects
    "users" -> Right SearchKindUsers
    _ -> Left "Invalid search kind"

instance ToHttpApiData SearchKind where
  toQueryParam = \case
    SearchKindProjects -> "projects"
    SearchKindUsers -> "users"

data UserSearchKind
  = UserSearchKindDefault
  | -- Only search handle, and only by prefix (useful for tab-completion)
    UserSearchKindHandlePrefix

instance FromHttpApiData UserSearchKind where
  parseQueryParam = \case
    "default" -> Right UserSearchKindDefault
    "handle-prefix" -> Right UserSearchKindHandlePrefix
    _ -> Left "Invalid user search kind"

instance ToHttpApiData UserSearchKind where
  toQueryParam = \case
    UserSearchKindDefault -> "default"
    UserSearchKindHandlePrefix -> "handle-prefix"

data ProjectSearchKind
  = ProjectSearchKindWebSearch
  | ProjectSearchKindSlugPrefix
  | -- Only search slug, and only by infix (useful for tab-completion)
    ProjectSearchKindSlugInfix

instance FromHttpApiData ProjectSearchKind where
  parseQueryParam = \case
    "web-search" -> Right ProjectSearchKindWebSearch
    "slug-prefix" -> Right ProjectSearchKindSlugPrefix
    "slug-infix" -> Right ProjectSearchKindSlugInfix
    _ -> Left "Invalid project search kind"

instance ToHttpApiData ProjectSearchKind where
  toQueryParam = \case
    ProjectSearchKindWebSearch -> "web-search"
    ProjectSearchKindSlugPrefix -> "slug-prefix"
    ProjectSearchKindSlugInfix -> "slug-infix"
