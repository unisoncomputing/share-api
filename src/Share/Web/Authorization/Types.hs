{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Share.Web.Authorization.Types
  ( RolePermission (..),
    RoleRef (..),
    projectRoles,
    ProjectMaintainerPermissions (..),
    PermissionsInfo (..),
    AuthSubject (..),
    SubjectKind (..),
    GenericAuthSubject,
    ResolvedAuthSubject,
    DisplayAuthSubject,
    _UserSubject,
    _OrgSubject,
    _TeamSubject,
    RoleAssignment (..),
    authSubjectKind,
    resolvedAuthSubjectColumns,

    -- * Generic request/response types
    ListRolesResponse (..),
    AddRolesResponse (..),
    RemoveRolesResponse (..),
    AddRolesRequest (..),
    RemoveRolesRequest (..),
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON, ToJSON (..), object, withObject, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.UUID (UUID)
import Hasql.Decoders qualified as HasqlDecode
import Hasql.Decoders qualified as HasqlDecoders
import Hasql.Encoders qualified as HasqlEncode
import Hasql.Interpolate qualified as Hasql
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.API (AtKey (..))
import Share.Web.Share.Types (OrgDisplayInfo, TeamDisplayInfo, UserDisplayInfo)

data SubjectKind = UserSubjectKind | OrgSubjectKind | TeamSubjectKind
  deriving (Show)

instance Hasql.EncodeValue SubjectKind where
  encodeValue =
    HasqlEncode.enum
      \case
        UserSubjectKind -> "user"
        OrgSubjectKind -> "org"
        TeamSubjectKind -> "team"

instance Hasql.DecodeValue SubjectKind where
  decodeValue =
    HasqlDecode.enum
      \case
        "user" -> Just UserSubjectKind
        "org" -> Just OrgSubjectKind
        "team" -> Just TeamSubjectKind
        _ -> Nothing

data AuthSubject user org team
  = UserSubject user
  | OrgSubject org
  | TeamSubject team
  deriving (Show)

makePrisms ''AuthSubject

type GenericAuthSubject = AuthSubject SubjectId SubjectId SubjectId

type ResolvedAuthSubject = AuthSubject UserId OrgId TeamId

-- | Auth subject with info needed by the frontend
type DisplayAuthSubject = AuthSubject UserDisplayInfo OrgDisplayInfo TeamDisplayInfo

authSubjectKind :: AuthSubject user org team -> SubjectKind
authSubjectKind = \case
  UserSubject _ -> UserSubjectKind
  OrgSubject _ -> OrgSubjectKind
  TeamSubject _ -> TeamSubjectKind

resolvedAuthSubjectColumns :: ResolvedAuthSubject -> (SubjectKind, UUID)
resolvedAuthSubjectColumns = \case
  UserSubject id -> (UserSubjectKind, coerce id)
  OrgSubject id -> (OrgSubjectKind, coerce id)
  TeamSubject id -> (TeamSubjectKind, coerce id)

instance ToJSON ResolvedAuthSubject where
  toJSON = \case
    UserSubject id -> object ["kind" .= ("user" :: Text), "id" .= id]
    OrgSubject id -> object ["kind" .= ("org" :: Text), "id" .= id]
    TeamSubject id -> object ["kind" .= ("team" :: Text), "id" .= id]

instance ToJSON DisplayAuthSubject where
  toJSON = \case
    UserSubject user -> object ["kind" .= ("user" :: Text), "data" .= user]
    OrgSubject org -> object ["kind" .= ("org" :: Text), "data" .= org]
    TeamSubject team -> object ["kind" .= ("team" :: Text), "data" .= team]

instance FromJSON ResolvedAuthSubject where
  parseJSON = withObject "ResolvedAuthSubject" $ \o -> do
    kind <- o Aeson..: "kind"
    case (kind :: Text) of
      "user" -> UserSubject <$> o Aeson..: "id"
      "org" -> OrgSubject <$> o Aeson..: "id"
      "team" -> TeamSubject <$> o Aeson..: "id"
      _ -> fail "Invalid ResolvedAuthSubject"

--  Decoder for (subject.kind, id :: UUID)
instance Hasql.DecodeRow ResolvedAuthSubject where
  decodeRow = do
    PG.decodeField @SubjectKind >>= \case
      UserSubjectKind -> UserSubject <$> PG.decodeField @UserId
      OrgSubjectKind -> OrgSubject <$> PG.decodeField @OrgId
      TeamSubjectKind -> TeamSubject <$> PG.decodeField @TeamId

-- Decoder for (subject.id, subject.kind)
instance Hasql.DecodeRow GenericAuthSubject where
  decodeRow = do
    subjectId <- PG.decodeField @SubjectId
    PG.decodeField @SubjectKind >>= \case
      UserSubjectKind -> UserSubject <$> pure subjectId
      OrgSubjectKind -> OrgSubject <$> pure subjectId
      TeamSubjectKind -> TeamSubject <$> pure subjectId

-- | Permissions which are actually tracked on Roles, as opposed to permissions which exist at
-- the application level, which may be mapped onto these.
--
-- E.g. The app may check the Contribution Merge permission by checking if the user has the
-- ProjectContribute permission.
data RolePermission
  = -- Project
    ProjectView
  | ProjectManage
  | ProjectContribute
  | ProjectMaintain
  | ProjectDelete
  | -- Org
    OrgView
  | OrgManage
  | OrgAdmin
  | OrgDelete
  | OrgChangeOwner
  | OrgProjectCreate
  | -- Team
    TeamView
  | TeamManage
  deriving (Show, Eq, Ord)

rolePermissionToText :: RolePermission -> Text
rolePermissionToText = \case
  ProjectView -> "project:view"
  ProjectManage -> "project:manage"
  ProjectContribute -> "project:contribute"
  ProjectMaintain -> "project:maintain"
  ProjectDelete -> "project:delete"
  OrgView -> "org:view"
  OrgManage -> "org:manage"
  OrgAdmin -> "org:admin"
  OrgDelete -> "org:delete"
  OrgChangeOwner -> "org:change_owner"
  OrgProjectCreate -> "org:project_create"
  TeamView -> "team:view"
  TeamManage -> "team:manage"

rolePermissionFromText :: Text -> Maybe RolePermission
rolePermissionFromText = \case
  "project:view" -> Just ProjectView
  "project:manage" -> Just ProjectManage
  "project:contribute" -> Just ProjectContribute
  "project:delete" -> Just ProjectDelete
  "project:maintain" -> Just ProjectMaintain
  "org:view" -> Just OrgView
  "org:manage" -> Just OrgManage
  "org:admin" -> Just OrgAdmin
  "org:delete" -> Just OrgDelete
  "org:change_owner" -> Just OrgChangeOwner
  "org:project_create" -> Just OrgProjectCreate
  "team:view" -> Just TeamView
  "team:manage" -> Just TeamManage
  _ -> Nothing

instance Hasql.EncodeValue RolePermission where
  encodeValue =
    Hasql.encodeValue @Text
      & contramap rolePermissionToText

instance Hasql.DecodeValue RolePermission where
  decodeValue =
    Hasql.decodeValue @Text
      & HasqlDecoders.refine \a -> case rolePermissionFromText a of
        Just x -> Right x
        Nothing -> Left $ "Invalid RolePermission: " <> a

instance ToJSON RolePermission where
  toJSON = Aeson.String . rolePermissionToText

instance FromJSON RolePermission where
  parseJSON = Aeson.withText "RolePermission" \a -> case rolePermissionFromText a of
    Just x -> pure x
    Nothing -> fail $ "Invalid RolePermission: " <> Text.unpack a

-- | There are a set of builtin Unison roles.
data RoleRef
  = RoleOrgViewer
  | RoleOrgContributor
  | RoleOrgMaintainer
  | RoleOrgAdmin
  | RoleOrgOwner
  | RoleOrgDefault
  | RoleTeamAdmin
  | RoleProjectViewer
  | RoleProjectContributor
  | RoleProjectMaintainer
  | RoleProjectAdmin
  | RoleProjectOwner
  | RoleProjectPublicAccess
  deriving (Show, Eq, Ord)

projectRoles :: Set RoleRef
projectRoles = Set.fromList [RoleProjectViewer, RoleProjectContributor, RoleProjectOwner, RoleProjectAdmin, RoleProjectMaintainer]

instance ToJSON RoleRef where
  toJSON = \case
    RoleOrgViewer -> Aeson.String "org_viewer"
    RoleOrgContributor -> Aeson.String "org_contributor"
    RoleOrgMaintainer -> Aeson.String "org_maintainer"
    RoleOrgAdmin -> Aeson.String "org_admin"
    RoleOrgOwner -> Aeson.String "org_owner"
    RoleOrgDefault -> Aeson.String "org_default"
    RoleTeamAdmin -> Aeson.String "team_admin"
    RoleProjectViewer -> Aeson.String "project_viewer"
    RoleProjectContributor -> Aeson.String "project_contributor"
    RoleProjectMaintainer -> "project_maintainer"
    RoleProjectAdmin -> Aeson.String "project_admin"
    RoleProjectOwner -> Aeson.String "project_owner"
    RoleProjectPublicAccess -> Aeson.String "project_public_access"

instance FromJSON RoleRef where
  parseJSON = Aeson.withText "RoleRef" \case
    "org_viewer" -> pure RoleOrgViewer
    "org_contributor" -> pure RoleOrgContributor
    "org_maintainer" -> pure RoleOrgMaintainer
    "org_admin" -> pure RoleOrgAdmin
    "org_owner" -> pure RoleOrgOwner
    "org_default" -> pure RoleOrgDefault
    "team_admin" -> pure RoleTeamAdmin
    "project_viewer" -> pure RoleProjectViewer
    "project_contributor" -> pure RoleProjectContributor
    "project_maintainer" -> pure RoleProjectMaintainer
    "project_admin" -> pure RoleProjectAdmin
    "project_owner" -> pure RoleProjectOwner
    "project_public_access" -> pure RoleProjectPublicAccess
    _ -> fail "Invalid RoleRef"

instance Hasql.DecodeValue RoleRef where
  decodeValue =
    HasqlDecode.enum $
      \case
        "org_viewer" -> Just RoleOrgViewer
        "org_contributor" -> Just RoleOrgContributor
        "org_maintainer" -> Just RoleOrgMaintainer
        "org_admin" -> Just RoleOrgAdmin
        "org_owner" -> Just RoleOrgOwner
        "org_default" -> Just RoleOrgDefault
        "team_admin" -> Just RoleTeamAdmin
        "project_viewer" -> Just RoleProjectViewer
        "project_contributor" -> Just RoleProjectContributor
        "project_maintainer" -> Just RoleProjectMaintainer
        "project_admin" -> Just RoleProjectAdmin
        "project_owner" -> Just RoleProjectOwner
        "project_public_access" -> Just RoleProjectPublicAccess
        _ -> Nothing

instance Hasql.EncodeValue RoleRef where
  encodeValue =
    HasqlEncode.enum $
      \case
        RoleOrgViewer -> "org_viewer"
        RoleOrgContributor -> "org_contributor"
        RoleOrgMaintainer -> "org_maintainer"
        RoleOrgAdmin -> "org_admin"
        RoleOrgOwner -> "org_owner"
        RoleOrgDefault -> "org_default"
        RoleTeamAdmin -> "team_admin"
        RoleProjectViewer -> "project_viewer"
        RoleProjectContributor -> "project_contributor"
        RoleProjectMaintainer -> "project_maintainer"
        RoleProjectAdmin -> "project_admin"
        RoleProjectOwner -> "project_owner"
        RoleProjectPublicAccess -> "project_public_access"

data RoleAssignment subject = RoleAssignment
  { subject :: subject,
    roles :: Set RoleRef
  }
  deriving (Show, Functor, Foldable, Traversable)

instance (ToJSON user) => ToJSON (RoleAssignment user) where
  toJSON RoleAssignment {..} =
    object
      [ "subject" Aeson..= subject,
        "roles" Aeson..= roles
      ]

instance (FromJSON user) => FromJSON (RoleAssignment user) where
  parseJSON = Aeson.withObject "RoleAssignment" $ \o -> do
    subject <- o Aeson..: "subject"
    roles <- o Aeson..: "roles"
    pure RoleAssignment {..}

-- | A type for mixing in permissions info on a response for a resource.
newtype PermissionsInfo = PermissionsInfo (Set RolePermission)
  deriving (Show)
  deriving (ToJSON) via (AtKey "permissions" (Set RolePermission))

data ProjectMaintainerPermissions = ProjectMaintainerPermissions
  { canView :: Bool,
    canMaintain :: Bool,
    canAdmin :: Bool
  }
  deriving (Show)

instance ToJSON ProjectMaintainerPermissions where
  toJSON ProjectMaintainerPermissions {canView, canMaintain, canAdmin} =
    object
      [ "canView" .= canView,
        "canMaintain" .= canMaintain,
        "canAdmin" .= canAdmin
      ]

instance FromJSON ProjectMaintainerPermissions where
  parseJSON = withObject "ProjectMaintainerPermissions" $ \o -> do
    canView <- o Aeson..: "canView"
    canMaintain <- o Aeson..: "canMaintain"
    canAdmin <- o Aeson..: "canAdmin"
    pure ProjectMaintainerPermissions {canView, canMaintain, canAdmin}

-- Generic request/response types
data ListRolesResponse = ListRolesResponse
  { -- Whether the caller is able to edit the roles.
    active :: Bool,
    roleAssignments :: [RoleAssignment DisplayAuthSubject]
  }
  deriving (Show)

instance ToJSON ListRolesResponse where
  toJSON ListRolesResponse {..} =
    object
      [ "role_assignments" Aeson..= roleAssignments,
        "active" .= active
      ]

data AddRolesResponse = AddRolesResponse
  { roleAssignments :: [RoleAssignment DisplayAuthSubject]
  }

instance ToJSON AddRolesResponse where
  toJSON AddRolesResponse {..} =
    object
      [ "role_assignments" Aeson..= roleAssignments
      ]

data RemoveRolesResponse = RemoveRolesResponse
  { roleAssignments :: [RoleAssignment DisplayAuthSubject]
  }

instance ToJSON RemoveRolesResponse where
  toJSON RemoveRolesResponse {..} =
    object
      [ "role_assignments" Aeson..= roleAssignments
      ]

data AddRolesRequest = AddRolesRequest
  { roleAssignments :: [RoleAssignment ResolvedAuthSubject]
  }
  deriving (Show)

instance FromJSON AddRolesRequest where
  parseJSON = Aeson.withObject "AddRolesRequest" $ \o -> do
    roleAssignments <- o Aeson..: "role_assignments"
    pure AddRolesRequest {..}

data RemoveRolesRequest = RemoveRolesRequest
  { roleAssignments :: [RoleAssignment ResolvedAuthSubject]
  }
  deriving (Show)

instance FromJSON RemoveRolesRequest where
  parseJSON = Aeson.withObject "RemoveRolesRequest" $ \o -> do
    roleAssignments <- o Aeson..: "role_assignments"
    pure RemoveRolesRequest {..}
