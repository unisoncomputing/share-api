{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Share.Web.Authorization.Types
  ( RolePermission (..),
    RoleRef (..),
    projectRoles,
    ProjectMaintainerPermissions (..),
    AuthSubject (..),
    SubjectKind (..),
    GenericAuthSubject,
    ResolvedAuthSubject,
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
import Data.UUID (UUID)
import Hasql.Decoders qualified as HasqlDecode
import Hasql.Encoders qualified as HasqlEncode
import Hasql.Interpolate qualified as Hasql
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

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

type GenericAuthSubject = AuthSubject SubjectId SubjectId SubjectId

type ResolvedAuthSubject = AuthSubject UserId OrgId TeamId

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

makePrisms ''AuthSubject

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
  | -- Org
    OrgView
  | OrgManage
  | OrgAdmin
  | OrgProjectCreate
  | -- Team
    TeamView
  | TeamManage
  deriving (Show)

instance Hasql.EncodeValue RolePermission where
  encodeValue =
    Hasql.encodeValue @Text
      & contramap \case
        ProjectView -> "project:view"
        ProjectManage -> "project:manage"
        ProjectContribute -> "project:contribute"
        OrgView -> "org:view"
        OrgManage -> "org:manage"
        OrgAdmin -> "org:admin"
        OrgProjectCreate -> "org:project_create"
        TeamView -> "team:view"
        TeamManage -> "team:manage"

-- | There are a set of builtin Unison roles.
data RoleRef
  = RoleOrgViewer
  | RoleOrgContributor
  | RoleOrgAdmin
  | RoleOrgDefault
  | RoleTeamAdmin
  | RoleProjectViewer
  | RoleProjectContributor
  | RoleProjectOwner
  deriving (Show, Eq, Ord)

projectRoles :: Set RoleRef
projectRoles = Set.fromList [RoleProjectViewer, RoleProjectContributor, RoleProjectOwner]

instance ToJSON RoleRef where
  toJSON = \case
    RoleOrgViewer -> Aeson.String "org_viewer"
    RoleOrgContributor -> Aeson.String "org_contributor"
    RoleOrgAdmin -> Aeson.String "org_admin"
    RoleOrgDefault -> Aeson.String "org_default"
    RoleTeamAdmin -> Aeson.String "team_admin"
    RoleProjectViewer -> Aeson.String "project_viewer"
    RoleProjectContributor -> Aeson.String "project_contributor"
    RoleProjectOwner -> Aeson.String "project_owner"

instance FromJSON RoleRef where
  parseJSON = Aeson.withText "RoleRef" \case
    "org_viewer" -> pure RoleOrgViewer
    "org_contributor" -> pure RoleOrgContributor
    "org_admin" -> pure RoleOrgAdmin
    "org_default" -> pure RoleOrgDefault
    "team_admin" -> pure RoleTeamAdmin
    "project_viewer" -> pure RoleProjectViewer
    "project_contributor" -> pure RoleProjectContributor
    "project_owner" -> pure RoleProjectOwner
    _ -> fail "Invalid RoleRef"

instance Hasql.DecodeValue RoleRef where
  decodeValue =
    HasqlDecode.enum $
      \case
        "org_viewer" -> Just RoleOrgViewer
        "org_contributor" -> Just RoleOrgContributor
        "org_admin" -> Just RoleOrgAdmin
        "org_default" -> Just RoleOrgDefault
        "team_admin" -> Just RoleTeamAdmin
        "project_viewer" -> Just RoleProjectViewer
        "project_contributor" -> Just RoleProjectContributor
        "project_owner" -> Just RoleProjectOwner
        _ -> Nothing

instance Hasql.EncodeValue RoleRef where
  encodeValue =
    HasqlEncode.enum $
      \case
        RoleOrgViewer -> "org_viewer"
        RoleOrgContributor -> "org_contributor"
        RoleOrgAdmin -> "org_admin"
        RoleOrgDefault -> "org_default"
        RoleTeamAdmin -> "team_admin"
        RoleProjectViewer -> "project_viewer"
        RoleProjectContributor -> "project_contributor"
        RoleProjectOwner -> "project_owner"

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
    roles :: [RoleAssignment ResolvedAuthSubject]
  }
  deriving (Show)

instance ToJSON ListRolesResponse where
  toJSON ListRolesResponse {..} =
    object
      [ "roles" Aeson..= roles,
        "active" .= active
      ]

data AddRolesResponse = AddRolesResponse
  { roles :: [RoleAssignment ResolvedAuthSubject]
  }

instance ToJSON AddRolesResponse where
  toJSON AddRolesResponse {..} =
    object
      [ "roles" Aeson..= roles
      ]

data RemoveRolesResponse = RemoveRolesResponse
  { roles :: [RoleAssignment ResolvedAuthSubject]
  }

instance ToJSON RemoveRolesResponse where
  toJSON RemoveRolesResponse {..} =
    object
      [ "roles" Aeson..= roles
      ]

data AddRolesRequest = AddRolesRequest
  { roles :: [RoleAssignment ResolvedAuthSubject]
  }
  deriving (Show)

instance FromJSON AddRolesRequest where
  parseJSON = Aeson.withObject "AddRolesRequest" $ \o -> do
    roles <- o Aeson..: "roles"
    pure AddRolesRequest {..}

data RemoveRolesRequest = RemoveRolesRequest
  { roles :: [RoleAssignment ResolvedAuthSubject]
  }
  deriving (Show)

instance FromJSON RemoveRolesRequest where
  parseJSON = Aeson.withObject "RemoveRolesRequest" $ \o -> do
    roles <- o Aeson..: "roles"
    pure RemoveRolesRequest {..}
