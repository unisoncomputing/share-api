{-# LANGUAGE TemplateHaskell #-}

module Share.Web.Authorization.Types
  ( RolePermission (..),
    ProjectMaintainerPermissions (..),
    AuthSubject (..),
    _UserSubject,
    _OrgSubject,
    _TeamSubject,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON, ToJSON (..), object, withObject, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..))
import Hasql.Interpolate qualified as Hasql
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

data SubjectKind = UserSubjectKind | OrgSubjectKind | TeamSubjectKind
  deriving (Show)

instance Hasql.EncodeValue SubjectKind where
  encodeValue =
    Hasql.encodeValue @Text
      & contramap \case
        UserSubjectKind -> "user"
        OrgSubjectKind -> "org"
        TeamSubjectKind -> "team"

instance Hasql.DecodeValue SubjectKind where
  decodeValue =
    Hasql.decodeValue @Text
      & fmap \case
        "user" -> UserSubjectKind
        "org" -> OrgSubjectKind
        "team" -> TeamSubjectKind
        _ -> error "Invalid SubjectKind"

data AuthSubject user org team
  = UserSubject user
  | OrgSubject org
  | TeamSubject team
  deriving (Show)

-- Decoder for (subject.id, subject.kind)
instance Hasql.DecodeRow (AuthSubject SubjectId SubjectId SubjectId) where
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
