module Share.Web.Authorization.Types
  ( RolePermission (..),
    ProjectMaintainerPermissions (..),
  )
where

import Data.Aeson (FromJSON, ToJSON (..), object, withObject, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..))
import Hasql.Interpolate qualified as Hasql
import Share.Prelude

-- Permissions which are actually tracked on Roles, as opposed to permissions which exist at
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
