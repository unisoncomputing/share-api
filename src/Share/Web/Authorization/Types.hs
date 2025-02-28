module Share.Web.Authorization.Types
  ( Permission(..),
    ProjectMaintainerPermission (..),
    ProjectMaintainerPermissions (..),
    hasProjectPermissions,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), object, withObject, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON (..))
import Data.Set (Set)

data Permission =
  -- Project
  ProjectView
    | ProjectManage
    | ProjectContribute
  -- Org
    | OrgView
    | OrgManage
    | OrgAdmin
  -- Team
    | TeamView
    | TeamManage
  deriving (Show)

instance Hasql.EncodeValue Permission where
  encodeValue = Hasql.encodeValue <<< \case
    ProjectView -> "project:view"
    ProjectManage -> "project:manage"
    ProjectContribute -> "project:contribute"
    OrgView -> "org:view"
    OrgManage -> "org:manage"
    OrgAdmin -> "org:admin"
    TeamView -> "team:view"
    TeamManage -> "team:manage"

data ProjectMaintainerPermission
  = -- Can see the project and its contents even if it's private.
    -- Can download any branches/releases
    -- Can comment on contributions & tickets
    ProjectView
  | -- Can create/delete/merge branches, tickets, contributions, etc.
    -- Can't change project settings or add new maintainers.
    ProjectMaintain
  | -- Can do anything the project owner can do.
    ProjectAdmin
  deriving (Show, Eq, Ord)

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

-- | >>> import Data.Set qualified as Set
-- >>> hasProjectPermissions (Set.fromList [ProjectView, ProjectMaintain]) (ProjectMaintainerPermissions {canView=True, canMaintain=True, canAdmin=False})
-- True
-- >>> hasProjectPermissions (Set.fromList [ProjectView, ProjectAdmin]) (ProjectMaintainerPermissions {canView=True, canMaintain=True, canAdmin=False})
-- False
hasProjectPermissions :: Set ProjectMaintainerPermission -> ProjectMaintainerPermissions -> Bool
hasProjectPermissions permissions ProjectMaintainerPermissions {canView, canMaintain, canAdmin} =
  all
    ( \case
        ProjectView -> canView
        ProjectMaintain -> canMaintain
        ProjectAdmin -> canAdmin
    )
    permissions
