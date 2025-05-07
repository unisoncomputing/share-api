-- Standard ways of displaying core Share concepts.
--
-- This was consolidated to this module mostly to avoid circular imports.
module Share.Web.Share.DisplayInfo.Types
  ( UserDisplayInfo (..),
    OrgDisplayInfo (..),
    TeamDisplayInfo (..),
    UserLike (..),
    UnifiedDisplayInfo,
    UserLikeIds,
    unifiedUser_,
    unifiedOrg_,
    unifiedTeam_,
  )
where

import Control.Lens
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON)
import Network.URI (URI)
import Share.IDs
import Share.Prelude
import Share.Utils.URI (URIParam (..))

-- | A single unified type for anywhere the frontend may need to display a user-like
-- thing; whether org, team, or user.
data UserLike user team org
  = UnifiedUser user
  | UnifiedOrg org
  | UnifiedTeam team
  deriving (Show, Eq, Ord)

instance (ToJSON user, ToJSON team, ToJSON org) => ToJSON (UserLike user team org) where
  toJSON = \case
    UnifiedUser u -> Aeson.object ["kind" Aeson..= ("user" :: Text), "info" Aeson..= u]
    UnifiedOrg o -> Aeson.object ["kind" Aeson..= ("org" :: Text), "info" Aeson..= o]
    UnifiedTeam t -> Aeson.object ["kind" Aeson..= ("team" :: Text), "info" Aeson..= t]

instance (FromJSON user, FromJSON team, FromJSON org) => FromJSON (UserLike user team org) where
  parseJSON =
    Aeson.withObject "UserLike" $ \o -> do
      kind <- o Aeson..: "kind"
      case kind of
        ("user" :: Text) -> UnifiedUser <$> o Aeson..: "info"
        ("org" :: Text) -> UnifiedOrg <$> o Aeson..: "info"
        ("team" :: Text) -> UnifiedTeam <$> o Aeson..: "info"
        _ -> fail $ "Unknown UserLike kind: " <> show kind

type UnifiedDisplayInfo = UserLike UserDisplayInfo TeamDisplayInfo OrgDisplayInfo

type UserLikeIds = UserLike UserId TeamId OrgId

unifiedUser_ :: Traversal (UserLike user team org) (UserLike user' team org) user user'
unifiedUser_ f = \case
  (UnifiedUser u) -> UnifiedUser <$> f u
  (UnifiedOrg o) -> pure $ UnifiedOrg o
  (UnifiedTeam t) -> pure $ UnifiedTeam t

unifiedOrg_ :: Traversal (UserLike user team org) (UserLike user team org') org org'
unifiedOrg_ f = \case
  (UnifiedUser u) -> pure $ UnifiedUser u
  (UnifiedOrg o) -> UnifiedOrg <$> f o
  (UnifiedTeam t) -> pure $ UnifiedTeam t

unifiedTeam_ :: Traversal (UserLike user team org) (UserLike user team' org) team team'
unifiedTeam_ f = \case
  (UnifiedUser u) -> pure $ UnifiedUser u
  (UnifiedOrg o) -> pure $ UnifiedOrg o
  (UnifiedTeam t) -> UnifiedTeam <$> f t

-- | Common type for displaying a user.
data UserDisplayInfo = UserDisplayInfo
  { handle :: UserHandle,
    name :: Maybe Text,
    avatarUrl :: Maybe URI,
    userId :: UserId
  }
  deriving (Show, Eq, Ord)

instance ToJSON UserDisplayInfo where
  toJSON UserDisplayInfo {handle, name, avatarUrl, userId} =
    Aeson.object
      [ "handle" Aeson..= handle,
        "name" Aeson..= name,
        "avatarUrl" Aeson..= (URIParam <$> avatarUrl),
        "userId" Aeson..= userId
      ]

instance FromJSON UserDisplayInfo where
  parseJSON =
    Aeson.withObject "UserDisplayInfo" $ \o -> do
      handle <- o Aeson..: "handle"
      name <- o Aeson..:? "name"
      avatarUrl <- fmap unpackURI <$> o Aeson..:? "avatarUrl"
      userId <- o Aeson..: "userId"
      pure UserDisplayInfo {handle, name, avatarUrl, userId}

-- | Common type for displaying an Org.
data OrgDisplayInfo = OrgDisplayInfo
  { user :: UserDisplayInfo,
    orgId :: OrgId,
    isCommercial :: Bool
  }
  deriving (Show, Eq, Ord)

instance ToJSON OrgDisplayInfo where
  toJSON OrgDisplayInfo {user, orgId, isCommercial} =
    Aeson.object
      [ "user" Aeson..= user,
        "orgId" Aeson..= orgId,
        "isCommercial" Aeson..= isCommercial
      ]

data TeamDisplayInfo = TeamDisplayInfo
  { teamId :: TeamId,
    name :: Text,
    avatarUrl :: Maybe URI
  }
  deriving (Show, Eq, Ord)

instance ToJSON TeamDisplayInfo where
  toJSON TeamDisplayInfo {teamId, name, avatarUrl} =
    Aeson.object
      [ "teamId" Aeson..= teamId,
        "name" Aeson..= name,
        "avatarUrl" Aeson..= (URIParam <$> avatarUrl)
      ]
