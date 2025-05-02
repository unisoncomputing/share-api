-- Standard ways of displaying core Share concepts.
--
-- This was consolidated to this module mostly to avoid circular imports.
module Share.Web.Share.DisplayInfo
  ( UserDisplayInfo (..),
    OrgDisplayInfo (..),
    TeamDisplayInfo (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Network.URI (URI)
import Share.IDs
import Share.Prelude
import Share.Utils.URI (URIParam (..))

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
