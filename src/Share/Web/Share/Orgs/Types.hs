{-# LANGUAGE RecordWildCards #-}

module Share.Web.Share.Orgs.Types
  ( Org (..),
    CreateOrgRequest (..),
    OrgMembersAddRequest (..),
    OrgMembersListResponse (..),
    OrgMembersRemoveRequest (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Share.IDs
import Share.Postgres (DecodeRow (..), decodeField)
import Share.Utils.URI (URIParam)
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)

data Org = Org {orgId :: OrgId, isCommercial :: Bool}
  deriving (Show, Eq, Ord)

instance DecodeRow Org where
  decodeRow = do
    orgId <- decodeField
    isCommercial <- decodeField
    pure Org {..}

data CreateOrgRequest = CreateOrgRequest
  { name :: Text,
    handle :: OrgHandle,
    avatarUrl :: Maybe URIParam,
    owner :: UserHandle,
    email :: Maybe Email,
    isCommercial :: Bool
  }
  deriving (Show, Eq)

instance ToJSON CreateOrgRequest where
  toJSON CreateOrgRequest {..} =
    object
      [ "name" .= name,
        "handle" .= handle,
        "avatarUrl" .= avatarUrl,
        "owner" .= owner,
        "email" .= email,
        "isCommercial" .= isCommercial
      ]

instance FromJSON CreateOrgRequest where
  parseJSON = withObject "CreateOrgRequest" $ \o -> do
    name <- o .: "name"
    handle <- o .: "handle"
    avatarUrl <- o .:? "avatarUrl"
    owner <- o .: "owner"
    email <- o .:? "email"
    isCommercial <- o .: "isCommercial"
    pure CreateOrgRequest {..}

data OrgMembersAddRequest = OrgMembersAddRequest
  { members :: [RoleAssignment UserHandle]
  }
  deriving (Show, Eq)

instance ToJSON OrgMembersAddRequest where
  toJSON OrgMembersAddRequest {..} =
    object
      [ "members" .= members
      ]

instance FromJSON OrgMembersAddRequest where
  parseJSON = withObject "OrgMembersAddRequest" $ \o -> do
    members <- o .: "members"
    pure OrgMembersAddRequest {..}

data OrgMembersListResponse = OrgMembersListResponse
  { members :: [RoleAssignment UserDisplayInfo]
  }
  deriving (Show, Eq)

instance ToJSON OrgMembersListResponse where
  toJSON OrgMembersListResponse {..} =
    object
      [ "members" .= members
      ]

instance FromJSON OrgMembersListResponse where
  parseJSON = withObject "OrgMembersListResponse" $ \o -> do
    members <- o .: "members"
    pure OrgMembersListResponse {..}

data OrgMembersRemoveRequest = OrgMembersRemoveRequest
  { members :: [UserHandle]
  }
  deriving (Show, Eq)

instance ToJSON OrgMembersRemoveRequest where
  toJSON OrgMembersRemoveRequest {..} =
    object
      [ "members" .= members
      ]

instance FromJSON OrgMembersRemoveRequest where
  parseJSON = withObject "OrgMembersRemoveRequest" $ \o -> do
    members <- o .: "members"
    pure OrgMembersRemoveRequest {..}
