{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Admin.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (Day)
import Share.IDs
import Share.Utils.URI
import Share.Web.Share.DisplayInfo.Types (OrgDisplayInfo)

data ProjectCategory = ProjectCategory
  { categoryName :: CategoryName,
    userHandle :: UserHandle,
    projectSlug :: ProjectSlug
  }
  deriving (Show)

instance FromJSON ProjectCategory where
  parseJSON = withObject "ProjectCategory" \o -> do
    categoryName <- o .: "categoryName"
    userHandle <- o .: "userHandle"
    projectSlug <- o .: "projectSlug"
    pure ProjectCategory {..}

data AddCloudUserRequest = AddCloudUserRequest
  { handle :: UserHandle
  }

instance FromJSON AddCloudUserRequest where
  parseJSON = withObject "AddCloudUserRequest" $ \o ->
    AddCloudUserRequest <$> o .: "handle"

data RemoveCloudUserRequest = RemoveCloudUserRequest
  { handle :: UserHandle
  }

instance FromJSON RemoveCloudUserRequest where
  parseJSON = withObject "RemoveCloudUserRequest" $ \o ->
    RemoveCloudUserRequest <$> o .: "handle"

data DeleteUserRequest = DeleteUserRequest
  { -- Used to verify that the caller is intentionally running this request,
    -- and that it's not an accidental click or something.
    currentDate :: Day
  }

instance FromJSON DeleteUserRequest where
  parseJSON = withObject "DeleteUserRequest" $ \o ->
    DeleteUserRequest <$> o .: "currentDate"

data AdminCreateOrgRequest = AdminCreateOrgRequest
  { orgName :: Text,
    orgHandle :: OrgHandle,
    orgEmail :: Maybe Email,
    orgAvatarUrl :: Maybe URIParam,
    orgOwner :: UserHandle,
    isCommercial :: Bool
  }
  deriving (Show)

instance FromJSON AdminCreateOrgRequest where
  parseJSON = withObject "AdminCreateOrgRequest" $ \o -> do
    orgName <- o .: "name"
    orgHandle <- o .: "handle"
    orgEmail <- o .:? "email"
    orgAvatarUrl <- o .:? "avatarUrl"
    orgOwner <- o .: "owner"
    isCommercial <- o .:? "isCommercial" .!= False
    pure AdminCreateOrgRequest {..}

instance ToJSON AdminCreateOrgRequest where
  toJSON AdminCreateOrgRequest {..} =
    object
      [ "name" .= orgName,
        "handle" .= orgHandle,
        "email" .= orgEmail,
        "avatarUrl" .= orgAvatarUrl,
        "owner" .= orgOwner,
        "isCommercial" .= isCommercial
      ]

data AdminCreateOrgResponse = AdminCreateOrgResponse
  { org :: OrgDisplayInfo
  }
  deriving (Show)

instance FromJSON AdminCreateOrgResponse where
  parseJSON = withObject "AdminCreateOrgResponse" $ \o -> do
    org <- o .: "org"
    pure AdminCreateOrgResponse {..}

instance ToJSON AdminCreateOrgResponse where
  toJSON AdminCreateOrgResponse {..} =
    object
      [ "org" .= org
      ]
