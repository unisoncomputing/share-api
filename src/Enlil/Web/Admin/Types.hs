{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Enlil.Web.Admin.Types where

import Data.Aeson
import Data.Time (Day)
import Enlil.IDs

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
