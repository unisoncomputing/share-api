{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Share.User where

import Data.Aeson
import Hasql.Interpolate qualified as Hasql
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.URI

data UserVisibility = UserPrivate | UserPublic
  deriving (Show, Eq, Ord)

instance Hasql.DecodeValue UserVisibility where
  decodeValue =
    PG.decodeValue
      <&> \case
        True -> UserPrivate
        False -> UserPublic

instance Hasql.EncodeValue UserVisibility where
  encodeValue =
    PG.encodeValue
      & contramap \case
        UserPrivate -> True
        UserPublic -> False

data User = User
  { user_id :: UserId,
    user_name :: Maybe Text,
    user_email :: Text,
    avatar_url :: Maybe URIParam,
    handle :: UserHandle,
    visibility :: UserVisibility
  }
  deriving (Show, Eq, Ord, Generic)

instance PG.DecodeRow User where
  decodeRow = do
    user_id <- PG.decodeField
    user_name <- PG.decodeField
    user_email <- PG.decodeField
    avatar_url <- PG.decodeField
    handle <- PG.decodeField
    visibility <- PG.decodeField
    pure $ User {..}

type RoleId = Int

data Role = Role
  { role_id :: Int,
    role_name :: Text,
    role_description :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type GroupId = Int

data Group = Group
  { group_id :: Int,
    group_name :: Text,
    group_description :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type PermissionId = Int

data Permission = Permission
  { permission_id :: PermissionId,
    permission_name :: Text,
    permission_description :: Maybe Text
  }
