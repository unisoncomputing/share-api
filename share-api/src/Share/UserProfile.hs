{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Share.UserProfile where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.URI
import Hasql.Interpolate qualified as Hasql

data UserProfile = UserProfile
  { user_id :: UserId,
    user_name :: Maybe Text,
    avatar_url :: Maybe URIParam,
    handle :: UserHandle,
    bio :: Maybe Text,
    website :: Maybe Text,
    location :: Maybe Text,
    twitterHandle :: Maybe Text,
    pronouns :: Maybe Text
  }
  deriving (Show, Generic)

instance Hasql.DecodeRow UserProfile where
  decodeRow = do
    user_id <- PG.decodeField
    user_name <- PG.decodeField
    avatar_url <- PG.decodeField
    handle <- PG.decodeField
    bio <- PG.decodeField
    website <- PG.decodeField
    location <- PG.decodeField
    twitterHandle <- PG.decodeField
    pronouns <- PG.decodeField
    pure $ UserProfile {..}
