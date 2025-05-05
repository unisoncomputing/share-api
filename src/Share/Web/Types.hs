{-# LANGUAGE DuplicateRecordFields #-}

module Share.Web.Types (DiscoveryDocument (..), UserInfo (..)) where

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Share.IDs
import Share.OAuth.Types (ResponseType)
import Share.Prelude
import Share.Utils.URI

-- | https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfigurationResponse
-- This is a very small supported subset of:
--   https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata
data DiscoveryDocument = DiscoveryDocument
  { issuer :: URIParam,
    authorizationE :: URIParam,
    tokenE :: URIParam,
    userInfoE :: URIParam,
    jwksURI :: URIParam,
    responseTypesSupported :: Set ResponseType
  }

instance ToJSON DiscoveryDocument where
  toJSON (DiscoveryDocument issuer authE tokenE userInfoE jwksURI responseTypesSupported) =
    Aeson.object
      [ "issuer" .= issuer,
        "authorization_endpoint" .= authE,
        "token_endpoint" .= tokenE,
        "userinfo_endpoint" .= userInfoE,
        "jwks_uri" .= jwksURI,
        "response_types_supported" .= responseTypesSupported
      ]

-- | This response is compliant with the claims specified here: https://openid.net/specs/openid-connect-core-1_0.html#StandardClaims
--
-- It's permitted to add additional claims, but any standard claims must be formatted
-- according to the spec.
data UserInfo = UserInfo
  { sub :: UserId,
    name :: Maybe Text,
    picture :: Maybe URIParam,
    profile :: URIParam, -- Link to the user's profile page
    email :: Maybe Email,
    -- Additional claims
    handle :: UserHandle
  }
  deriving (Show, Eq)

instance ToJSON UserInfo where
  toJSON UserInfo {sub, name, picture, profile, email, handle} =
    Aeson.object
      [ "sub" .= sub,
        "name" .= name,
        "picture" .= picture,
        "profile" .= profile,
        "email" .= email,
        "handle" .= handle
      ]
