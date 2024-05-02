{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enlil.OAuth.IdentityProvider.Types
  ( IdentityProviderConfig (..),
  )
where

import Enlil.OAuth.Scopes
import Enlil.OAuth.Types
import Servant
import Servant.Client

-- | Methods needed to auth with an identity provider.
-- See 'Enlil.OAuth.IdentityProvider.Share' for a preset.
data IdentityProviderConfig = IdentityProviderConfig
  { exchangeCodeForToken :: OAuthClientId -> OAuthClientSecret -> Code -> URI -> PKCEVerifier -> IO (Either ClientError TokenResponse),
    authorizationURI :: URI -> OAuthClientId -> PendingSessionId -> PKCEChallenge -> PKCEChallengeMethod -> Scopes -> URI
  }
