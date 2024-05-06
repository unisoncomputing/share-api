{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Share.OAuth.IdentityProvider.Types
  ( IdentityProviderConfig (..),
  )
where

import Share.OAuth.Scopes
import Share.OAuth.Types
import Servant
import Servant.Client

-- | Methods needed to auth with an identity provider.
-- See 'Share.OAuth.IdentityProvider.Share' for a preset.
data IdentityProviderConfig = IdentityProviderConfig
  { exchangeCodeForToken :: OAuthClientId -> OAuthClientSecret -> Code -> URI -> PKCEVerifier -> IO (Either ClientError TokenResponse),
    authorizationURI :: URI -> OAuthClientId -> PendingSessionId -> PKCEChallenge -> PKCEChallengeMethod -> Scopes -> URI
  }
