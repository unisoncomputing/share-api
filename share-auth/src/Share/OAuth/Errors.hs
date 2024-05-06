{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Share.OAuth.Errors
  ( OAuth2ErrorCode (..),
    OAuth2Error (..),
    OAuth2ErrorRedirect (..),
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import Share.OAuth.Types (AuthenticationRequest (..), Code, OAuthClientId)
import Share.Utils.Show (tShow)
import Share.Utils.URI (URIParam)
import Servant

-- | Represents an oauth2 error, and the information which should be included when redirecting
-- to the client with the error.
data OAuth2ErrorRedirect = OAuth2ErrorRedirect
  { errCode :: OAuth2ErrorCode,
    errDescription :: Text,
    -- The state sent with the original authorization request if available.
    state :: Maybe Text,
    -- Where to send the error message, if and only if the redirectURI validated correctly.
    redirectURI :: Maybe URIParam
  }
  deriving (Show)

-- | Valid error codes according to the oauth2 spec.
-- https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.2.1
data OAuth2ErrorCode
  = InvalidRequest
  | UnauthorizedClient
  | AccessDenied
  | UnsupportedResponseType
  | InvalidScope
  | OAuthServerError
  | TemporarilyUnavailable

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.2.1
instance Show OAuth2ErrorCode where
  show = \case
    InvalidRequest -> "invalid_request"
    UnauthorizedClient -> "unauthorized_client"
    AccessDenied -> "access_denied"
    UnsupportedResponseType -> "unsupported_response_type"
    InvalidScope -> "invalid_scope"
    OAuthServerError -> "server_error"
    TemporarilyUnavailable -> "temporarily_unavailable"

instance ToHttpApiData OAuth2ErrorCode where
  toQueryParam = tShow

data OAuth2Error
  = CodeMissingOrExpired Code
  | MismatchedClientId
      OAuthClientId
      -- ^ from client
      OAuthClientId
      -- ^ from auth request
  | MismatchedRedirectURI
      URIParam
      -- ^ from client
      URIParam
      -- ^ from auth request
  | UnregisteredRedirectURI OAuthClientId URIParam
  | MismatchedClientSecret OAuthClientId
  | UnknownClient OAuthClientId
  | OpenIDScopeRequired AuthenticationRequest
  | PKCEChallengeFailure
  deriving stock (Show)
  deriving anyclass (Exception)
