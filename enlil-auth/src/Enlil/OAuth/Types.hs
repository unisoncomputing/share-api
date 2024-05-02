{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Enlil.OAuth.Types
  ( OAuth2State,
    ResponseType (..),
    GrantType (..),
    AccessToken (..),
    IDToken (..),
    RefreshToken (..),
    TokenType (..),
    Code (..),
    OAuthClientId (..),
    OAuthClientSecret (..),
    OAuthClientConfig (..),
    AuthenticationRequest (..),
    TokenRequest (..),
    TokenResponse (..),
    RedirectReceiverErr (..),
    PKCEChallenge (..),
    PKCEChallengeMethod (..),
    PKCEVerifier (..),
    PendingSessionId (..),
    pendingSessionCookieKey,
    PendingSessionCookieKey,
    SessionId (..),
    UserId (..),
    JTI (..),
  )
where

import Control.Monad.Random (Random)
import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON, ToJSON (..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Binary (Binary (..))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import Data.UUID (UUID)
import Enlil.JWT
import Enlil.OAuth.Scopes
import Enlil.Utils.Binary (JSONBinary (..))
import Enlil.Utils.IDs
import Enlil.Utils.Show (Censored (..))
import Enlil.Utils.URI (URIParam)
import GHC.TypeLits (Symbol, symbolVal)
import Hasql.Interpolate qualified as Hasql
import Servant
import Web.FormUrlEncoded (FromForm (..), ToForm (..))
import Web.FormUrlEncoded qualified as Form

newtype SessionId = SessionId UUID
  deriving stock (Eq, Ord)
  deriving newtype (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "S-" UUID)

newtype UserId = UserId UUID
  deriving stock (Eq, Ord)
  deriving newtype (Binary, Random, Hasql.EncodeValue, Hasql.DecodeValue)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "U-" UUID)

-- | A JWT token ID.
newtype JTI = JTI UUID
  deriving stock (Eq, Ord)
  deriving newtype (Binary, Random)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "JTI-" UUID)

type PendingSessionCookieKey = ("pending_session_id" :: Symbol)

pendingSessionCookieKey :: Text
pendingSessionCookieKey = Text.pack $ symbolVal (Proxy @PendingSessionCookieKey)

-- | Represents a session we're in the process of negotiating.
newtype PendingSessionId = PendingSessionId Text
  deriving stock (Eq, Ord)
  deriving newtype (Binary)
  deriving (IsID, Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "P-" Text)

type OAuth2State = PendingSessionId

newtype PKCEChallenge = PKCEChallenge Text
  deriving (ToHttpApiData, FromHttpApiData, ToJSON, FromJSON) via Text
  deriving (Show) via Censored PKCEChallenge

data PKCEChallengeMethod
  = SHA256Method
  | PlainTextMethod
  deriving (Show)

instance ToHttpApiData PKCEChallengeMethod where
  toQueryParam = \case
    SHA256Method -> "S256"
    PlainTextMethod -> "plain"

instance ToJSON PKCEChallengeMethod where
  toJSON = \case
    SHA256Method -> "S256"
    PlainTextMethod -> "plain"

instance FromHttpApiData PKCEChallengeMethod where
  parseQueryParam = \case
    "S256" -> pure SHA256Method
    "plain" -> pure PlainTextMethod
    txt -> Left $ "Unknown PKCEChallengeMethod: " <> txt

instance FromJSON PKCEChallengeMethod where
  parseJSON = Aeson.withText "PKCEChallengeMethod" $ \txt ->
    either (fail . Text.unpack) pure $ parseQueryParam txt

newtype PKCEVerifier = PKCEVerifier Text
  deriving newtype (ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)
  deriving (Show) via Text

data ResponseType = ResponseTypeCode

instance ToJSON ResponseType where
  toJSON ResponseTypeCode = Aeson.String "code"

instance ToHttpApiData ResponseType where
  toQueryParam = \case
    ResponseTypeCode -> "code"

instance FromHttpApiData ResponseType where
  parseQueryParam "code" = Right ResponseTypeCode
  parseQueryParam _ = Left "'response_type' MUST be 'code'"

data GrantType = AuthorizationCode

instance FromHttpApiData GrantType where
  parseQueryParam "authorization_code" = Right AuthorizationCode
  parseQueryParam _ = Left "'grant_type' MUST be 'authorization_code'"

instance ToHttpApiData GrantType where
  toQueryParam = \case
    AuthorizationCode -> "authorization_code"

newtype AccessToken = AccessToken JWTParam
  deriving newtype (Binary, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
  deriving (Show) via Censored AccessToken

newtype IDToken = IDToken JWTParam
  deriving newtype (Binary, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
  deriving (Show) via Censored IDToken

newtype RefreshToken = RefreshToken JWTParam
  deriving newtype (Binary, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
  deriving (Show) via Censored RefreshToken

data TokenType = BearerToken
  deriving stock (Show)

instance ToJSON TokenType where
  toJSON BearerToken = "bearer"

instance FromJSON TokenType where
  parseJSON = Aeson.withText "token_type" $ \txt -> do
    case Text.toLower txt of
      "bearer" -> pure BearerToken
      _ -> fail $ "Unsupported token_type: " <> Text.unpack txt

data OAuthClientConfig = OAuthClientConfig
  { clientId :: OAuthClientId,
    clientSecret :: Maybe OAuthClientSecret,
    -- | The host at which redirect URIs must be hosted
    redirectHost :: URIParam,
    approvedScopes :: Scopes,
    audience :: URIParam
  }
  deriving stock (Eq, Ord, Show)

instance Hasql.DecodeRow OAuthClientConfig where
  decodeRow =
    Hasql.decodeRow
      <&> \(clientId, clientSecret, redirectHost, approvedScopes, audience) ->
        OAuthClientConfig {..}

newtype OAuthClientId = OAuthClientId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, Hasql.DecodeValue, Hasql.EncodeValue)

newtype OAuthClientSecret = OAuthClientSecret Text
  deriving stock (Eq, Ord)
  deriving (Show) via Censored OAuthClientSecret
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON, Hasql.DecodeValue, Hasql.EncodeValue)

-- | A code received at an OAuth2 client's redirect_uri upon a successful authentication
-- with the oauth2 server. Should be exchanged with the server's Token endpoint to receive
-- an access token.
newtype Code = Code {codeText :: Text}
  deriving (Show) via (Censored Code)
  deriving (FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via Text

-- | Represents all the data required by an authorization endpoint.
-- There are still many parameters specified by:
-- https://openid.net/specs/openid-connect-core-1_0.html#AuthRequest
-- which we don't yet support.
data AuthenticationRequest = AuthenticationRequest
  { clientId :: OAuthClientId,
    redirectURI :: URIParam,
    state :: Text,
    scopes :: Scopes,
    pkceChallenge :: PKCEChallenge,
    pkceChallengeMethod :: PKCEChallengeMethod
  }
  deriving (Binary) via JSONBinary AuthenticationRequest
  deriving stock (Show)

instance ToJSON AuthenticationRequest where
  toJSON (AuthenticationRequest clientId redirectURI state scopes pkceChallenge pkceChallengeMethod) =
    Aeson.object
      [ "client_id" .= clientId,
        "redirect_uri" .= redirectURI,
        "state" .= state,
        "scope" .= scopes,
        "code_challenge" .= pkceChallenge,
        "code_challenge_method" .= pkceChallengeMethod
      ]

instance FromJSON AuthenticationRequest where
  parseJSON = Aeson.withObject "AuthenticationRequest" $ \obj -> do
    clientId <- obj .: "client_id"
    redirectURI <- obj .: "redirect_uri"
    state <- obj .: "state"
    scopes <- obj .: "scope"
    pkceChallenge <- obj .: "code_challenge"
    pkceChallengeMethod <- obj .: "code_challenge_method"
    pure $ AuthenticationRequest {..}

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3
data TokenRequest = TokenRequest
  { grantType :: GrantType,
    code :: Code,
    redirectURI :: URIParam,
    pkceVerifier :: PKCEVerifier,
    clientId :: OAuthClientId,
    clientSecret :: Maybe OAuthClientSecret
  }

instance ToForm TokenRequest where
  toForm (TokenRequest grantType code redirectURI pkceVerifier clientId clientSecret) =
    toForm $
      [ ("grant_type" :: Text, toQueryParam grantType),
        ("code", toQueryParam code),
        ("redirect_uri", toQueryParam redirectURI),
        ("client_id", toQueryParam clientId),
        ("code_verifier", toQueryParam pkceVerifier)
      ]
        <> case clientSecret of
          Just cs -> [("client_secret", toQueryParam cs)]
          Nothing -> []

instance FromForm TokenRequest where
  fromForm form = do
    grantType <- Form.parseUnique "grant_type" form
    code <- Form.parseUnique "code" form
    redirectURI <- Form.parseUnique "redirect_uri" form
    clientId <- Form.parseUnique "client_id" form
    clientSecret <- Form.parseMaybe "client_secret" form
    pkceVerifier <- Form.parseUnique "code_verifier" form
    pure $ TokenRequest {..}

data TokenResponse = TokenResponse
  { accessToken :: AccessToken,
    idToken :: Maybe IDToken,
    tokenType :: TokenType,
    expiresIn :: NominalDiffTime,
    refreshToken :: Maybe RefreshToken,
    scope :: Scopes
  }
  deriving stock (Show)

instance ToJSON TokenResponse where
  toJSON (TokenResponse accessToken idToken tokenType expiresIn refreshToken scope) =
    Aeson.object
      [ "access_token" .= accessToken,
        "id_token" .= idToken,
        "token_type" .= tokenType,
        "expires_in" .= expiresIn,
        "refresh_token" .= refreshToken,
        "scope" .= scope
      ]

instance FromJSON TokenResponse where
  parseJSON = Aeson.withObject "TokenResponse" \obj -> do
    accessToken <- obj Aeson..: "access_token"
    idToken <- obj Aeson..: "id_token"
    tokenType <- obj Aeson..: "token_type"
    expiresIn <- obj Aeson..: "expires_in"
    refreshToken <- obj Aeson..: "refresh_token"
    scope <- obj Aeson..: "scope"
    pure TokenResponse {..}

data RedirectReceiverErr
  = MismatchedState OAuth2State OAuth2State
  | -- (error_type, error_description)
    ErrorFromIdentityProvider Text Text
  | MissingOrExpiredPendingSession
  | MissingState
  | MissingCode
  | FailedToCreateSession
  | InvalidJWTFromIDP JWTError
  deriving stock (Show)
