{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Authentication.AccessToken where

import Control.Lens hiding ((.=))
import Crypto.JWT qualified as Crypto.JWT
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time
import Network.URI (URI)
import Servant.Auth.JWT
import Share.App
import Share.IDs (SessionId, UserId (..))
import Share.JWT (JWTParam (..))
import Share.OAuth.Scopes
import Share.OAuth.Types (AccessToken (..))
import Share.Postgres.Ops qualified as PGO
import Share.Prelude
import Share.User
import Share.Web.App
import Share.Web.Authentication.JWT qualified as Auth.JWT
import Share.Web.Authentication.JWT qualified as AuthJWT
import Share.Web.Authentication.JWT qualified as JWT
import Share.Web.Authentication.Types
import Share.Web.Errors

-- Once we have an easy way to refresh these (e.g. private key stored on the client)
-- we should drop this down to 30 minutes or less.
accessTokenTTL :: NominalDiffTime
accessTokenTTL = 30 * nominalDay

data AccessTokenClaims = AccessTokenClaims
  { standardClaims :: JWT.StandardClaims,
    scope :: Scopes
  }

accessTokenUser :: AccessTokenClaims -> UserId
accessTokenUser = JWT.sub . standardClaims

instance ToJWT AccessTokenClaims where
  encodeJWT (AccessTokenClaims standardClaims scope) =
    JWT.encodeStandardClaims standardClaims (Map.fromList [("scope", Aeson.toJSON scope)])

instance FromJWT AccessTokenClaims where
  decodeJWT :: Crypto.JWT.ClaimsSet -> Either Text AccessTokenClaims
  decodeJWT claims = do
    (standardClaims, extra) <- JWT.decodeStandardClaims claims
    scope <- fromMaybe (Left "Invalid scope") (extra ^? ix "scope" . to (resultEither . Aeson.fromJSON))
    pure (AccessTokenClaims {..})
    where
      resultEither = \case
        Aeson.Error err -> Left (Text.pack err)
        Aeson.Success a -> Right a

-- | A version of verifyAccessToken which returns an Either rather than throwing an exception.
verifyAccessToken' :: Scopes -> AccessToken -> AppM reqCtx (Either AuthenticationErr AccessTokenClaims)
verifyAccessToken' (Scopes requiredScopes) (AccessToken (JWTParam signed)) = do
  Auth.JWT.verifyJWT signed extraClaimsChecks
  where
    extraClaimsChecks (AccessTokenClaims {scope = Scopes tokenScopes}) =
      let missingScopes = Set.difference requiredScopes tokenScopes
       in if null missingScopes
            then Nothing
            else Just (MissingScopes (Scopes missingScopes))

-- | Verifies the validity of a token and returns its claims.
--
-- Checks:
--
-- * Token isn't expired
-- * Token was signed by Share
-- * Token contains all required scopes
verifyAccessToken :: Scopes -> AccessToken -> WebApp AccessTokenClaims
verifyAccessToken requiredScopes token = do
  verifyAccessToken' requiredScopes token >>= \case
    Left err -> respondError err
    Right claims -> pure claims

userForAccessToken :: AccessToken -> WebApp User
userForAccessToken token = do
  userID <- accessTokenUser <$> verifyAccessToken mempty token
  PGO.expectUserById userID

createAccessToken :: Set URI -> UserId -> SessionId -> Scopes -> WebApp AccessToken
createAccessToken aud userID sessionID scope = do
  standardClaims <- JWT.shareStandardClaims aud userID accessTokenTTL sessionID
  let accessTokenClaims = AccessTokenClaims {scope, standardClaims}
  signedJWT <- AuthJWT.signJWT accessTokenClaims
  pure (AccessToken (JWTParam signedJWT))
