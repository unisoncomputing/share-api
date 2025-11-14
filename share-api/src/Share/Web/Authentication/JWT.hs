{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Share.Web.Authentication.JWT where

import Control.Lens hiding ((.=))
import Crypto.JWT (SignedJWT)
import Data.Either.Combinators qualified as Either
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Share.App
import Share.Env.Types qualified as Env
import Share.IDs (JTI (..), SessionId (..), UserId (..))
import Share.IDs qualified as IDs
import Share.JWT
import Share.JWT qualified as JWT
import Share.Prelude
import Share.Web.App
import Share.Web.Authentication.Types
import Share.Web.Errors

shareStandardClaims :: Set Audience -> UserId -> NominalDiffTime -> SessionId -> AppM reqCtx JWT.StandardClaims
shareStandardClaims aud sub ttl (SessionId sessionIdUUID) = do
  let jti = IDs.toText $ JTI sessionIdUUID
  iss <- shareIssuer
  iat <- liftIO getCurrentTime
  let exp = addUTCTime ttl iat
  pure (JWT.StandardClaims {sub = IDs.toText sub, ..})

signJWT :: (JWT.AsJWTClaims a) => a -> WebApp SignedJWT
signJWT claims = do
  jSettings <- asks Env.jwtSettings
  Share.JWT.signJWT jSettings claims >>= \case
    Left err -> respondError (InternalServerError "jwt:signing-error" err)
    Right a -> pure a

-- | Decodes a JWT and verifies the following:
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifyJWT :: forall claims reqCtx. (JWT.AsJWTClaims claims) => SignedJWT -> (claims -> Maybe AuthenticationErr) -> AppM reqCtx (Either AuthenticationErr claims)
verifyJWT signedJWT checks = do
  jwtS <- asks Env.jwtSettings
  Either.mapLeft JWTErr <$> Share.JWT.verifyJWT @claims jwtS signedJWT <&> \case
    Left err -> Left err
    Right a -> case checks a of
      Nothing -> Right a
      Just err -> Left err
