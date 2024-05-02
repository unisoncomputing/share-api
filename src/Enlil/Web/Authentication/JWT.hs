{-# LANGUAGE RecordWildCards #-}

module Enlil.Web.Authentication.JWT where

import Control.Lens hiding ((.=))
import Crypto.JWT
import Crypto.JWT qualified as JWT
import Data.Aeson qualified as Aeson
import Data.Either.Combinators qualified as Either
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Enlil.App
import Enlil.Env qualified as Env
import Enlil.IDs (JTI (..), SessionId (..), UserId (..))
import Enlil.IDs qualified as IDs
import Enlil.JWT qualified as JWT
import Enlil.Prelude
import Enlil.Web.App
import Enlil.Web.Authentication.Types
import Enlil.Web.Errors
import Network.URI (URI)
import Network.URI qualified as URI

-- | Helper for working with standard claim sets more easily/without a bunch of Maybes.
data StandardClaims = StandardClaims
  { sub :: UserId,
    iat :: UTCTime,
    exp :: UTCTime,
    iss :: URI,
    aud :: Set URI,
    jti :: Text
  }
  deriving stock (Show, Eq, Ord)

instance JWT.ToJWT StandardClaims where
  encodeJWT (StandardClaims {sub, iat, exp, iss, aud, jti}) =
    JWT.emptyClaimsSet
      & JWT.claimSub ?~ (JWT.string # IDs.toText sub)
      & JWT.claimIat ?~ JWT.NumericDate iat
      & JWT.claimExp ?~ JWT.NumericDate exp
      & JWT.claimIss ?~ (JWT.uri # iss)
      & JWT.claimAud ?~ JWT.Audience (review JWT.uri <$> Set.toList aud)
      & JWT.claimJti ?~ jti

instance JWT.FromJWT StandardClaims where
  decodeJWT claims = do
    sub <- maybe (Left "Invalid 'sub' claim.") Right $ (claims ^? JWT.claimSub . _Just . JWT.string . folding (IDs.fromText @UserId))
    JWT.NumericDate iat <- maybe (Left "Invalid 'iat' claim.") Right $ (claims ^? JWT.claimIat . _Just)
    JWT.NumericDate exp <- maybe (Left "Invalid 'exp' claim.") Right $ (claims ^? JWT.claimExp . _Just)
    iss <- maybe (Left "Invalid 'iss' claim.") Right $ (claims ^? JWT.claimIss . _Just . re JWT.stringOrUri . folding URI.parseURI)
    let aud = (claims & setOf (JWT.claimAud . _Just . folding (\(Audience auds) -> auds) . JWT.uri))
    when (null aud) $ Left "Invalid 'aud' claim."
    jti <- maybe (Left "Invalid 'jti' claim.") Right $ (claims ^? JWT.claimJti . _Just)
    pure StandardClaims {..}

-- | Encode a JWT from standard claims and additional claims
encodeStandardClaims :: StandardClaims -> Map Text Aeson.Value -> ClaimsSet
encodeStandardClaims standardClaims additionalClaims =
  JWT.encodeJWT standardClaims
    & JWT.unregisteredClaims .~ additionalClaims

decodeStandardClaims :: ClaimsSet -> Either Text (StandardClaims, Map Text Aeson.Value)
decodeStandardClaims claims = do
  standardClaims <- JWT.decodeJWT claims
  let additionalClaims = claims ^. JWT.unregisteredClaims
  pure (standardClaims, additionalClaims)

enlilStandardClaims :: Set URI -> UserId -> NominalDiffTime -> SessionId -> AppM reqCtx StandardClaims
enlilStandardClaims aud sub ttl (SessionId sessionIdUUID) = do
  let jti = IDs.toText $ JTI sessionIdUUID
  iss <- enlilIssuer
  iat <- liftIO getCurrentTime
  let exp = addUTCTime ttl iat
  pure (StandardClaims {..})

signJWT :: JWT.ToJWT a => a -> WebApp SignedJWT
signJWT claims = do
  jSettings <- asks Env.jwtSettings
  JWT.signJWT jSettings claims >>= \case
    Left err -> respondError (InternalServerError "jwt:signing-error" err)
    Right a -> pure a

-- | Decodes a JWT and verifies the following:
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifyJWT :: forall claims reqCtx. (JWT.FromJWT claims, Typeable claims) => SignedJWT -> (claims -> Maybe AuthenticationErr) -> AppM reqCtx (Either AuthenticationErr claims)
verifyJWT signedJWT checks = do
  jwtS <- asks Env.jwtSettings
  Either.mapLeft JWTErr <$> JWT.verifyJWT @claims jwtS signedJWT <&> \case
    Left err -> Left err
    Right a -> case checks a of
      Nothing -> Right a
      Just err -> Left err
