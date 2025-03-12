{-# LANGUAGE RecordWildCards #-}

-- | This module provides helpers for working with JSON Web Tokens (JWTs).
module Share.JWT
  ( -- * App setup
    JWTSettings,
    defaultJWTSettings,

    -- * Signing keys
    SupportedAlg (..),
    KeyDescription (..),
    KeyThumbprint (..),
    KeyMap (..),
    keyDescToJWK,
    publicJWKSet,

    -- * Claims and converting between types
    StandardClaims (..),
    JWTClaimsMap,
    AsJWTClaims (..),
    JSONJWTClaims (..),
    addClaim,
    getClaim,

    -- * JWT operations
    signJWT,
    signJWTWithJWK,
    verifyJWT,

    -- * Utilities
    JWTParam (..),
    textToSignedJWT,
    signedJWTToText,
    createSignedCookie,

    -- * Re-exports
    CryptoError (..),
  )
where

import Control.Lens
import Control.Monad.Except
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.JOSE.JWA.JWS qualified as JWS
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JWT qualified as CryptoJWT
import Crypto.JWT qualified as JWT
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as Aeson
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant
import Share.JWT.Types
import Share.OAuth.Orphans ()
import Share.Utils.Servant.Cookies qualified as Cookies
import UnliftIO (MonadIO (..))

-- | Get the JWK Set value which is safe to expose to the public, e.g. in a JWKS endpoint.
-- This will only include public keys.
--
-- Note that this will not include the legacy key or any HS256 keys, since those don't have a
-- safe public component.
publicJWKSet :: JWTSettings -> JWK.JWKSet
publicJWKSet JWTSettings {validationKeys = KeyMap {byKeyId}} =
  JWK.JWKSet
    ( byKeyId
        & foldMap (\jwk -> jwk ^.. JWK.asPublicKey . _Just)
    )

-- | Create a 'JWTSettings' using the required information.
defaultJWTSettings ::
  -- | The key used to sign JWTs.
  KeyDescription ->
  -- | The legacy key used to verify old JWTs from before key IDs were used. This will be used to verify tokens that don't have a key id.
  Maybe KeyDescription ->
  -- | Any old keys which we still want to accept tokens from. This is useful for key rotation.
  Set KeyDescription ->
  -- | The audiences which represent acceptable audiences on tokens for this service.
  -- Tokens must have an audience which is present in this set.
  --
  -- E.g. https://api.unison.cloud
  Set URI ->
  -- | The token issuer.
  --
  -- E.g. https://api.unison-lang.org
  URI ->
  Either CryptoError JWTSettings
defaultJWTSettings signingKey legacyKey oldValidKeys acceptedAudiences issuer = do
  sjwk@(_, signingJWK) <- keyDescToJWK signingKey
  verificationJWKs <- (sjwk :) <$> traverse keyDescToJWK (Set.toList oldValidKeys)
  let byKeyId = Map.fromList verificationJWKs
  legacyKey <- traverse keyDescToJWK legacyKey <&> fmap snd
  pure $
    JWTSettings
      { signingJWK,
        validationKeys = KeyMap {byKeyId, legacyKey},
        audienceMatches = \s ->
          (review JWT.stringOrUri s) `Set.member` (Set.map (show @URI) acceptedAudiences),
        acceptedAudiences,
        issuer
      }

-- | Converts a 'KeyDescription' to a 'JWK' and a 'KeyThumbprint'.
keyDescToJWK :: KeyDescription -> Either CryptoError (KeyThumbprint, JWK.JWK)
keyDescToJWK (KeyDescription {key, alg}) = cryptoFailableToEither $ do
  case alg of
    HS256 -> do
      let jwk =
            JWK.fromOctets key
              & JWK.jwkUse .~ Just JWK.Sig
              & JWK.jwkAlg .~ Just (JWK.JWSAlg JWS.HS256)
      let thumbprint = jwkThumbprint jwk
      pure (KeyThumbprint thumbprint, jwk & JWK.jwkKid .~ Just thumbprint)
    Ed25519 -> do
      privKey <- Ed25519.secretKey key
      let pubKey = Ed25519.toPublic privKey
      let jwk =
            (JWT.Ed25519Key pubKey (Just privKey))
              & JWT.OKPKeyMaterial
              & JWK.fromKeyMaterial
              & JWK.jwkUse .~ Just JWK.Sig
              & JWK.jwkAlg .~ Just (JWK.JWSAlg JWS.EdDSA)
      let thumbprint = jwkThumbprint jwk
      pure (KeyThumbprint thumbprint, jwk & JWK.jwkKid .~ Just thumbprint)
  where
    cryptoFailableToEither :: CryptoFailable a -> Either CryptoError a
    cryptoFailableToEither (CryptoFailed err) = Left err
    cryptoFailableToEither (CryptoPassed a) = Right a

    jwkThumbprint :: JWK.JWK -> Text
    jwkThumbprint jwk =
      jwk ^. JWK.thumbprint @JWK.SHA256
        & ByteArray.unpack
        & BS.pack
        & Base64URL.encodeUnpadded
        & Text.decodeUtf8

-- | Signs and encodes a JWT using the given 'JWTSettings'.
signJWT :: forall v m. (MonadIO m, AsJWTClaims v) => JWTSettings -> v -> m (Either JWT.JWTError JWT.SignedJWT)
signJWT JWTSettings {signingJWK} v = signJWTWithJWK signingJWK v

-- | Signs and encodes a JWT using the given JWK, you should typically use 'signJWT' instead
-- unless you have a specific reason to use a different JWK.
signJWTWithJWK :: forall m v. (MonadIO m, AsJWTClaims v) => JWK.JWK -> v -> m (Either JWT.JWTError JWT.SignedJWT)
signJWTWithJWK jwk v = runExceptT $ do
  jwtHeader <- mapExceptT liftIO (JWT.makeJWSHeader jwk)
  let claimsJSON = Aeson.toJSON (toClaims v)
  mapExceptT liftIO (JWT.signJWT jwk jwtHeader claimsJSON)

-- | Decodes a JWT and verifies the following:
-- * algorithm (except for legacy tokens)
-- * key id (except for legacy tokens)
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifyJWT :: forall claims m. (AsJWTClaims claims, MonadIO m) => JWTSettings -> JWT.SignedJWT -> m (Either JWT.JWTError claims)
verifyJWT JWTSettings {validationKeys, issuer, acceptedAudiences} signedJWT = runExceptT do
  jwtClaimsMap <- ExceptT . liftIO . runExceptT $ JWT.verifyJWT validators validationKeys signedJWT
  case fromClaims jwtClaimsMap of
    Left err -> throwError $ JWT.JWTClaimsSetDecodeError (Text.unpack err)
    Right claims -> pure claims
  where
    auds :: [CryptoJWT.StringOrURI]
    auds =
      -- Annoyingly StringOrURI doesn't have an ord instance.
      Set.toList acceptedAudiences
        & map (review CryptoJWT.uri)
    validators =
      CryptoJWT.defaultJWTValidationSettings (`elem` auds)
        & CryptoJWT.issuerPredicate .~ (== review CryptoJWT.uri issuer)
        & CryptoJWT.validationSettings
          .~ ( CryptoJWT.defaultValidationSettings
                 -- Limiting the algorithms to ones we use helps limit algorithm substitution attacks.
                 & CryptoJWT.validationSettingsAlgorithms .~ Set.fromList [JWS.HS256, JWS.EdDSA]
             )

-- | Create a signed session cookie using a ToJSON instance.
createSignedCookie :: (MonadIO m, AsJWTClaims session) => JWTSettings -> Cookies.CookieSettings -> Text -> session -> m (Either JWT.JWTError Cookies.SetCookie)
createSignedCookie jwtSettings cookieSettings sessionName value = runExceptT do
  signedJWT <- ExceptT (signJWT jwtSettings value)
  pure $ Cookies.newSetCookie cookieSettings sessionName (signedJWTToText signedJWT)
