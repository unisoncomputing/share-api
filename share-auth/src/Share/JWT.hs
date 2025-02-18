{-# LANGUAGE RecordWildCards #-}

-- | This module provides helpers for working with JSON Web Tokens (JWTs).
module Share.JWT
  ( JWTSettings,
    defaultJWTSettings,
    SupportedAlg (..),
    KeyDescription (..),
    JWTParam (..),
    ServantAuth.ToJWT (..),
    ServantAuth.FromJWT (..),
    signJWT,
    verifyJWT,

    -- * Additional Helpers
    textToSignedJWT,
    signedJWTToText,
    createSignedCookie,
    publicJWKSet,

    -- * Re-exports
    CryptoError (..),
  )
where

import Control.Applicative (empty)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Except
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.JOSE qualified as Jose
import Crypto.JOSE.JWA.JWS qualified as JWS
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JWT qualified as CryptoJWT
import Crypto.JWT qualified as JWT
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Binary
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Typeable (Typeable, typeRep)
import Servant
import Servant.Auth.Server qualified as ServantAuth
import Share.OAuth.Orphans ()
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Utils.Show (Censored (..))
import UnliftIO (MonadIO (..))

-- | @JWTSettings@ are used to generate and verify JWTs.
data JWTSettings = JWTSettings
  { -- | Key used to sign JWT.
    signingJWK :: Jose.JWK,
    -- | Keys used to validate JWT.
    validationKeys :: KeyMap,
    -- | An @aud@ predicate. The @aud@ is a string or URI that identifies the
    -- intended recipient of the JWT.
    audienceMatches :: JWT.StringOrURI -> Bool,
    -- | The set of audiences the app accepts tokens for.
    acceptedAudiences :: Set URI,
    issuer :: URI
  }
  deriving (Show) via Censored JWTSettings

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

data SupportedAlg = HS256 | Ed25519
  deriving (Eq, Ord)

data KeyDescription = KeyDescription {alg :: SupportedAlg, key :: BS.ByteString}
  deriving (Eq, Ord)

newtype KeyThumbprint = KeyThumbprint Text
  deriving newtype (Eq, Ord)

data KeyMap = KeyMap
  { byKeyId :: (Map KeyThumbprint JWT.JWK),
    -- | The key from before the introduction of key ids. This will be used to verify legacy tokens, but can eventually be removed.
    legacyKey :: Maybe JWT.JWK
  }
  deriving (Show) via (Censored KeyMap)

-- | This instance is used to look up the verification keys for a given JWT, assuring that the
-- expected algorithm and key id matches.
instance (Applicative m) => JWT.VerificationKeyStore m (JWT.JWSHeader ()) JWT.ClaimsSet KeyMap where
  getVerificationKeys header _claims (KeyMap km legacyKey) =
    case (header ^? JWT.kid . _Just . JWT.param) of
      Nothing -> pure $ maybeToList legacyKey
      Just jwtKid -> pure . maybe [] List.singleton $ do
        let jwtAlg = header ^. JWT.alg . JWT.param
        case Map.lookup (KeyThumbprint jwtKid) km of
          Just key | Just (JWT.JWSAlg keyAlg) <- key ^. JWT.jwkAlg -> do
            guard (keyAlg == jwtAlg)
            pure key
          _ -> empty

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
defaultJWTSettings signingKey legacyKey oldValidKeys acceptedAudiences issuer = toEither do
  sjwk@(_, signingJWK) <- toJWK signingKey
  verificationJWKs <- (sjwk :) <$> traverse toJWK (Set.toList oldValidKeys)
  let byKeyId = Map.fromList verificationJWKs
  legacyKey <- traverse toJWK legacyKey <&> fmap snd
  pure $
    JWTSettings
      { signingJWK,
        validationKeys = KeyMap {byKeyId, legacyKey},
        audienceMatches = \s ->
          (review JWT.stringOrUri s) `Set.member` (Set.map (show @URI) acceptedAudiences),
        acceptedAudiences,
        issuer
      }
  where
    toEither :: CryptoFailable a -> Either CryptoError a
    toEither (CryptoFailed err) = Left err
    toEither (CryptoPassed a) = Right a
    toJWK :: KeyDescription -> CryptoFailable (KeyThumbprint, JWK.JWK)
    toJWK (KeyDescription {key, alg}) =
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

    jwkThumbprint :: JWK.JWK -> Text
    jwkThumbprint jwk =
      jwk ^. JWK.thumbprint @JWK.SHA256
        & ByteArray.unpack
        & BS.pack
        & Base64URL.encodeUnpadded
        & Text.decodeUtf8

newtype JWTParam = JWTParam JWT.SignedJWT
  deriving (Show) via (Censored JWTParam)

instance ToHttpApiData JWTParam where
  toQueryParam (JWTParam signed) = signedJWTToText signed

instance ToJSON JWTParam where
  toJSON = Aeson.String . toQueryParam

instance FromHttpApiData JWTParam where
  parseQueryParam txt = bimap (Text.pack . show) JWTParam $ textToSignedJWT txt

instance FromJSON JWTParam where
  parseJSON = Aeson.withText "jwt" $ either (fail . Text.unpack) pure . parseQueryParam

instance Binary JWTParam where
  put = put . toQueryParam
  get =
    (parseQueryParam <$> get)
      >>= ( \case
              Left err -> fail (Text.unpack err)
              Right a -> pure a
          )

-- | Encode a signed JWT as text.
signedJWTToText :: JWT.SignedJWT -> Text
signedJWTToText =
  TL.toStrict . TL.decodeUtf8 . JWT.encodeCompact

textToSignedJWT :: Text -> Either JWT.JWTError JWT.SignedJWT
textToSignedJWT jwtText = JWT.decodeCompact (TL.encodeUtf8 . TL.fromStrict $ jwtText)

-- | Signs and encodes a JWT using the given 'JWTSettings'.
signJWT :: forall m v. (MonadIO m, ServantAuth.ToJWT v) => JWTSettings -> v -> m (Either JWT.JWTError JWT.SignedJWT)
signJWT JWTSettings {signingJWK} v = runExceptT $ do
  let claimsSet = ServantAuth.encodeJWT v
  jwtHeader <- mapExceptT liftIO (JWT.makeJWSHeader signingJWK)
  mapExceptT liftIO (JWT.signClaims signingJWK jwtHeader claimsSet)

-- | Decodes a JWT and verifies the following:
-- * algorithm (except for legacy tokens)
-- * key id (except for legacy tokens)
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifyJWT :: forall claims m. (Typeable claims, MonadIO m, ServantAuth.FromJWT claims) => JWTSettings -> JWT.SignedJWT -> m (Either JWT.JWTError claims)
verifyJWT JWTSettings {validationKeys, issuer, acceptedAudiences} signedJWT = do
  result :: Either JWT.JWTError JWT.ClaimsSet <- liftIO . runExceptT $ JWT.verifyClaims validators validationKeys signedJWT
  pure $ do
    claimsSet <- result
    case ServantAuth.decodeJWT claimsSet of
      Left err -> Left . JWT.JWTClaimsSetDecodeError $ "Failed to decode " <> show (typeRep (Proxy @claims)) <> ": " <> Text.unpack err
      Right claims -> Right claims
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

-- | Create a signed session cookie using a ToJWT instance.
createSignedCookie :: (MonadIO m, ServantAuth.ToJWT session) => JWTSettings -> Cookies.CookieSettings -> Text -> session -> m (Either JWT.JWTError Cookies.SetCookie)
createSignedCookie jwtSettings cookieSettings sessionName value = runExceptT do
  signedJWT <- ExceptT (signJWT jwtSettings value)
  pure $ Cookies.newSetCookie cookieSettings sessionName (signedJWTToText signedJWT)
