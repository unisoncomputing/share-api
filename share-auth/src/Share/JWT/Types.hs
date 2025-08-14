{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Primitive types and functions for working with JSON Web Tokens (JWTs).
module Share.JWT.Types
  ( JWTParam (..),
    JWTSettings (..),
    Issuer (..),
    Audience (..),
    StandardClaims (..),
    SupportedAlg (..),
    KeyDescription (..),
    KeyMap (..),
    KeyThumbprint (..),
    JWTClaimsMap,
    AsJWTClaims (..),
    JSONJWTClaims (..),
    addClaim,
    getClaim,
    signedJWTToText,
    textToSignedJWT,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Crypto.JOSE qualified as Jose
import Crypto.JWT (HasClaimsSet)
import Crypto.JWT qualified as JWT
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (FromJSON (..))
import Data.Binary
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (UTCTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPTLS
import Network.URI qualified as URI
import Servant
import Share.Utils.Show (Censored (..))
import UnliftIO (MonadIO (..))
import UnliftIO.STM
import Prelude hiding (exp)

-- | A map of claims which represent a JWT
newtype JWTClaimsMap = JWTClaimsMap (Map Text Aeson.Value)
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

instance HasClaimsSet JWTClaimsMap where
  claimsSet = lens getClaims setClaims
    where
      setClaims (JWTClaimsMap cm) cs =
        case Aeson.fromJSON . Aeson.toJSON $ cs of
          Aeson.Success newMap -> JWTClaimsMap (Map.union newMap cm)
          Aeson.Error err -> error $ err
      getClaims cm = do
        case Aeson.fromJSON $ Aeson.toJSON cm of
          Aeson.Success a -> a
          Aeson.Error err -> error $ err

-- | Class for converting a data type to/from a JWT claims map.
--
-- The mixed use of ToJSON/FromJSON ToJWT/FromJWT and HasClaimsSet between servant auth and
-- jose is confusing and error prone, and makes it too easy to accidentally drop or miss
-- validating key claims, so this class unifies them all.
class AsJWTClaims a where
  toClaims :: a -> JWTClaimsMap
  fromClaims :: JWTClaimsMap -> Either Text a

instance (ToJSON a, FromJSON a) => AsJWTClaims (JSONJWTClaims a) where
  toClaims (JSONJWTClaims a) = toClaims . toJSON $ a
  fromClaims c = do
    v <- fromClaims c
    case Aeson.fromJSON v of
      Aeson.Error err -> Left (Text.pack err)
      Aeson.Success a -> Right (JSONJWTClaims a)

instance AsJWTClaims Aeson.Value where
  toClaims = \case
    Aeson.Object o ->
      KeyMap.toMap o
        & Map.mapKeys Key.toText
        & JWTClaimsMap
    _ -> error "Expected object."
  fromClaims (JWTClaimsMap m) =
    m
      & Map.mapKeys Key.fromText
      & KeyMap.fromMap
      & Aeson.Object
      & Right

deriving via JSONJWTClaims JWT.ClaimsSet instance AsJWTClaims JWT.ClaimsSet

instance AsJWTClaims JWTClaimsMap where
  toClaims = id
  fromClaims = Right

-- | Newtype for deriving AsJWTClaims instances for types that have ToJSON/FromJSON instances
-- using 'deriving via JSONJWTClaims Foo'
newtype JSONJWTClaims a = JSONJWTClaims a

-- | Add a claim to a JWTClaimsMap.
addClaim :: (ToJSON a) => Text -> a -> JWTClaimsMap -> JWTClaimsMap
addClaim k v (JWTClaimsMap m) = JWTClaimsMap (Map.insert k (Aeson.toJSON v) m)

-- | Get a claim from a JWTClaimsMap.
getClaim :: (FromJSON a) => Text -> JWTClaimsMap -> Either Text a
getClaim k (JWTClaimsMap m) =
  case Map.lookup k m of
    Nothing -> Left $ "Claim not found: " <> k
    Just v -> case Aeson.fromJSON v of
      Aeson.Error err -> Left $ "Error decoding claim: " <> Text.pack err
      Aeson.Success a -> Right a

-- | Encode a signed JWT as text.
signedJWTToText :: JWT.SignedJWT -> Text
signedJWTToText =
  TL.toStrict . TL.decodeUtf8 . JWT.encodeCompact

-- | Convert from Text to the jose SignedJWT type.
textToSignedJWT :: Text -> Either JWT.JWTError JWT.SignedJWT
textToSignedJWT jwtText = JWT.decodeCompact (TL.encodeUtf8 . TL.fromStrict $ jwtText)

-- | @JWTSettings@ are used to generate and verify JWTs.
data JWTSettings = JWTSettings
  { -- | Key used to sign JWT.
    signingJWK :: Jose.JWK,
    -- | Keys used to validate JWTs.
    validationKeys :: KeyMap,
    -- | Locations to fetch external JWKs from.
    -- Typically this will be a map of issuer: "<issuer>/.well-known/jwks.json"
    externalJWKLocations :: Map Issuer URI,
    -- | The set of audiences the app accepts tokens for.
    acceptedAudiences :: Set Audience,
    -- | The set of issuers the app accepts tokens from.
    acceptedIssuers :: Set Issuer
  }
  deriving (Show) via Censored JWTSettings

-- | Helper for working with standard claim sets more easily/without a bunch of Maybes.
data StandardClaims = StandardClaims
  { sub :: Text,
    iat :: UTCTime,
    exp :: UTCTime,
    iss :: URI,
    aud :: Set URI,
    jti :: Text
  }
  deriving stock (Show, Eq, Ord)

instance HasClaimsSet StandardClaims where
  claimsSet = lens standardClaimsToClaimsSet (\_ claims -> either (\err -> error $ "Invalid claims set: " <> Text.unpack err) id $ standardClaimsFromClaimsSet claims)

instance AsJWTClaims StandardClaims where
  toClaims = toClaims . standardClaimsToClaimsSet
  fromClaims c = do
    cs <- fromClaims @JWT.ClaimsSet c
    standardClaimsFromClaimsSet cs

standardClaimsToClaimsSet :: StandardClaims -> JWT.ClaimsSet
standardClaimsToClaimsSet StandardClaims {..} =
  JWT.emptyClaimsSet
    & JWT.claimSub ?~ (JWT.string # sub)
    & JWT.claimIat ?~ JWT.NumericDate iat
    & JWT.claimExp ?~ JWT.NumericDate exp
    & JWT.claimIss ?~ (JWT.uri # iss)
    & JWT.claimAud ?~ JWT.Audience (review JWT.uri <$> Set.toList aud)
    & JWT.claimJti ?~ jti

standardClaimsFromClaimsSet :: JWT.ClaimsSet -> Either Text StandardClaims
standardClaimsFromClaimsSet claims = do
  sub <- maybe (Left "Invalid 'sub' claim.") Right $ (claims ^? JWT.claimSub . _Just . JWT.string)
  JWT.NumericDate iat <- maybe (Left "Invalid 'iat' claim.") Right $ (claims ^? JWT.claimIat . _Just)
  JWT.NumericDate exp <- maybe (Left "Invalid 'exp' claim.") Right $ (claims ^? JWT.claimExp . _Just)
  iss <- maybe (Left "Invalid 'iss' claim.") Right $ (claims ^? JWT.claimIss . _Just . re JWT.stringOrUri . folding URI.parseURI)
  let aud = (claims & setOf (JWT.claimAud . _Just . folding (\(JWT.Audience auds) -> auds) . JWT.uri))
  when (null aud) $ Left "Invalid 'aud' claim."
  jti <- maybe (Left "Invalid 'jti' claim.") Right $ (claims ^? JWT.claimJti . _Just)
  pure StandardClaims {..}

instance ToJSON StandardClaims where
  toJSON sc = toJSON (standardClaimsToClaimsSet sc)

instance FromJSON StandardClaims where
  parseJSON v = do
    cs <- parseJSON @JWT.ClaimsSet v
    either (fail . Text.unpack) pure $ standardClaimsFromClaimsSet cs

-- | Signing algorithms supported in Unison apps.
data SupportedAlg = HS256 | Ed25519
  deriving (Eq, Ord)

-- | A description of a key used to sign or verify JWTs.
data KeyDescription = KeyDescription {alg :: SupportedAlg, key :: BS.ByteString}
  deriving (Eq, Ord)

-- | A thumbprint of an encryption key
newtype KeyThumbprint = KeyThumbprint Text
  deriving newtype (Eq, Ord)

newtype Issuer = Issuer URI
  deriving newtype (Eq, Ord)

newtype Audience = Audience URI
  deriving newtype (Eq, Ord)

-- | Storage mechanism for keys used to sign and verify JWTs.
--
-- Mostly exists just to implement the 'JWT.VerificationKeyStore' instance for interop with
-- jose.
data KeyMap = KeyMap
  { -- It's important that we verify that the issuer, key id, and algorithm all match
    -- what we expect. Otherwise there are attacks where the caller can create a collision by
    -- using a given key id using a different algorithm, or using a key from one issuer against
    -- another issuer.
    --
    -- This is a TVar so we can auto-update keys from supported issuers when we get validation
    -- failures.
    keysVar :: TVar (Map Issuer [JWT.JWK]),
    -- | The key from before the introduction of key ids.
    -- This will be used to verify legacy tokens, but is also used to sign HashJWTs on share.
    legacyKey :: Maybe JWT.JWK
  }
  deriving (Show) via (Censored KeyMap)

-- | This instance is used to look up the verification keys for a given JWT, assuring that the
-- expected algorithm and key id matches.
instance (MonadIO m, HasClaimsSet payload) => JWT.VerificationKeyStore m (JWT.JWSHeader ()) payload JWTSettings where
  getVerificationKeys header claims (JWTSettings {validationKeys = KeyMap {keysVar, legacyKey}, externalJWKLocations}) = do
    -- Issuer is required on normal claims, but old hashJWTs may not have one.
    let mayIssuer = claims ^? JWT.claimIss . _Just . JWT.uri . to Issuer
    case mayIssuer of
      Nothing -> pure $ maybeToList legacyKey
      Just jwtIssuer -> do
        matchingJWKs <- getJwksForIssuer jwtIssuer
        case matchingJWKs of
          [] -> refreshExternalJWKs jwtIssuer
          matchingJWKs -> pure matchingJWKs
    where
      getJwksForIssuer :: Issuer -> m [JWT.JWK]
      getJwksForIssuer jwtIssuer = do
        keys <- readTVarIO keysVar
        let mayKid = header ^? JWT.kid . _Just . JWT.param
        let jwtAlg = header ^. JWT.alg . JWT.param
        case mayKid of
          -- If we don't have a kid and issuer, it must be a legacy token.
          Nothing -> pure $ maybeToList legacyKey
          Just jwtKid -> do
            case Map.lookup jwtIssuer keys of
              Just issKeys -> do
                pure $ do
                  jwk <- issKeys
                  -- Assert that the key id matches the one in the JWT header.
                  guard (Just jwtKid == jwk ^? JWT.jwkKid . _Just)
                  case jwk ^. JWT.jwkAlg of
                    Just (JWT.JWSAlg jwkAlg)
                      | jwkAlg == jwtAlg ->
                          pure jwk
                    _ -> empty
              _ -> pure mempty
      refreshExternalJWKs :: (MonadIO m) => Issuer -> m [JWT.JWK]
      refreshExternalJWKs iss = fmap (fromMaybe []) . runMaybeT $ do
        uri <- hoistMaybe $ Map.lookup iss externalJWKLocations
        -- We only fetch external JWKs from manually configured trusted jwk URIs
        -- so it's safe not to use a proxy.
        httpMan <- liftIO $ HTTPTLS.getGlobalManager
        req <- hoistMaybe $ HTTP.requestFromURI uri
        respBytes <- liftIO $ HTTP.httpLbs req httpMan
        case Aeson.eitherDecode (HTTP.responseBody respBytes) of
          Left _ -> empty
          Right (JWT.JWKSet jwks) -> do
            -- Add new keys to the key map.
            atomically $ modifyTVar' keysVar $ \m -> do
              Map.insert (iss) jwks m
            -- After refreshing keys, try again to find a match.
            lift $ getJwksForIssuer iss

-- Make an http request to fetch the JWKs from the issuer.

-- | A newtype for JWTs which provides the appropriate encoding/decoding instances.
newtype JWTParam = JWTParam JWT.SignedJWT
  deriving newtype (Eq)
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
