{-# LANGUAGE RecordWildCards #-}

module Enlil.JWT
  ( JWTSettings (..),
    defaultJWTSettings,
    JWTParam (..),
    ServantAuth.ToJWT (..),
    ServantAuth.FromJWT (..),
    signJWT,
    verifyJWT,

    -- * Additional Helpers
    textToSignedJWT,
    signedJWTToText,
    createSignedCookie,
  )
where

import Control.Lens
import Control.Monad.Except
import Crypto.JOSE as Jose
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JWT
import Crypto.JWT as Jose
import Crypto.JWT qualified as CryptoJWT
import Crypto.JWT qualified as JWT
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Binary
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Typeable (Typeable, typeRep)
import Enlil.Utils.Servant.Cookies qualified as Cookies
import Enlil.Utils.Show (Censored (..))
import Servant
import Servant.Auth.Server qualified as ServantAuth

-- | @JWTSettings@ are used to generate and verify JWTs.
data JWTSettings = JWTSettings
  { -- | Key used to sign JWT.
    jwk :: Jose.JWK,
    -- | Keys used to validate JWT.
    validationKeys :: IO Jose.JWKSet,
    -- | An @aud@ predicate. The @aud@ is a string or URI that identifies the
    -- intended recipient of the JWT.
    audienceMatches :: Jose.StringOrURI -> Bool,
    -- | The set of audiences the app accepts tokens for.
    acceptedAudiences :: Set URI,
    issuer :: URI
  }
  deriving (Show) via Censored JWTSettings

-- | Create a 'JWTSettings' using the given secret key and accepted audiences.
defaultJWTSettings ::
  -- | The secret key used to sign and verify JWTs.
  BS.ByteString ->
  -- | The audiences which represent acceptable audiences on tokens.
  -- E.g. https://api.unison.cloud
  Set URI ->
  -- | Issuers for tokens.
  URI ->
  JWTSettings
defaultJWTSettings hs256Key acceptedAudiences issuer =
  JWTSettings
    { jwk,
      validationKeys = pure $ Jose.JWKSet [jwk],
      audienceMatches = \s ->
        (review stringOrUri s) `Set.member` (Set.map (show @URI) acceptedAudiences),
      acceptedAudiences,
      issuer
    }
  where
    jwk = JWK.fromOctets hs256Key

newtype JWTParam = JWTParam SignedJWT
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
signedJWTToText :: SignedJWT -> Text
signedJWTToText =
  TL.toStrict . TL.decodeUtf8 . encodeCompact

textToSignedJWT :: Text -> Either JWTError SignedJWT
textToSignedJWT jwtText = decodeCompact (TL.encodeUtf8 . TL.fromStrict $ jwtText)

-- | Signs and encodes a JWT using the given 'JWTSettings'.
signJWT :: (MonadIO m, ServantAuth.ToJWT v) => JWTSettings -> v -> m (Either JWTError SignedJWT)
signJWT JWTSettings {jwk} v = do
  let claimsSet = ServantAuth.encodeJWT v
  liftIO $ runExceptT (signClaims jwk jwtHeader claimsSet)

jwtHeader :: JWT.JWSHeader ()
jwtHeader = JWT.newJWSHeader ((), jwtAlgorithm)

-- | We currently only support hs256 JWTs, we can easily change this later if needed, but
-- be wary of algorithm subtitution attacks: https://datatracker.ietf.org/doc/html/rfc7515#section-10.7
jwtAlgorithm :: JWT.Alg
jwtAlgorithm = JWT.HS256

-- | Decodes a JWT and verifies the following:
-- * issuer
-- * audience
-- * expiry
-- * signature
--
-- Any other checks should be performed on the returned claims.
verifyJWT :: forall claims m. (Typeable claims, MonadIO m, ServantAuth.FromJWT claims) => JWTSettings -> SignedJWT -> m (Either JWTError claims)
verifyJWT JWTSettings {jwk, issuer, acceptedAudiences} signedJWT = do
  result :: Either JWTError ClaimsSet <- liftIO . runExceptT $ JWT.verifyClaims validators jwk signedJWT
  pure $ do
    claimsSet <- result
    case ServantAuth.decodeJWT claimsSet of
      Left err -> Left . JWTClaimsSetDecodeError $ "Failed to decode " <> show (typeRep (Proxy @claims)) <> ": " <> Text.unpack err
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
                 -- Hard-coding the algorithm prevents algorithm substitution attacks.
                 & CryptoJWT.validationSettingsAlgorithms .~ Set.singleton jwtAlgorithm
             )

-- | Create a signed session cookie using a ToJWT instance.
createSignedCookie :: (MonadIO m, ServantAuth.ToJWT session) => JWTSettings -> Cookies.CookieSettings -> Text -> session -> m (Either JWTError Cookies.SetCookie)
createSignedCookie jwtSettings cookieSettings sessionName value = runExceptT do
  signedJWT <- ExceptT (signJWT jwtSettings value)
  pure $ Cookies.newSetCookie cookieSettings sessionName (signedJWTToText signedJWT)
