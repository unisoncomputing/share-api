{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for working with IDs.
-- Mostly meant to be used with Deriving Via.
module Share.Utils.IDs
  ( IsID (toText, fromText),
    idFrom,
    fromId,
    fromUUID,
    UsingID (..),
    PrefixedID (..),
    CaseInsensitiveID (..),
  )
where

import Control.Exception
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Coerce (Coercible, coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Contravariant (contramap)
import Data.Int
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.Decoders qualified as Decoder
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Read (readMaybe)
import Witch (From (..))

fromId :: forall t a. (From Text a, IsID t) => t -> a
fromId = from . toText

idFrom :: forall a t. (From a Text, IsID t) => a -> Either Text t
idFrom = fromText . from

fromUUID :: (Coercible UUID t) => UUID -> t
fromUUID = coerce

-- | Helper newtype for deriving via, e.g.:
--
-- @@
-- newtype UserId = UserId UUID
--   deriving (IsID, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (PrefixedID "UID-")
-- @@
--
-- This will generate instances which parse and render the underlying UUID with a "UID"
-- prefix in all APIs and url params.
--
-- There are also many places in our APIs where we have arbitrary prefixes on otherwise
-- standardized types. E.g. Hashes may be prefixed with "@" or "#", or not prefixed at all.
-- User Handles may be prefixed with "@" or not prefixed at all.
-- This type allows us to use the same underlying type, but easily alter the ToJSON/FromJSON
-- and ToHttpApiData/FromHttpApiData instances to match the expected format.
newtype PrefixedID (prefix :: Symbol) a = PrefixedID a
  deriving newtype (Eq, Ord, Generic)
  deriving (Show, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON) via (UsingID (PrefixedID prefix a))

instance (KnownSymbol s, IsID a) => IsID (PrefixedID s a) where
  toText (PrefixedID uuid) =
    let prefix = Text.pack (symbolVal (Proxy @s))
     in prefix <> fromId @a @Text uuid
  fromText txt =
    let prefix = Text.pack $ symbolVal (Proxy @s)
     in maybe (Left $ "Could not parse '" <> txt <> "', expected " <> prefix <> "<uuid>") Right $ do
          idText <- Text.stripPrefix prefix txt
          uuid <- eitherToMaybe $ fromText idText
          pure (PrefixedID uuid)
    where
      eitherToMaybe :: Either x a -> Maybe a
      eitherToMaybe = either (const Nothing) Just

instance (IsID a, Typeable a) => FromJSON (UsingID a) where
  parseJSON = Aeson.withText (show $ typeRep (Proxy @a)) $ \txt ->
    case fromText txt of
      Left err -> fail (Text.unpack err)
      Right a -> pure a

instance (IsID a) => ToJSON (UsingID a) where
  toJSON = Aeson.String . toText

-- | Newtype wrapper for deriving instances
newtype UsingID a = UsingID a
  deriving (IsID) via a

instance (IsID a) => Show (UsingID a) where
  show (UsingID a) = Text.unpack . toText $ a

instance (IsID a) => FromHttpApiData (UsingID a) where
  parseQueryParam = fromText

instance (IsID a) => ToHttpApiData (UsingID a) where
  toQueryParam = toText

instance (IsID a) => Hasql.EncodeValue (UsingID a) where
  encodeValue =
    Hasql.encodeValue
      & contramap (\(UsingID a) -> toText a)

newtype IdDeserializationError = IdDeserializationError Text
  deriving stock (Show)
  deriving anyclass (Exception)

instance (IsID a, Typeable a) => Hasql.DecodeValue (UsingID a) where
  decodeValue =
    Hasql.decodeValue
      & Decoder.refine \txt -> fromText txt <&> UsingID

-- | CI doesnt' expose its internal constructor so we can't derive via without adding our own
-- instances.
newtype CaseInsensitiveID = CaseInsensitiveID (CI Text)

instance IsID CaseInsensitiveID where
  toText (CaseInsensitiveID ci) = CI.original ci
  fromText = Right . CaseInsensitiveID . CI.mk

instance Hasql.EncodeValue CaseInsensitiveID where
  encodeValue =
    Hasql.encodeValue
      & contramap \(CaseInsensitiveID ci) -> CI.original ci

instance Hasql.DecodeValue CaseInsensitiveID where
  decodeValue = CaseInsensitiveID . CI.mk <$> Hasql.decodeValue

-- | Generic class for anything that's an ID.
class IsID t where
  toText :: t -> Text
  fromText :: Text -> Either Text t

instance IsID UUID where
  toText = Text.pack . UUID.toString
  fromText = maybe (Left "Invalid UUID") Right . UUID.fromString . Text.unpack

instance IsID Text where
  toText = id
  fromText = Right

instance IsID Int where
  toText n = Text.pack . show $ n
  fromText txt = maybeToEither "Invalid Int" $ readMaybe (Text.unpack txt)
    where
      maybeToEither :: a -> Maybe b -> Either a b
      maybeToEither a = maybe (Left a) Right

instance IsID Int32 where
  toText n = Text.pack . show $ n
  fromText txt = maybeToEither "Invalid Int32" $ readMaybe (Text.unpack txt)
    where
      maybeToEither :: a -> Maybe b -> Either a b
      maybeToEither a = maybe (Left a) Right

instance IsID Int64 where
  toText n = Text.pack . show $ n
  fromText txt = maybeToEither "Invalid Int64" $ readMaybe (Text.unpack txt)
    where
      maybeToEither :: a -> Maybe b -> Either a b
      maybeToEither a = maybe (Left a) Right
