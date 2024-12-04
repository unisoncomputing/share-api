{-# LANGUAGE InstanceSigs #-}

module Share.Utils.Aeson (MaybeEncoded (..), PreEncoded (..)) where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)

data MaybeEncoded a
  = IsEncoded BL.ByteString
  | NotEncoded a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Typeable a, ToJSON a) => ToJSON (MaybeEncoded a) where
  toJSON :: (HasCallStack) => MaybeEncoded a -> Aeson.Value
  toJSON (IsEncoded txt) = Aeson.toJSON (PreEncoded @a txt)
  toJSON (NotEncoded a) = Aeson.toJSON a

  toEncoding :: (HasCallStack) => MaybeEncoded a -> Aeson.Encoding
  toEncoding (IsEncoded txt) = Aeson.toEncoding (PreEncoded @a txt)
  toEncoding (NotEncoded a) = Aeson.toEncoding a

newtype PreEncoded a = PreEncoded BL.ByteString
  deriving stock (Show, Eq, Ord)

instance (Typeable a) => ToJSON (PreEncoded a) where
  toJSON :: (HasCallStack) => PreEncoded a -> Aeson.Value
  toJSON (PreEncoded txt) =
    -- It's regrettable we have to do this, but seemingly it's required when building values
    -- with @@object [key .= val]@@ syntax.
    fromMaybe (error $ "Invalid PreEncoded JSON for type: " <> show (typeRep (Proxy @a))) $ Aeson.decode txt

  toEncoding :: (HasCallStack) => PreEncoded a -> Aeson.Encoding
  toEncoding (PreEncoded txt) = Encoding.unsafeToEncoding . Builder.fromLazyByteString $ txt
