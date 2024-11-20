{-# LANGUAGE InstanceSigs #-}

module Share.Utils.Aeson (MaybeEncoded (..), PreEncoded (..)) where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as BL
import GHC.Stack (HasCallStack)

data MaybeEncoded a
  = IsEncoded BL.ByteString
  | NotEncoded a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (ToJSON a) => ToJSON (MaybeEncoded a) where
  toJSON :: (HasCallStack) => MaybeEncoded a -> Aeson.Value
  toJSON (IsEncoded _txt) = error "Tried to encode a MaybeEncoded value back into a Value. Ensure you're using toEncoding instead."
  toJSON (NotEncoded a) = Aeson.toJSON a

  toEncoding :: (HasCallStack) => MaybeEncoded a -> Aeson.Encoding
  toEncoding (IsEncoded txt) = Aeson.toEncoding (PreEncoded txt)
  toEncoding (NotEncoded a) = Aeson.toEncoding a

newtype PreEncoded a = PreEncoded BL.ByteString
  deriving stock (Show, Eq, Ord)

instance ToJSON (PreEncoded a) where
  toJSON :: (HasCallStack) => PreEncoded a -> Aeson.Value
  toJSON (PreEncoded _txt) = error "Tried to encode a PreEncoded value back into a Value. Ensure you're using toEncoding instead."

  toEncoding :: (HasCallStack) => PreEncoded a -> Aeson.Encoding
  toEncoding (PreEncoded txt) = Encoding.unsafeToEncoding . Builder.fromLazyByteString $ txt
