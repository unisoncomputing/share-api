{-# LANGUAGE InstanceSigs #-}

module Share.Utils.Aeson (MaybeEncoded (..), PreEncoded (..)) where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Binary.Builder qualified as Builder
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stack (HasCallStack)

data MaybeEncoded a
  = IsEncoded TL.Text
  | NotEncoded a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (ToJSON a) => ToJSON (MaybeEncoded a) where
  toJSON :: (HasCallStack) => MaybeEncoded a -> Aeson.Value
  toJSON (IsEncoded _txt) = error "Tried to encode a MaybeEncoded value back into a Value. Ensure you're using toEncoding instead."
  toJSON (NotEncoded a) = Aeson.toJSON a

  toEncoding :: (HasCallStack) => MaybeEncoded a -> Aeson.Encoding
  toEncoding (IsEncoded txt) = Aeson.toEncoding (PreEncoded txt)
  toEncoding (NotEncoded a) = Aeson.toEncoding a

newtype PreEncoded = PreEncoded TL.Text
  deriving stock (Show, Eq, Ord)

instance ToJSON PreEncoded where
  toJSON :: (HasCallStack) => PreEncoded -> Aeson.Value
  toJSON (PreEncoded _txt) = error "Tried to encode a PreEncoded value back into a Value. Ensure you're using toEncoding instead."

  toEncoding :: (HasCallStack) => PreEncoded -> Aeson.Encoding
  toEncoding (PreEncoded txt) = Encoding.unsafeToEncoding $ Builder.fromLazyByteString $ TL.encodeUtf8 txt
