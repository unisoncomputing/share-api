module Share.Utils.Postgres
  ( OrdBy (..),
    ordered,
    RawBytes (..),
    RawLazyBytes (..),
  )
where

import Data.ByteString.Lazy qualified as BL
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Share.Prelude

-- | A type for propagating an application-code ordering through a database query.
-- We can't trust the order returned by PG, so we make sure to order things explicitly.
newtype OrdBy = OrdBy {unOrdBy :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (Hasql.DecodeValue, Hasql.EncodeValue) via Int32

instance From Int OrdBy where
  from = OrdBy . fromIntegral

instance From Int32 OrdBy where
  from = OrdBy

ordered :: [a] -> [(OrdBy, a)]
ordered = zip (OrdBy <$> [0 ..])

-- | Preferably you should use custom newtypes for your bytes, but you can use this with
-- deriving via to get the encoding/decoding instances.
newtype RawBytes = RawBytes {unRawBytes :: ByteString}
  deriving stock (Show, Eq, Ord)

instance Hasql.EncodeValue RawBytes where
  encodeValue = contramap unRawBytes Encoders.bytea

instance Hasql.DecodeValue RawBytes where
  decodeValue = RawBytes <$> Decoders.bytea

-- | Preferably you should use custom newtypes for your bytes, but you can use this with
-- deriving via to get the encoding/decoding instances.
newtype RawLazyBytes = RawLazyBytes {unLazyRawBytes :: BL.ByteString}
  deriving stock (Show, Eq, Ord)

instance Hasql.EncodeValue RawLazyBytes where
  encodeValue = contramap (BL.toStrict . unLazyRawBytes) Encoders.bytea

instance Hasql.DecodeValue RawLazyBytes where
  decodeValue = RawLazyBytes . BL.fromStrict <$> Decoders.bytea
