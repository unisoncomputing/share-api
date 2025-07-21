-- | A Typeclass for decoding composite values
module Share.Postgres.Composites
  ( DecodeComposite (..),
    CompositeRow (..),
  )
where

import Hasql.Decoders (Composite)
import Hasql.Decoders qualified as Decoders
import Hasql.Interpolate qualified as Hasql

-- | Annoyingly, Hasql expects all values to be deserialized by a single column,
-- to deserialize multiple columns to a single value you need to use Composite.
-- But using composite decoders requires passing the component decoder manually, which doesn't
-- play well with the Hasql.DecodeValue typeclass.
--
-- So we introduce DecodeComposite to bridge the gap.
-- Wrapping any type in 'CompositeRow' will use the DecodeComposite instance for it, which
-- allows parsing multiple columns into a single value, e.g. when returning arrays of tuples
-- like '[[(a, b)]]'.
class DecodeComposite a where
  -- | Decode a composite value
  decodeComposite :: Composite a

newtype CompositeRow a = CompositeRow {compositeValue :: a}

instance (DecodeComposite a) => Hasql.DecodeValue (CompositeRow a) where
  decodeValue = CompositeRow <$> Decoders.composite decodeComposite

instance (Hasql.DecodeField a, Hasql.DecodeField b) => DecodeComposite (a, b) where
  decodeComposite = do
    a <- Decoders.field Hasql.decodeField
    b <- Decoders.field Hasql.decodeField
    pure (a, b)
