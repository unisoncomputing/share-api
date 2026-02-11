module Share.Hasql.Utils
  ( Coerced (..),
  )
where

import Data.Coerce (Coercible, coerce)
import Hasql.Interpolate qualified as Hasql

-- Hasql no longer allows DecodeValue instances to be derived via newtypes, so we work around
-- it.
newtype Coerced a b = Coerced {fromCoerced :: b}

instance (Coercible a b, Hasql.DecodeValue a) => Hasql.DecodeValue (Coerced a b) where
  decodeValue = (Coerced . coerce @a @b) <$> Hasql.decodeValue
