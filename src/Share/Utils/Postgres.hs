module Share.Utils.Postgres
  ( OrdBy (..),
    ordered,
  )
where

import Share.Postgres qualified as PG
import Share.Prelude

-- | A type for propagating an application-code ordering through a database query.
-- We can't trust the order returned by PG, so we make sure to order things explicitly.
newtype OrdBy = OrdBy {unOrdBy :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving (PG.DecodeValue, PG.EncodeValue) via Int32

instance From Int OrdBy where
  from = OrdBy . fromIntegral

instance From Int32 OrdBy where
  from = OrdBy

ordered :: [a] -> [(OrdBy, a)]
ordered = zip (OrdBy <$> [0 ..])
