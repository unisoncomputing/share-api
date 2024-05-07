module Share.Utils.Binary (JSONBinary (..)) where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Binary (Binary (..))
import Data.ByteString.Lazy qualified as BL

-- | Newtype wrapper for a deriving a Binary instance via an Aeson instance
-- this is a bit less efficient, but it's much easier to maintain back compatibility
-- using JSON than it is for binary formats.
--
-- E.g.
-- @@
-- data Person = Person
--   { name :: String,
--     age :: Int,
--   }
--   deriving (Binary) via JSONBinary Person
--
--  instance ToJSON Person where ...
--  instance FromJSON Person where ...
-- @@
newtype JSONBinary a = JSONBinary a
  deriving newtype (ToJSON, FromJSON)

instance (ToJSON a, FromJSON a) => Binary (JSONBinary a) where
  get = do
    bs <- get @BL.ByteString
    either fail pure $ Aeson.eitherDecode bs
  put a = put $ Aeson.encode a
