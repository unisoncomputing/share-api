{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Share.Utils.Show (Censored (..), tShow) where

import Data.Text

-- | For deriving a Show instance which won't leak sensitive data.
--
-- E.g.
--
-- @@
-- newtype MySecret = MySecret Text
--   deriving Show via (Censored Text)
-- @@
newtype Censored a = Censored a

instance Show (Censored a) where
  show _ = "<censored>"

tShow :: Show a => a -> Text
tShow = pack . show
