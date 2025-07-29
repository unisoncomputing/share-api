{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Share.Prelude.Orphans () where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Align (Semialign (..))
import Data.Text (Text)
import Data.These (These (..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.TypeLits qualified as TypeError
import Hasql.Interpolate qualified as Interp
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Unison.Server.Orphans ()
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Witch

instance {-# OVERLAPPING #-} (TypeError.TypeError ('TypeError.Text "A String will be encoded as char[], Did you mean to use Text instead?")) => Interp.EncodeValue String where
  encodeValue = error "unpossible"

instance {-# OVERLAPPING #-} (TypeError.TypeError ('TypeError.Text "Strings are decoded as a char[], Did you mean to use Text instead?")) => Interp.DecodeValue String where
  decodeValue = error "unpossible"

-- Useful instance, but doesn't exist in either lib, likely because they just don't want to depend on one another.
instance (Semialign f) => Semialign (Cofree f) where
  align :: Cofree f a -> Cofree f b -> Cofree f (These a b)
  align (a :< l) (b :< r) =
    These a b :< alignWith go l r
    where
      go :: forall x y. These (Cofree f x) (Cofree f y) -> Cofree f (These x y)
      go = \case
        This x -> This <$> x
        That y -> That <$> y
        These x y -> align x y

instance From UUID Text where
  from = UUID.toText

instance From ShortHash Text where
  from = SH.toText

instance (MonadTracer m) => MonadTracer (MaybeT m) where
  getTracer = lift getTracer
