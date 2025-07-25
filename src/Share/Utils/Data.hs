module Share.Utils.Data (zip3, zip4, mapFromSelf) where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Zip qualified as Zip
import Prelude as X hiding (zip3)

zip3 :: (Zip.Zip t) => t a -> t b -> t c -> t (a, b, c)
zip3 as bs cs =
  Zip.zipWith (\(a, b) c -> (a, b, c)) (Zip.zip as bs) cs

zip4 :: (Zip.Zip t) => t a -> t b -> t c -> t d -> t (a, b, c, d)
zip4 as bs cs ds =
  Zip.zipWith (\(a, b) (c, d) -> (a, b, c, d)) (Zip.zip as bs) (Zip.zip cs ds)

mapFromSelf :: (Ord k) => [k] -> Map k k
mapFromSelf ks =
  Map.fromList (ks <&> \k -> (k, k))
