module Share.Utils.Data
  ( zip2,
    zipWith2,
    zip3,
    zipWith3,
    zip4,
    zipWith4,
    zip5,
    zipWith5,
    mapFromSelf,
  )
where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Zip qualified as Zip
import Prelude as X hiding (zip3, zipWith3)

zip2 :: (Zip.Zip t) => t a -> t b -> t (a, b)
zip2 as bs =
  Zip.zip as bs

-- | Zip together two alignable structures with a function.
-- The function comes last for easier chaining.
zipWith2 :: (Zip.Zip t) => t a -> t b -> (a -> b -> c) -> t c
zipWith2 as bs f =
  Zip.zipWith (\a b -> f a b) as bs

zip3 :: (Zip.Zip t) => t a -> t b -> t c -> t (a, b, c)
zip3 as bs cs =
  Zip.zipWith (\(a, b) c -> (a, b, c)) (Zip.zip as bs) cs

-- | Zip together three alignable structures with a function.
-- The function comes last for easier chaining.
zipWith3 :: (Zip.Zip t) => t a -> t b -> t c -> (a -> b -> c -> d) -> t d
zipWith3 as bs cs f =
  zip3 as bs cs
    <&> (\(a, b, c) -> f a b c)

zip4 :: (Zip.Zip t) => t a -> t b -> t c -> t d -> t (a, b, c, d)
zip4 as bs cs ds =
  Zip.zipWith (\(a, b) (c, d) -> (a, b, c, d)) (Zip.zip as bs) (Zip.zip cs ds)

-- | Zip together four alignable structures with a function.
-- The function comes last for easier chaining.
zipWith4 :: (Zip.Zip t) => t a -> t b -> t c -> t d -> (a -> b -> c -> d -> e) -> t e
zipWith4 as bs cs ds f =
  zip4 as bs cs ds
    <&> (\(a, b, c, d) -> f a b c d)

zip5 :: (Zip.Zip t) => t a -> t b -> t c -> t d -> t e -> t (a, b, c, d, e)
zip5 as bs cs ds es =
  Zip.zipWith (\(a, b) (c, d, e) -> (a, b, c, d, e)) (Zip.zip as bs) (zip3 cs ds es)

zipWith5 :: (Zip.Zip t) => t a -> t b -> t c -> t d -> t e -> (a -> b -> c -> d -> e -> f) -> t f
zipWith5 as bs cs ds es f =
  zip5 as bs cs ds es
    <&> (\(a, b, c, d, e) -> f a b c d e)

mapFromSelf :: (Ord k) => [k] -> Map k k
mapFromSelf ks =
  Map.fromList (ks <&> \k -> (k, k))
