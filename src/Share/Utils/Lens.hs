module Share.Utils.Lens (asListOf, asListOfDeduped) where

import Control.Lens
import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Witherable (mapMaybe)

-- | This is just asListOf, but with better call stacks and error handling.
-- in case we get a list mismatch.
asListOf :: (HasCallStack) => Traversal s t a b -> Lens s t [a] [b]
asListOf trav f s =
  s
    & unsafePartsOf trav %%~ \as ->
      f as <&> \bs ->
        let aLength = length as
            bLength = length bs
         in if aLength /= bLength
              then error $ "asListOf: length mismatch, expected " ++ show aLength ++ " elements, got " ++ show bLength <> " elements"
              else bs

-- | This is asListOf, but it deduplicates the list before passing it to the function,
-- runs the transformation, then maps the result back to the original list mapping results
-- back to the duplicated slots as necessary.
--
-- >>> [1 :: Int, 1, 10, 1, 3, 10, 5] & asListOfDeduped traversed %%~ \xs -> (length xs, fmap (* 10) xs)
-- (4,[10,10,100,10,30,100,50])
asListOfDeduped :: (HasCallStack, Ord a) => Traversal s t a b -> Lens s t [a] [b]
asListOfDeduped trav f s =
  s
    & asListOf trav %%~ \as -> do
      let asMap = Map.fromList (zip as as)
      resultMap <- asMap & asListOf traversed f
      pure $ mapMaybe (\k -> Map.lookup k resultMap) as
