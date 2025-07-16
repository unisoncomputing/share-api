module Share.Utils.Lens (asListOf) where

import Control.Lens
import GHC.Stack (HasCallStack)

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
