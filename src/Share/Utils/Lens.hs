module Share.Utils.Lens (asListOf) where

import Control.Lens
import GHC.Stack (HasCallStack, withFrozenCallStack)

-- | This is just asListOf, but with a slightly restricted signature and a HasCallStack
-- in case we get a list mismatch.
asListOf :: (HasCallStack) => Traversal s t a b -> Lens s t [a] [b]
asListOf f = withFrozenCallStack unsafePartsOf f
