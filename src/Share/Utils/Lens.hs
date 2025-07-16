{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Share.Utils.Lens (asList) where

import Control.Lens
import GHC.Stack (HasCallStack)

-- | This is just unsafePartsOf, but with a slightly restricted signature and a HasCallStack
-- in case we get a list mismatch.
asList :: (HasCallStack) => Traversal s t a b -> Lens s t [a] [b]
asList = unsafePartsOf
