module Enlil.Web.UCM.Sync.Types
  ( EntityBunch (..),
    EntityKind (..),
    entityKind,
  )
where

import Enlil.Prelude
import Unison.Sync.Types qualified as Share

-- | Helper type for handling entities of different types.
data EntityBunch a = EntityBunch
  { causals :: [a],
    namespaces :: [a],
    terms :: [a],
    types :: [a],
    patches :: [a]
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (EntityBunch a) where
  EntityBunch c1 n1 t1 ty1 p1 <> EntityBunch c2 n2 t2 ty2 p2 =
    EntityBunch (c1 <> c2) (n1 <> n2) (t1 <> t2) (ty1 <> ty2) (p1 <> p2)

instance Monoid (EntityBunch a) where
  mempty = EntityBunch [] [] [] [] []

data EntityKind
  = CausalEntity
  | NamespaceEntity
  | TermEntity
  | TypeEntity
  | PatchEntity
  deriving (Show, Eq, Ord)

entityKind :: HasCallStack => Share.Entity text hash hash' -> EntityKind
entityKind = \case
  Share.C _ -> CausalEntity
  Share.N _ -> NamespaceEntity
  Share.ND _ -> error "entityKind: Unsupported Entity Kind: NamespaceDiff"
  Share.TC _ -> TermEntity
  Share.DC _ -> TypeEntity
  Share.P _ -> PatchEntity
  Share.PD _ -> error "entityKind: Unsupported Entity Kind: PatchDiff"
