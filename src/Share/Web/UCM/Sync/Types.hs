module Share.Web.UCM.Sync.Types
  ( EntityBunch (..),
    entityKind,
    RepoInfoKind (..),
  )
where

import Share.IDs (ProjectBranchShortHand, ProjectReleaseShortHand, UserHandle)
import Share.Prelude
import Unison.Sync.Types qualified as Share
import Unison.SyncV2.Types qualified as SyncV2

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

entityKind :: (HasCallStack) => Share.Entity text hash hash' -> SyncV2.EntityKind
entityKind = \case
  Share.C _ -> SyncV2.CausalEntity
  Share.N _ -> SyncV2.NamespaceEntity
  Share.ND _ -> error "entityKind: Unsupported Entity Kind: NamespaceDiff"
  Share.TC _ -> SyncV2.TermEntity
  Share.DC _ -> SyncV2.TypeEntity
  Share.P _ -> SyncV2.PatchEntity
  Share.PD _ -> error "entityKind: Unsupported Entity Kind: PatchDiff"

data RepoInfoKind
  = RepoInfoUser UserHandle
  | RepoInfoProjectBranch ProjectBranchShortHand
  | RepoInfoProjectRelease ProjectReleaseShortHand
  deriving stock (Show)
