module Share.NamespaceDiffs.Types
  ( DefinitionDiffs (..),
    DefinitionDiff (..),
    DefinitionDiffKind (..),
    NamespaceDiffError (..),
    DiffAtPath (..),
    NamespaceTreeDiff,
    diffAtPathReferents_,
    diffAtPathReferences_,
    namespaceTreeDiffReferents_,
    namespaceTreeDiffReferences_,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Lens hiding ((:<))
import Data.Map qualified as Map
import Data.Set.NonEmpty (NESet)
import Servant (err500)
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Unison.Codebase.Path (Path)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Util.Set qualified as Set

data NamespaceDiffError
  = ImpossibleError Text
  deriving stock (Show)

instance ToServerError NamespaceDiffError where
  toServerError ImpossibleError {} = (ErrorID "namespace-diff:impossible-error", err500)

instance Logging.Loggable NamespaceDiffError where
  toLog (ImpossibleError t) =
    Logging.textLog t
      & Logging.withSeverity Logging.Error

-- | The differences between two namespaces.
data DefinitionDiffs name r = DefinitionDiffs
  { -- Brand new added terms, neither the name nor definition exist in the old namespace.
    added :: Map name r,
    -- Removed terms. These names for these definitions were removed, and there are no newly
    -- added names for these definitions.
    removed :: Map name r,
    -- Updated terms. These names exist in both the old and new namespace, but the definitions
    -- assigned to them have changed.
    updated :: Map name (r {- old -}, r {- new -}),
    -- Renamed terms. These definitions exist in both the old and new namespace, but the names have
    -- changed.
    renamed :: Map r (NESet name {- old names for this ref -}, NESet name {- new names for this ref -}),
    -- New aliases. These definitions exist in both the old namespace, but have received new names
    -- in the new namespace without removing the old ones.
    newAliases :: Map r (NESet name {- Existing names for this ref -}, NESet name)
  }
  deriving stock (Eq, Show)

data DefinitionDiff r = DefinitionDiff
  { kind :: DefinitionDiffKind r,
    -- The fully qualified name of the definition we're concerned with.
    fqn :: Name
  }
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

-- | Information about a single definition which is different.
data DefinitionDiffKind r
  = Added r
  | NewAlias r (NESet Name {- existing names -})
  | Removed r
  | Updated r {- old -} r {- new -}
  | -- This definition was removed away from this location and added at the provided names.
    RenamedTo r (NESet Name)
  | -- This definition was added at this location and removed from the provided names.
    RenamedFrom r (NESet Name)
  deriving stock (Eq, Show, Ord, Functor, Foldable, Traversable)

instance (Ord r) => Semigroup (DefinitionDiffs Name r) where
  d1 <> d2 =
    DefinitionDiffs
      { added = added d1 <> added d2,
        removed = removed d1 <> removed d2,
        updated = updated d1 <> updated d2,
        renamed = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (renamed d1) (renamed d2),
        newAliases = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (newAliases d1) (newAliases d2)
      }

instance (Ord r) => Monoid (DefinitionDiffs Name r) where
  mempty =
    DefinitionDiffs
      { added = mempty,
        removed = mempty,
        updated = mempty,
        renamed = mempty,
        newAliases = mempty
      }

-- | A compressed tree of differences between two namespaces.
-- All intermediate namespaces with no differences are compressed into the keys of the
-- first child that has differences.
--
-- E.g.
--
-- If there's a change at `a.b.c` and `a.x.y`, the tree will look like:
--
-- @@
-- a
-- ├── b.c = DiffAtPath
-- └── x.y = DiffAtPath
-- @@
--
-- If there's a change at a.b.c and a.b.x, the tree will look like:
-- @@
-- a
-- └── b
--    ├── c = DiffAtPath
--    └── x = DiffAtPath
-- @@
type NamespaceTreeDiff referent reference = Cofree (Map Path) (Map NameSegment (DiffAtPath referent reference))

-- | The differences at a specific path in the namespace tree.
data DiffAtPath referent reference = DiffAtPath
  { termDiffsAtPath :: Set (DefinitionDiff referent),
    typeDiffsAtPath :: Set (DefinitionDiff reference)
  }
  deriving stock (Eq, Show)

-- | A traversal over all the referents in a `DiffAtPath`.
diffAtPathReferents_ :: (Ord referent') => Traversal (DiffAtPath referent reference) (DiffAtPath referent' reference) referent referent'
diffAtPathReferents_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . traverse) %%~ f
    & fmap \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the references in a `DiffAtPath`.
diffAtPathReferences_ :: (Ord reference') => Traversal (DiffAtPath referent reference) (DiffAtPath referent reference') reference reference'
diffAtPathReferences_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . traverse) %%~ f
    & fmap \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | Traversal over all the referents in a `NamespaceTreeDiff`.
namespaceTreeDiffReferents_ :: (Ord referent') => Traversal (NamespaceTreeDiff referent reference) (NamespaceTreeDiff referent' reference) referent referent'
namespaceTreeDiffReferents_ =
  traversed . traversed . diffAtPathReferents_

-- | Traversal over all the references in a `NamespaceTreeDiff`.
namespaceTreeDiffReferences_ :: (Ord reference') => Traversal (NamespaceTreeDiff referent reference) (NamespaceTreeDiff referent reference') reference reference'
namespaceTreeDiffReferences_ = traversed . traversed . diffAtPathReferences_
