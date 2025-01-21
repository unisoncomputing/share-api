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
    namespaceTreeDiffTermDiffs_,
    namespaceTreeDiffTypeDiffs_,
    namespaceTreeDiffRenderedTerms_,
    namespaceTreeDiffRenderedTypes_,
    namespaceTreeTermDiffKinds_,
    namespaceTreeTypeDiffKinds_,
    definitionDiffRendered_,
    definitionDiffRefs_,
    definitionDiffDiffs_,
    definitionDiffKindRefs_,
    definitionDiffKindDiffs_,
    definitionDiffKindRendered_,

    -- * Misc. helpers
    definitionDiffsToTree,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((:<))
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Servant (err404, err500)
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Unison.Codebase.Path (Path)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Util.Set qualified as Set

data NamespaceDiffError
  = ImpossibleError Text
  | MissingEntityError EntityMissing
  deriving stock (Eq, Show)

instance ToServerError NamespaceDiffError where
  toServerError = \case
    ImpossibleError {} -> (ErrorID "namespace-diff:impossible-error", err500)
    MissingEntityError (EntityMissing eId _msg) -> (eId, err404)

instance Logging.Loggable NamespaceDiffError where
  toLog = \case
    (ImpossibleError t) ->
      Logging.textLog t
        & Logging.withSeverity Logging.Error
    (MissingEntityError e) -> Logging.toLog e

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

data DefinitionDiff r rendered diff = DefinitionDiff
  { kind :: DefinitionDiffKind r rendered diff,
    -- The fully qualified name of the definition we're concerned with.
    fqn :: Name
  }
  deriving stock (Eq, Show, Ord)

definitionDiffKind_ :: Lens (DefinitionDiff r rendered diff) (DefinitionDiff r' rendered' diff') (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r' rendered' diff')
definitionDiffKind_ = lens getter setter
  where
    getter (DefinitionDiff k _) = k
    setter (DefinitionDiff _ n) k = DefinitionDiff k n

definitionDiffRefs_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r' rendered diff) r r'
definitionDiffRefs_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindRefs_ f k <*> pure n

definitionDiffDiffs_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r rendered diff') diff diff'
definitionDiffDiffs_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindDiffs_ f k <*> pure n

definitionDiffRendered_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r rendered' diff) rendered rendered'
definitionDiffRendered_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindRendered_ f k <*> pure n

-- | Information about a single definition which is different.
data DefinitionDiffKind r rendered diff
  = Added r rendered
  | NewAlias r (NESet Name {- existing names -}) rendered
  | Removed r rendered
  | Updated r {- old -} r {- new -} diff
  | -- This definition was removed away from this location and added at the provided names.
    RenamedTo r (NESet Name) rendered
  | -- This definition was added at this location and removed from the provided names.
    RenamedFrom r (NESet Name) rendered
  deriving stock (Eq, Show, Ord)

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

definitionDiffKindRefs_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r' rendered diff) r r'
definitionDiffKindRefs_ f = \case
  Added r rendered -> Added <$> f r <*> pure rendered
  NewAlias r ns rendered -> NewAlias <$> f r <*> pure ns <*> pure rendered
  Removed r rendered -> Removed <$> f r <*> pure rendered
  Updated old new diff -> Updated <$> f old <*> f new <*> pure diff
  RenamedTo r old rendered -> RenamedTo <$> f r <*> pure old <*> pure rendered
  RenamedFrom r old rendered -> RenamedFrom <$> f r <*> pure old <*> pure rendered

definitionDiffKindDiffs_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r rendered diff') diff diff'
definitionDiffKindDiffs_ f = \case
  Added r rendered -> Added r <$> pure rendered
  NewAlias r ns rendered -> NewAlias r ns <$> pure rendered
  Removed r rendered -> Removed r <$> pure rendered
  Updated old new diff -> Updated old new <$> f diff
  RenamedTo r old rendered -> RenamedTo r old <$> pure rendered
  RenamedFrom r old rendered -> RenamedFrom r old <$> pure rendered

definitionDiffKindRendered_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r rendered' diff) rendered rendered'
definitionDiffKindRendered_ f = \case
  Added r rendered -> Added r <$> f rendered
  NewAlias r ns rendered -> NewAlias r ns <$> f rendered
  Removed r rendered -> Removed r <$> f rendered
  Updated old new diff -> Updated old new <$> pure diff
  RenamedTo r old rendered -> RenamedTo r old <$> f rendered
  RenamedFrom r old rendered -> RenamedFrom r old <$> f rendered

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
type NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff = Cofree (Map Path) (Map NameSegment (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff))

-- | The differences at a specific path in the namespace tree.
data DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff = DiffAtPath
  { termDiffsAtPath :: Set (DefinitionDiff referent renderedTerm termDiff),
    typeDiffsAtPath :: Set (DefinitionDiff reference renderedType typeDiff)
  }
  deriving stock (Eq, Show)

-- | A traversal over all the referents in a `DiffAtPath`.
diffAtPathReferents_ :: (Ord referent', Ord termDiff, Ord renderedTerm) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent' reference renderedTerm renderedType termDiff typeDiff) referent referent'
diffAtPathReferents_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffRefs_) %%~ f
    & fmap \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the references in a `DiffAtPath`.
diffAtPathReferences_ :: (Ord reference', Ord typeDiff, Ord renderedType) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference' renderedTerm renderedType termDiff typeDiff) reference reference'
diffAtPathReferences_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffRefs_) %%~ f
    & fmap \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the term diffs in a `DiffAtPath`.
diffAtPathTermDiffs_ :: (Ord termDiff', Ord referent, Ord renderedTerm) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType termDiff' typeDiff) termDiff termDiff'
diffAtPathTermDiffs_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffDiffs_) %%~ f
    <&> \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the type diffs in a `DiffAtPath`.
diffAtPathTypeDiffs_ :: (Ord typeDiff', Ord reference, Ord renderedType) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff') typeDiff typeDiff'
diffAtPathTypeDiffs_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffDiffs_) %%~ f
    <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

diffAtPathTermDiffKinds_ :: (Ord renderedTerm', Ord termDiff', Ord referent') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent' reference renderedTerm' renderedType termDiff' typeDiff) (DefinitionDiffKind referent renderedTerm termDiff) (DefinitionDiffKind referent' renderedTerm' termDiff')
diffAtPathTermDiffKinds_ f (DiffAtPath terms types) = do
  newTerms <- terms & Set.traverse . definitionDiffKind_ %%~ f
  pure $ DiffAtPath newTerms types

diffAtPathTypeDiffKinds_ :: (Ord renderedType', Ord typeDiff', Ord reference') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference' renderedTerm renderedType' termDiff typeDiff') (DefinitionDiffKind reference renderedType typeDiff) (DefinitionDiffKind reference' renderedType' typeDiff')
diffAtPathTypeDiffKinds_ f (DiffAtPath terms types) = do
  newTypes <- types & Set.traverse . definitionDiffKind_ %%~ f
  pure $ DiffAtPath terms newTypes

-- | A traversal over all the rendered terms in a `DiffAtPath`.
diffAtPathRenderedTerms_ :: (Ord termDiff, Ord referent, Ord renderedTerm') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm' renderedType termDiff typeDiff) renderedTerm renderedTerm'
diffAtPathRenderedTerms_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffRendered_) %%~ f
    <&> \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the rendered types in a `DiffAtPath`.
diffAtPathRenderedTypes_ :: (Ord typeDiff, Ord reference, Ord renderedType') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType' termDiff typeDiff) renderedType renderedType'
diffAtPathRenderedTypes_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffRendered_) %%~ f
    <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | Traversal over all the referents in a `NamespaceTreeDiff`.
namespaceTreeDiffReferents_ :: (Ord referent', Ord termDiff, Ord renderedTerm) => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent' reference renderedTerm renderedType termDiff typeDiff) referent referent'
namespaceTreeDiffReferents_ =
  traversed . traversed . diffAtPathReferents_

-- | Traversal over all the references in a `NamespaceTreeDiff`.
namespaceTreeDiffReferences_ :: (Ord reference', Ord typeDiff, Ord renderedType) => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference' renderedTerm renderedType termDiff typeDiff) reference reference'
namespaceTreeDiffReferences_ = traversed . traversed . diffAtPathReferences_

namespaceTreeDiffTermDiffs_ :: (Ord termDiff', Ord referent, Ord renderedTerm) => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff' typeDiff) termDiff termDiff'
namespaceTreeDiffTermDiffs_ = traversed . traversed . diffAtPathTermDiffs_

namespaceTreeDiffTypeDiffs_ :: (Ord typeDiff', Ord reference, Ord renderedType) => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff') typeDiff typeDiff'
namespaceTreeDiffTypeDiffs_ = traversed . traversed . diffAtPathTypeDiffs_

namespaceTreeTermDiffKinds_ :: (Ord renderedTerm', Ord termDiff', Ord referent') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent' reference renderedTerm' renderedType termDiff' typeDiff) (DefinitionDiffKind referent renderedTerm termDiff) (DefinitionDiffKind referent' renderedTerm' termDiff')
namespaceTreeTermDiffKinds_ = traversed . traversed . diffAtPathTermDiffKinds_

namespaceTreeTypeDiffKinds_ :: (Ord renderedType', Ord typeDiff', Ord reference') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference' renderedTerm renderedType' termDiff typeDiff') (DefinitionDiffKind reference renderedType typeDiff) (DefinitionDiffKind reference' renderedType' typeDiff')
namespaceTreeTypeDiffKinds_ = traversed . traversed . diffAtPathTypeDiffKinds_

namespaceTreeDiffRenderedTerms_ :: (Ord termDiff, Ord referent, Ord renderedTerm') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm' renderedType termDiff typeDiff) renderedTerm renderedTerm'
namespaceTreeDiffRenderedTerms_ = traversed . traversed . diffAtPathRenderedTerms_

namespaceTreeDiffRenderedTypes_ :: (Ord typeDiff, Ord reference, Ord renderedType') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm renderedType' termDiff typeDiff) renderedType renderedType'
namespaceTreeDiffRenderedTypes_ = traversed . traversed . diffAtPathRenderedTypes_

-- | Convert a `DefinitionDiffs` into a tree of differences.
definitionDiffsToTree :: forall ref. (Ord ref) => DefinitionDiffs Name ref -> Cofree (Map NameSegment) (Map NameSegment (Set (DefinitionDiff ref Name Name)))
definitionDiffsToTree dd =
  let DefinitionDiffs {added, removed, updated, renamed, newAliases} = dd
      expandedAliases :: Map Name (Set (DefinitionDiffKind ref Name Name))
      expandedAliases =
        newAliases
          & Map.toList
          & foldMap
            ( \(r, (existingNames, newNames)) ->
                ( Foldable.toList newNames
                    <&> \newName -> Map.singleton newName (Set.singleton (NewAlias r existingNames newName))
                )
            )
          & Map.unionsWith (<>)
      expandedRenames :: Map Name (Set (DefinitionDiffKind ref Name Name))
      expandedRenames =
        renamed
          & Map.toList
          & foldMap \(r, (oldNames, newNames)) ->
            ( -- We don't currently want to track the old names in a rename, and including them messes up
              -- the path-compression for the diff tree, so we just omit them.
              -- ( Foldable.toList oldNames
              --       <&> \oldName -> Map.singleton oldName (Set.singleton (RenamedTo r newNames))
              --   )
              -- <>
              ( Foldable.toList newNames
                  <&> \newName -> Map.singleton newName (Set.singleton (RenamedFrom r oldNames newName))
              )
            )
              & Map.unionsWith (<>)
      diffTree :: Map Name (Set (DefinitionDiffKind ref Name Name))
      diffTree =
        Map.unionsWith
          (<>)
          [ (added & Map.mapWithKey \n r -> Set.singleton $ Added r n),
            expandedAliases,
            (removed & Map.mapWithKey \n r -> Set.singleton $ Removed r n),
            (updated & Map.mapWithKey \name (oldR, newR) -> Set.singleton $ Updated oldR newR name),
            expandedRenames
          ]
      includeFQNs :: Map Name (Set (DefinitionDiffKind ref Name Name)) -> Map Name (Set (DefinitionDiff ref Name Name))
      includeFQNs m = m & imap \n ds -> (ds & Set.map \d -> DefinitionDiff {kind = d, fqn = n})
   in diffTree
        & includeFQNs
        & expandNameTree

-- Unfolds a Map of names into a Cofree of paths by name segemnt.
--
-- >>> import qualified Unison.Syntax.Name as NS
-- >>> expandNameTree $ Map.fromList [(NS.unsafeParseText "a.b", "a.b"), (NS.unsafeParseText "a.c", "a.c"), (NS.unsafeParseText "x.y.z", "x.y.z")]
-- fromList [] :< fromList [(a,fromList [(b,"a.b"),(c,"a.c")] :< fromList []),(x,fromList [] :< fromList [(y,fromList [(z,"x.y.z")] :< fromList [])])]
expandNameTree :: forall a. Map Name a -> Cofree (Map NameSegment) (Map NameSegment a)
expandNameTree m =
  let (here, children) =
        m
          & Map.toList
          & fmap splitNames
          & partitionEithers
      childMap =
        children
          & Map.fromListWith Map.union
          & fmap expandNameTree
   in (Map.fromList here) Cofree.:< childMap
  where
    splitNames :: (Name, a) -> Either (NameSegment, a) (NameSegment, Map Name a)
    splitNames (n, a) =
      case Name.segments n of
        (ns :| []) -> Left (ns, a)
        (ns :| (r : rs)) -> Right (ns, Map.singleton (Name.fromSegments (r :| rs)) a)
