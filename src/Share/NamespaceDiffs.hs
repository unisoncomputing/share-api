-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs
  ( NamespaceTreeDiff,
    DiffAtPath (..),
    NamespaceDiffError (..),
    DefinitionDiff (..),
    DefinitionDiffKind (..),
    diffTreeNamespaces,
    namespaceTreeDiffReferences_,
    namespaceTreeDiffReferents_,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((:<))
import Data.Align (Semialign (..))
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Share.Postgres qualified as PG
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Conversions qualified as Cv
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NamespaceDiffs qualified as ND
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Servant (err500)
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Rel
import Unison.Util.Set qualified as Set

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

instance Ord r => Semigroup (DefinitionDiffs Name r) where
  d1 <> d2 =
    DefinitionDiffs
      { added = added d1 <> added d2,
        removed = removed d1 <> removed d2,
        updated = updated d1 <> updated d2,
        renamed = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (renamed d1) (renamed d2),
        newAliases = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (newAliases d1) (newAliases d2)
      }

instance Ord r => Monoid (DefinitionDiffs Name r) where
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
diffAtPathReferents_ :: Ord referent' => Traversal (DiffAtPath referent reference) (DiffAtPath referent' reference) referent referent'
diffAtPathReferents_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . traverse) %%~ f
    & fmap \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the references in a `DiffAtPath`.
diffAtPathReferences_ :: Ord reference' => Traversal (DiffAtPath referent reference) (DiffAtPath referent reference') reference reference'
diffAtPathReferences_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . traverse) %%~ f
    & fmap \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | Traversal over all the referents in a `NamespaceTreeDiff`.
namespaceTreeDiffReferents_ :: Ord referent' => Traversal (NamespaceTreeDiff referent reference) (NamespaceTreeDiff referent' reference) referent referent'
namespaceTreeDiffReferents_ =
  traversed . traversed . diffAtPathReferents_

-- | Traversal over all the references in a `NamespaceTreeDiff`.
namespaceTreeDiffReferences_ :: Ord reference' => Traversal (NamespaceTreeDiff referent reference) (NamespaceTreeDiff referent reference') reference reference'
namespaceTreeDiffReferences_ = traversed . traversed . diffAtPathReferences_

data NamespaceDiffError = ImpossibleError Text
  deriving stock (Eq, Show)

instance ToServerError NamespaceDiffError where
  toServerError ImpossibleError {} = (ErrorID "namespace-diff:impossible-error", err500)

instance Logging.Loggable NamespaceDiffError where
  toLog (ImpossibleError t) =
    Logging.textLog t
      & Logging.withSeverity Logging.Error

-- | Compute the tree of differences between two namespace hashes.
-- Note: This ignores all dependencies in the lib namespace.
diffTreeNamespaces :: (BranchHashId, NameLookupReceipt) -> (BranchHashId, NameLookupReceipt) -> (PG.Transaction e (Either NamespaceDiffError (NamespaceTreeDiff V2.Referent V2.Reference)))
diffTreeNamespaces (oldBHId, oldNLReceipt) (newBHId, newNLReceipt) = do
  (oldTerms, newTerms) <- ND.getRelevantTermsForDiff oldNLReceipt oldBHId newBHId
  (oldTypes, newTypes) <- ND.getRelevantTypesForDiff newNLReceipt oldBHId newBHId
  case diffTreeNamespacesHelper (oldTerms, newTerms) (oldTypes, newTypes) of
    Left e -> pure $ Left e
    Right nd ->
      Right
        <$> ( Cv.referentsPGTo2Of (namespaceTreeDiffReferents_) nd
                >>= Cv.referencesPGTo2Of (namespaceTreeDiffReferences_)
            )

-- | Compute the tree of differences between two namespaces.
-- This is the core logic for computing the differences between two namespaces.
diffTreeNamespacesHelper ::
  forall referent reference.
  (Ord referent, Ord reference) =>
  (Relation Name referent, Relation Name referent) ->
  (Relation Name reference, Relation Name reference) ->
  Either NamespaceDiffError (NamespaceTreeDiff referent reference)
diffTreeNamespacesHelper (oldTerms, newTerms) (oldTypes, newTypes) = do
  termTree <- computeDefinitionDiff oldTerms newTerms <&> definitionDiffsToTree
  typeTree <- computeDefinitionDiff oldTypes newTypes <&> definitionDiffsToTree
  let compressed =
        alignWith combineTermsAndTypes termTree typeTree
          & compressNameTree
  pure compressed
  where
    combineTermsAndTypes :: These (Map NameSegment (Set (DefinitionDiff referent))) (Map NameSegment (Set (DefinitionDiff reference))) -> Map NameSegment (DiffAtPath referent reference)
    combineTermsAndTypes = \case
      This termsMap -> termsMap <&> \termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
      That typesMap -> typesMap <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
      These trms typs -> alignWith combineNode trms typs
    combineNode :: These (Set (DefinitionDiff referent)) (Set (DefinitionDiff reference)) -> DiffAtPath referent reference
    combineNode = \case
      This termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
      That typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
      These termDiffsAtPath typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

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

-- | Collapse all links which have only a single child into a path.
-- I.e. the resulting tree will not contain nodes that have only a single namespace child with no diffs.
--
-- Note that the final node will always have a map of name segments containing the final
-- segment of the name, since that's considered the name and not part of the path.
--
-- >>> import qualified Unison.Syntax.Name as NS
-- >>> let expanded = expandNameTree $ Map.fromList [(NS.unsafeParseText "a.b", "a.b"), (NS.unsafeParseText "a.c", "a.c"), (NS.unsafeParseText "x.y.z", "x.y.z")]
-- >>> compressNameTree expanded
-- fromList [] :< fromList [(a,fromList [(b,"a.b"),(c,"a.c")] :< fromList []),(x.y,fromList [(z,"x.y.z")] :< fromList [])]
compressNameTree :: Cofree (Map NameSegment) (Map NameSegment a) -> Cofree (Map Path) (Map NameSegment a)
compressNameTree (diffs Cofree.:< children) =
  let compressedChildren =
        children
          & fmap compressNameTree
          & Map.toList
          & fmap
            ( \(ns, child) ->
                case child of
                  (childDiffs Cofree.:< nestedChildren)
                    | null childDiffs,
                      [(k, v)] <- Map.toList nestedChildren ->
                        (ns Path.:< k, v)
                    | otherwise ->
                        (Path.singleton ns, child)
            )
          & Map.fromList
   in diffs Cofree.:< compressedChildren

-- | Compute changes between two unstructured Name relations, determining what has changed and how
-- it should be interpreted so it's meaningful to the user.
computeDefinitionDiff ::
  Ord ref =>
  Relation Name ref {- Relevant definitions from old namespace -} ->
  Relation Name ref {- Relevant definitions from new namespace -} ->
  Either NamespaceDiffError (DefinitionDiffs Name ref)
computeDefinitionDiff old new =
  (Rel.dom old <> Rel.dom new)
    & Monoid.foldMapM
      ( \name ->
          case (NESet.nonEmptySet (Rel.lookupDom name old), NESet.nonEmptySet (Rel.lookupDom name new)) of
            (Nothing, Nothing) -> Left $ ImpossibleError "Name in diff doesn't exist in either old or new namespace"
            -- Doesn't exist in the old namespace, it's a new addition or a new alias
            (Nothing, Just refs) -> do
              -- There shouldn't be multiple refs for the same name, but this wasn't true for the old
              -- update process, so we'll just take the first ref.
              let ref = NESet.findMin refs
              case Set.toList (Rel.lookupRan ref old) of
                -- No old names for this ref, so it's a new addition not an alias
                [] -> Right $ mempty {added = Map.singleton name ref}
                -- There are old names for this ref, but not old refs for this name, so it's
                -- either a new alias or a rename.
                --
                -- If at least one old name for this ref no longer exists, we treat it like a
                -- rename.
                (n : ns) -> do
                  let existingNames = NESet.fromList (n NEList.:| ns)
                  case NESet.nonEmptySet (Rel.lookupRan ref new) of
                    Nothing -> Left $ ImpossibleError "Expected to find at least one name for ref in new namespace, since we found the ref by the name."
                    Just allNewNames ->
                      case NESet.nonEmptySet (NESet.difference allNewNames existingNames) of
                        Nothing -> Left $ ImpossibleError "Expected to find at least one new name for ref in new namespace, since we found the ref by the name."
                        Just newNamesWithoutOldNames ->
                          case NESet.nonEmptySet (NESet.difference existingNames allNewNames) of
                            -- If all the old names still exist in the new namespace, it's a new alias.
                            Nothing -> Right $ mempty {newAliases = Map.singleton ref (existingNames, newNamesWithoutOldNames)}
                            -- Otherwise, treat it as a rename.
                            Just namesWhichDisappeared -> Right $ mempty {renamed = Map.singleton ref (namesWhichDisappeared, newNamesWithoutOldNames)}

            -- Doesn't exist in the new namespace,
            -- so it's a removal or rename.
            (Just refs, Nothing) -> do
              refs
                & Monoid.foldMapM
                  ( \ref -> do
                      case Set.toList (Rel.lookupRan ref new) of
                        -- No names for this ref, it was removed.
                        [] -> Right $ mempty {removed = Map.singleton name ref}
                        newNames ->
                          newNames
                            & Monoid.foldMapM (\newName -> Right $ mempty {renamed = Map.singleton ref (NESet.singleton name, NESet.singleton newName)})
                  )
            -- Exists in both old and new namespaces, so it's an update
            (Just oldRefs, Just newRefs) -> do
              -- There should only be one ref for each name in the old and new namespaces,
              -- but this wasn't true for the old update process, so we'll just take the
              -- first ref.
              let (oldRef, newRef) = (NESet.findMin oldRefs, NESet.findMin newRefs)
              -- It's possible it's an unchanged ref which we should just ignore.
              if oldRef == newRef
                then Right mempty
                else Right $ mempty {updated = Map.singleton name (NESet.findMin oldRefs, NESet.findMin newRefs)}
      )

-- | Convert a `DefinitionDiffs` into a tree of differences.
definitionDiffsToTree :: forall ref. Ord ref => DefinitionDiffs Name ref -> Cofree (Map NameSegment) (Map NameSegment (Set (DefinitionDiff ref)))
definitionDiffsToTree dd =
  let DefinitionDiffs {added, removed, updated, renamed, newAliases} = dd
      expandedAliases :: Map Name (Set (DefinitionDiffKind ref))
      expandedAliases =
        newAliases
          & Map.toList
          & foldMap
            ( \(r, (existingNames, newNames)) ->
                ( Foldable.toList newNames
                    <&> \newName -> Map.singleton newName (Set.singleton (NewAlias r existingNames))
                )
            )
          & Map.unionsWith (<>)
      expandedRenames :: Map Name (Set (DefinitionDiffKind ref))
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
                  <&> \newName -> Map.singleton newName (Set.singleton (RenamedFrom r oldNames))
              )
            )
              & Map.unionsWith (<>)
      diffTree :: Map Name (Set (DefinitionDiffKind ref))
      diffTree =
        Map.unionsWith
          (<>)
          [ (added <&> Set.singleton . Added),
            expandedAliases,
            (removed <&> Set.singleton . Removed),
            (updated <&> \(oldR, newR) -> Set.singleton $ Updated oldR newR),
            expandedRenames
          ]
      includeFQNs :: Map Name (Set (DefinitionDiffKind ref)) -> Map Name (Set (DefinitionDiff ref))
      includeFQNs m = m & imap \n ds -> (ds & Set.map \d -> DefinitionDiff {kind = d, fqn = n})
   in diffTree
        & includeFQNs
        & expandNameTree
