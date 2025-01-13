{-# LANGUAGE ApplicativeDo #-}

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
import Data.Set.NonEmpty qualified as NESet
import Share.NamespaceDiffs.Types
import Share.Postgres qualified as PG
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Conversions qualified as Cv
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NamespaceDiffs qualified as ND
import Share.Prelude
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

-- | Compute the tree of differences between two namespace hashes.
-- Note: This ignores all dependencies in the lib namespace.
diffTreeNamespaces :: (BranchHashId, NameLookupReceipt) -> (BranchHashId, NameLookupReceipt) -> (PG.Transaction e (Either NamespaceDiffError (NamespaceTreeDiff V2.Referent V2.Reference Name Name Name Name)))
diffTreeNamespaces (oldBHId, oldNLReceipt) (newBHId, newNLReceipt) = do
  ((oldTerms, newTerms), (oldTypes, newTypes)) <- PG.pipelined do
    terms <- ND.getRelevantTermsForDiff oldNLReceipt oldBHId newBHId
    types <- ND.getRelevantTypesForDiff newNLReceipt oldBHId newBHId
    pure (terms, types)
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
  Either NamespaceDiffError (NamespaceTreeDiff referent reference Name Name Name Name)
diffTreeNamespacesHelper (oldTerms, newTerms) (oldTypes, newTypes) = do
  termTree <- computeDefinitionDiff oldTerms newTerms <&> definitionDiffsToTree
  typeTree <- computeDefinitionDiff oldTypes newTypes <&> definitionDiffsToTree
  let compressed =
        alignWith combineTermsAndTypes termTree typeTree
          & compressNameTree
  pure compressed
  where
    combineTermsAndTypes :: These (Map NameSegment (Set (DefinitionDiff referent Name Name))) (Map NameSegment (Set (DefinitionDiff reference Name Name))) -> Map NameSegment (DiffAtPath referent reference Name Name Name Name)
    combineTermsAndTypes = \case
      This termsMap -> termsMap <&> \termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
      That typesMap -> typesMap <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
      These trms typs -> alignWith combineNode trms typs
    combineNode :: These (Set (DefinitionDiff referent Name Name)) (Set (DefinitionDiff reference Name Name)) -> DiffAtPath referent reference Name Name Name Name
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
  (Ord ref) =>
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
