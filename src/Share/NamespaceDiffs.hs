{-# LANGUAGE DeriveAnyClass #-}

-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs
  ( NamespaceDiffResult (..),
    NamespaceAndLibdepsDiff,
    GNamespaceAndLibdepsDiff (..),
    NamespaceTreeDiff,
    GNamespaceTreeDiff,
    DiffAtPath (..),
    NamespaceDiffError (..),
    DefinitionDiff (..),
    DefinitionDiffKind (..),
    computeThreeWayNamespaceDiff,
    compressNameTree,
    namespaceTreeDiffReferences_,
    namespaceTreeDiffReferents_,
    namespaceTreeDiffTermDiffs_,
    witherNamespaceTreeDiffTermDiffs,
    namespaceTreeDiffTypeDiffs_,
    namespaceTreeDiffRenderedTerms_,
    namespaceTreeDiffRenderedTypes_,
    namespaceTreeTermDiffKinds_,
    witherNamespaceTreeTermDiffKinds,
    namespaceTreeTypeDiffKinds_,
    namespaceAndLibdepsDiffDefns_,
    namespaceAndLibdepsDiffLibdeps_,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((.=), (:<))
import Control.Monad.Except
import Data.Align (Semialign (..))
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Share.Codebase qualified as Codebase
import Share.Names.Postgres qualified as PGNames
import Share.NamespaceDiffs.Types
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Queries qualified as DefnsQ
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Postgres.NamesPerspective.Ops qualified as NPOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Prelude
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.DataDeclaration (Decl)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Merge (Mergeblob0, Mergeblob1, ThreeWay (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Mergeblob1 qualified as Mergeblob1
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF3, alignDefnsWith)
import Unison.Util.Nametree (Nametree (..))

-- | Convert a `DefinitionDiffs` into a tree of differences.
definitionDiffsToTree ::
  forall ref.
  (Ord ref) =>
  DefinitionDiffs Name ref ->
  GNamespaceTreeOf NameSegment (Set (DefinitionDiff ref Name Name))
definitionDiffsToTree dd =
  let DefinitionDiffs {added, removed, updated, propagated, renamed, newAliases} = dd
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
            (propagated & Map.mapWithKey \name (oldR, newR) -> Set.singleton $ Propagated oldR newR name),
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
-- fromList [] :< fromList [(NameSegment {toUnescapedText = "a"},fromList [(NameSegment {toUnescapedText = "b"},"a.b"),(NameSegment {toUnescapedText = "c"},"a.c")] :< fromList []),(NameSegment {toUnescapedText = "x"},fromList [] :< fromList [(NameSegment {toUnescapedText = "y"},fromList [(NameSegment {toUnescapedText = "z"},"x.y.z")] :< fromList [])])]
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
   in Map.fromList here Cofree.:< childMap
  where
    splitNames :: (Name, a) -> Either (NameSegment, a) (NameSegment, Map Name a)
    splitNames (n, a) =
      case Name.segments n of
        (ns :| []) -> Left (ns, a)
        (ns :| (r : rs)) -> Right (ns, Map.singleton (Name.fromSegments (r :| rs)) a)

combineTermsAndTypes ::
  forall f reference referent.
  (Semialign f, Ord reference, Ord referent) =>
  These
    (f (Set (DefinitionDiff referent Name Name)))
    (f (Set (DefinitionDiff reference Name Name))) ->
  f (DiffAtPath referent reference Name Name Name Name)
combineTermsAndTypes = \case
  This termsMap -> termsMap <&> \termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
  That typesMap -> typesMap <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
  These trms typs -> alignWith combineNode trms typs
  where
    combineNode ::
      These
        (Set (DefinitionDiff referent Name Name))
        (Set (DefinitionDiff reference Name Name)) ->
      DiffAtPath referent reference Name Name Name Name
    combineNode = \case
      This termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
      That typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
      These termDiffsAtPath typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

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
                        (Path.prefix (Path.singleton ns) (Path.Relative k), v)
                    | otherwise ->
                        (Path.singleton ns, child)
            )
          & Map.fromList
   in diffs Cofree.:< compressedChildren

computeThreeWayNamespaceDiff ::
  TwoWay Codebase.CodebaseEnv ->
  TwoOrThreeWay BranchHashId ->
  TwoOrThreeWay NameLookupReceipt ->
  PG.Transaction NamespaceDiffError (GNamespaceAndLibdepsDiff NameSegment Referent Reference Name Name Name Name BranchHashId)
computeThreeWayNamespaceDiff codebaseEnvs2 branchHashIds3 nameLookupReceipts3 = PG.transactionSpan "computeThreeWayNamespaceDiff" mempty $ do
  -- Load a flat definitions names (no lib) for Alice/Bob/LCA
  defnsNames3 :: TwoOrThreeWay Names <-
    sequence (NL.projectNamesWithoutLib <$> nameLookupReceipts3 <*> branchHashIds3)

  -- Unflatten each Names to a Nametree (leniently). Really, only the LCA is "allowed" to break the diff/merge rules of
  -- no conflicted names, but we don't enforce that here. If Alice or Bob have a conflicted name for some reason, we'll
  -- just silently pick one of the refs and move on.
  let defnsNametrees3 :: TwoOrThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
      defnsNametrees3 =
        Names.lenientToNametree <$> defnsNames3

  -- Load the shallow libdeps for Alice/Bob/LCA. This can fail with "lib at unexpected path"
  libdeps3 :: TwoOrThreeWay (Map NameSegment BranchHashId) <- do
    let f :: NameLookupReceipt -> BranchHashId -> PG.Transaction NamespaceDiffError (Map NameSegment BranchHashId)
        f nameLookupReceipt branchHashId = do
          mounts <- NL.listNameLookupMounts nameLookupReceipt branchHashId
          libDepsList <-
            for mounts \(NL.PathSegments path, libBhId) -> do
              case NameSegment.unsafeParseText <$> path of
                [NameSegment.LibSegment, dep] -> pure (dep, libBhId)
                p -> throwError $ LibFoundAtUnexpectedPath (Path.fromList p)
          pure $ Map.fromList libDepsList
    sequence (f <$> nameLookupReceipts3 <*> branchHashIds3)

  -- Make that 0th mergeblob
  let blob0 :: Mergeblob0 BranchHashId
      blob0 =
        Merge.makeMergeblob0
          ThreeWay
            { alice = defnsNametrees3.alice,
              bob = defnsNametrees3.bob,
              lca = fromMaybe Nametree {value = Defns Map.empty Map.empty, children = Map.empty} defnsNametrees3.lca
            }
          ThreeWay
            { alice = libdeps3.alice,
              bob = libdeps3.bob,
              lca = fromMaybe Map.empty libdeps3.lca
            }

  -- Hydrate defns in Alice/Bob/LCA
  hydratedDefns3 ::
    ThreeWay
      ( DefnsF
          (Map Name)
          (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
          (TypeReferenceId, Decl Symbol Ann)
      ) <- PG.transactionSpan "hydratedDefns3" mempty do
    let hydrateTermsOf ::
          Codebase.CodebaseEnv ->
          Traversal s t TermReferenceId (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
          s ->
          PG.Transaction e t
        hydrateTermsOf codebase trav s = PG.transactionSpan "hydrateTerms" mempty do
          s
            & asListOf trav %%~ \refs -> do
              v2Terms <- DefnsQ.expectTermsByRefIdsOf codebase traversed refs
              let v2TermsWithRef = zip refs v2Terms
              let refHashes = v2TermsWithRef <&> \(refId, (term, typ)) -> (refId, ((Reference.idToHash refId), term, typ))
              Codebase.convertTerms2to1Of (traversed . _2) refHashes
        hydrateTypesOf ::
          Codebase.CodebaseEnv ->
          Traversal s t TypeReferenceId (TypeReferenceId, Decl Symbol Ann) ->
          s ->
          PG.Transaction e t
        hydrateTypesOf codebase trav s = PG.transactionSpan "hydrateTypes" mempty do
          s
            & asListOf trav %%~ \typeReferenceIds -> do
              typeIdsWithComponents <- zip typeReferenceIds <$> DefnsQ.expectTypeComponentElementsAndTypeIdsOf codebase traversed typeReferenceIds
              DefnsQ.loadDeclByTypeComponentElementAndTypeIdsOf (traversed . _2) typeIdsWithComponents
                <&> fmap \(refId, v2Decl) ->
                  let v1Decl = Cv.decl2to1 (Reference.idToHash refId) v2Decl
                   in (refId, v1Decl)
        hydrateDefns ::
          Codebase.CodebaseEnv ->
          Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
          PG.Transaction
            e
            ( DefnsF
                (Map Name)
                (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
                (TypeReferenceId, Decl Symbol Ann)
            )
        hydrateDefns codebase (Defns {terms, types}) = PG.transactionSpan "hydrateDefns" mempty do
          let termReferenceIds = Map.mapMaybe Referent.toTermReferenceId (BiMultimap.range terms)
          hydratedTerms <- hydrateTermsOf codebase traversed termReferenceIds
          let typeReferenceIds = Map.mapMaybe Reference.toId (BiMultimap.range types)
          hydratedTypes <- hydrateTypesOf codebase traversed typeReferenceIds
          pure
            Defns
              { terms = hydratedTerms,
                types = hydratedTypes
              }

    let -- Here we assume that the LCA is in the same codebase as Alice.
        codebaseEnvs3 :: ThreeWay Codebase.CodebaseEnv
        codebaseEnvs3 =
          ThreeWay
            { alice = codebaseEnvs2.alice,
              bob = codebaseEnvs2.bob,
              lca = codebaseEnvs2.alice
            }
    sequence (hydrateDefns <$> codebaseEnvs3 <*> blob0.defns)

  -- Get a names object that contains just enough names to compute the diff:
  names3 :: TwoOrThreeWay Names <- do
    -- Massage the hydrated definitions into a set of "labeled dependency" that contains the definitions themselves
    -- and their direct references.
    --
    -- FIXME: Mitchell wonders why self is necessary. Aren't direct dependency names enough?
    let labeledDeps3 :: ThreeWay (Set LabeledDependency)
        labeledDeps3 =
          Mergeblob1.hydratedDefnsLabeledDependencies <$> hydratedDefns3
    -- Get a names perspective for Alice/Bob/LCA
    namesPerspectives3 :: TwoOrThreeWay (NamesPerspective m) <-
      for branchHashIds3 \branchHashId -> NPOps.namesPerspectiveForRoot branchHashId
    sequence (PGNames.namesForReferences <$> namesPerspectives3 <*> ThreeWay.toTwoOrThreeWay labeledDeps3)

  blob1 :: Mergeblob1 BranchHashId <- do
    let names3' = ThreeWay {alice = names3.alice, bob = names3.bob, lca = fromMaybe (mempty @Names) names3.lca}
    case Merge.makeMergeblob1 blob0 names3' hydratedDefns3 of
      Right blob -> pure blob
      Left err -> throwError (IncoherentDecl err)

  -- Boilerplate conversion: make a "DefinitionDiffs" from the info in a "Mergeblob1".
  --
  -- (Mitchell says: I think Share and UCM should share a type here – perhaps DefinitionDiffs should be pushed down? Or
  -- is it just isomorphic to something that already exists in UCM?)
  --
  -- We start focusing only on Bob here, the contributor, even though Alice could have a diff as well of course (since
  -- the LCA is arbitrarily behind Alice).

  let definitionDiffs :: DefnsF (DefinitionDiffs Name) Referent TypeReference
      definitionDiffs =
        let f :: forall ref. (Ord ref) => Map Name (HumanDiffOp ref) -> DefinitionDiffs Name ref
            f =
              Map.toList >>> foldMap \(name, op) ->
                case op of
                  HumanDiffOp'Add ref -> mempty {added = Map.singleton name ref}
                  HumanDiffOp'Delete ref -> mempty {removed = Map.singleton name ref}
                  HumanDiffOp'Update refs -> mempty {updated = Map.singleton name (refs.old, refs.new)}
                  HumanDiffOp'PropagatedUpdate refs -> mempty {propagated = Map.singleton name (refs.old, refs.new)}
                  HumanDiffOp'AliasOf ref names ->
                    mempty {newAliases = Map.singleton ref (names, NESet.singleton name)}
                  HumanDiffOp'RenamedFrom ref names ->
                    mempty {renamed = Map.singleton ref (names, NESet.singleton name)}
                  HumanDiffOp'RenamedTo ref names ->
                    mempty {renamed = Map.singleton ref (NESet.singleton name, names)}
         in bimap f f blob1.humanDiffsFromLCA.bob

  -- Convert definition diffs to two uncompressed trees of diffs (one for terms, one for types)
  let twoUncompressedTrees ::
        DefnsF3
          (Cofree (Map NameSegment))
          (Map NameSegment)
          Set
          (DefinitionDiff Referent Name Name)
          (DefinitionDiff TypeReference Name Name)
      twoUncompressedTrees =
        bimap definitionDiffsToTree definitionDiffsToTree definitionDiffs

  -- Align terms and types trees into one tree (still uncompressed)
  let oneUncompressedTree :: GNamespaceTreeDiff NameSegment Referent TypeReference Name Name Name Name
      oneUncompressedTree =
        alignDefnsWith combineTermsAndTypes twoUncompressedTrees

  pure
    NamespaceAndLibdepsDiff
      { defns = oneUncompressedTree,
        libdeps = blob1.libdepsDiffs.bob
      }
