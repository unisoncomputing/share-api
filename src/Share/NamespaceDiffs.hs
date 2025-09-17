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
    makeNamespaceDiffTree,
    compressNameTree,
    namespaceTreeDiffReferences_,
    namespaceTreeDiffReferents_,
    namespaceTreeDiffTermDiffs_,
    mapMaybeNamespaceTreeDiffTermDiffs,
    witherNamespaceTreeDiffTermDiffs,
    namespaceTreeDiffTypeDiffs_,
    namespaceTreeDiffRenderedTerms_,
    namespaceTreeDiffRenderedTypes_,
    namespaceTreeTermDiffKinds_,
    mapMaybeNamespaceTreeTermDiffKinds,
    witherNamespaceTreeTermDiffKinds,
    namespaceTreeTypeDiffKinds_,
    namespaceAndLibdepsDiffDefns_,
    namespaceAndLibdepsDiffLibdeps_,
    definitionDiffKindRendered_,
    definitionDiffKindRenderedOldNew_,
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
import Share.Codebase.CodeCache qualified as CodeCache
import Share.Codebase.Types (CodeCache)
import Share.Names.Postgres qualified as PGNames
import Share.NamespaceDiffs.Types
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Queries qualified as DefnsQ
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Queries (NameSearchScope (TransitiveDependencies))
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Postgres.NamesPerspective.Ops qualified as NPOps
import Share.Prelude
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Merge qualified as Merge
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.Synhashed (Synhashed)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoOrThreeWay qualified as TwoOrThreeWay
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Reference (TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView qualified as UnconflictedLocalDefnsView
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF3, alignDefnsWith)
import Unison.Util.Defns qualified as Defns

-- | Convert a `DefinitionDiffs` into a tree of differences.
definitionDiffsToTree ::
  forall ref.
  (Ord ref) =>
  DefinitionDiffs Name ref ->
  GNamespaceTreeOf NameSegment (Set (DefinitionDiff ref Name Name))
definitionDiffsToTree diff =
  let expandedAliases :: Map Name (Set (DefinitionDiffKind ref Name Name))
      expandedAliases =
        diff.newAliases
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
        diff.renamed
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
          Set.union
          [ (diff.added & Map.mapWithKey \n r -> Set.singleton $ Added r n),
            expandedAliases,
            (diff.removed & Map.mapWithKey \n r -> Set.singleton $ Removed r n),
            (diff.updated & Map.mapWithKey \name (oldR, newR) -> Set.singleton $ Updated oldR newR name),
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
  Merge.TwoWay Codebase.CodebaseEnv ->
  CodeCache alice ->
  CodeCache bob ->
  Merge.TwoOrThreeWay BranchHashId ->
  Merge.TwoOrThreeWay NameLookupReceipt ->
  PG.Transaction NamespaceDiffError (Merge.Diffblob BranchHashId)
computeThreeWayNamespaceDiff codebaseEnvs2 aliceCodeCache bobCodeCache branchHashIds nameLookupReceipts = PG.transactionSpan "computeThreeWayNamespaceDiff" mempty do
  -- Load the shallow libdeps for Alice/Bob/LCA. This can fail with "lib at unexpected path"
  libdeps :: Merge.ThreeWay (Map NameSegment BranchHashId) <- do
    PG.transactionSpan "load libdeps" mempty do
      let f :: NameLookupReceipt -> BranchHashId -> PG.Transaction NamespaceDiffError (Map NameSegment BranchHashId)
          f nameLookupReceipt branchHashId = do
            mounts <- NL.listNameLookupMounts nameLookupReceipt branchHashId
            libDepsList <-
              for mounts \(NL.PathSegments path, libBhId) -> do
                case NameSegment.unsafeParseText <$> path of
                  [NameSegment.LibSegment, dep] -> pure (dep, libBhId)
                  p -> throwError $ LibFoundAtUnexpectedPath (Path.fromList p)
            pure $ Map.fromList libDepsList
      TwoOrThreeWay.toThreeWay Map.empty <$> sequence (f <$> nameLookupReceipts <*> branchHashIds)

  -- Load all local definition names (outside lib) for Alice/Bob/LCA.
  --
  -- FIXME In a normal diff+merge, we require that the local names are unconflicted: each name may only refer to one
  -- thing. However, we currently don't represent that in the `NamespaceDiffError` type. We should, but for now, we
  -- instead just ignore conflicted names, if they exist.
  defnsList <-
    PG.transactionSpan "load definitions names" mempty do
      sequence (NL.projectNamesWithoutLib <$> nameLookupReceipts <*> branchHashIds)
  let defns =
        let f = foldr (uncurry Map.insert) Map.empty
         in TwoOrThreeWay.toThreeWay
              UnconflictedLocalDefnsView.empty
              (UnconflictedLocalDefnsView.fromDefns . bimap f f <$> defnsList)

  -- Load decl name lookups for Alice/Bob/LCA. This can fail with "incoherent decl".
  declNameLookups <- do
    numConstructors <-
      PG.transactionSpan "load constructor counts" mempty do
        TwoOrThreeWay.toThreeWay Map.empty
          <$> sequence (NL.projectConstructorCountsWithoutLib <$> nameLookupReceipts <*> branchHashIds)
    declNameLookups <-
      PG.transactionSpan "check decl coherency" mempty do
        sequence $
          ( \v c e ->
              checkDeclCoherency v.nametree c
                & mapLeft (IncoherentDecl . e)
                & liftEither
          )
            <$> ThreeWay.forgetLca defns
            <*> ThreeWay.forgetLca numConstructors
            <*> Merge.TwoWay EitherWay.Alice EitherWay.Bob
    let lcaDeclNameLookup =
          lenientCheckDeclCoherency defns.lca.nametree numConstructors.lca
    pure (TwoWay.gtoThreeWay lcaDeclNameLookup declNameLookups)

  let hydrate ::
        Merge.ThreeWay (DefnsF Set TermReferenceId TypeReferenceId) ->
        PG.Transaction
          e
          ( Defns
              (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
              (Map TypeReferenceId (Decl Symbol Ann))
          )
      hydrate Merge.ThreeWay {lca = lcaDefns, alice = aliceDefns, bob = bobDefns} = do
        -- We assume LCA and Alice come from the same codebase, so hydrate them together.
        let lcaAndAliceDefns = lcaDefns <> aliceDefns
        lcaAndAliceHydratedDefns <- hydrateDefns "hydrate alice & lca definitions" codebaseEnvs2.alice aliceCodeCache lcaAndAliceDefns

        -- Only bother hydrating Bob defns that we haven't already found in Alice's codebase.
        let bobDefnsNotInAlice = Defns.zipDefnsWith Set.difference Set.difference bobDefns lcaAndAliceDefns
        bobHydratedDefns <- hydrateDefns "hydrate bob definitions" codebaseEnvs2.bob bobCodeCache bobDefnsNotInAlice

        pure (lcaAndAliceHydratedDefns <> bobHydratedDefns)

  let loadNames :: Merge.ThreeWay (Set LabeledDependency) -> PG.Transaction NamespaceDiffError (Merge.ThreeWay Names)
      loadNames dependencies = do
        perspectives <-
          PG.transactionSpan "load names perspectives" mempty do
            traverse NPOps.namesPerspectiveForRoot branchHashIds
        names <-
          PG.transactionSpan "load names" mempty do
            sequence (PGNames.namesForReferences TransitiveDependencies <$> perspectives <*> ThreeWay.toTwoOrThreeWay dependencies)
        pure (TwoOrThreeWay.toThreeWay Names.empty names)

  Merge.makeDiffblob
    Merge.DiffblobLog
      { logDefns = \_ -> pure (),
        logDiff = \_ -> pure (),
        logDiffsFromLCA = \_ -> pure (),
        logNarrowedDefns = \_ -> pure (),
        logSynhashedNarrowedDefns = \_ -> pure ()
      }
    hydrate
    loadNames
    defns
    libdeps
    declNameLookups

hydrateDefns ::
  Text ->
  Codebase.CodebaseEnv ->
  CodeCache scope ->
  DefnsF Set TermReferenceId TypeReferenceId ->
  PG.Transaction
    e
    ( Defns
        (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
        (Map TypeReferenceId (Decl Symbol Ann))
    )
hydrateDefns description codebase codeCache Defns {terms, types}
  | Set.null terms && Set.null types = pure (Defns Map.empty Map.empty)
  | otherwise =
      PG.transactionSpan description mempty do
        hydratedTerms <-
          if Set.null terms
            then pure Map.empty
            else do
              hydratedTerms <- Map.fromList <$> hydrateTermsOf codebase traversed (Set.toList terms)
              CodeCache.cacheTermAndTypes codeCache hydratedTerms
              pure hydratedTerms
        hydratedTypes <-
          if Set.null types
            then pure Map.empty
            else do
              hydratedTypes <- Map.fromList <$> hydrateTypesOf codebase traversed (Set.toList types)
              CodeCache.cacheDecls codeCache hydratedTypes
              pure hydratedTypes
        pure Defns {terms = hydratedTerms, types = hydratedTypes}

hydrateTermsOf ::
  Codebase.CodebaseEnv ->
  Traversal s t TermReferenceId (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) ->
  s ->
  PG.Transaction e t
hydrateTermsOf codebase trav =
  asListOf trav %%~ \refs -> do
    v2Terms <- DefnsQ.expectTermsByRefIdsOf codebase traversed refs
    let v2TermsWithRef = zip refs v2Terms
    let refHashes = v2TermsWithRef <&> \(refId, (term, typ)) -> (refId, (Reference.idToHash refId, term, typ))
    Codebase.convertTerms2to1Of (traversed . _2) refHashes

hydrateTypesOf ::
  Codebase.CodebaseEnv ->
  Traversal s t TypeReferenceId (TypeReferenceId, Decl Symbol Ann) ->
  s ->
  PG.Transaction e t
hydrateTypesOf codebase trav =
  asListOf trav %%~ \typeReferenceIds -> do
    typeIdsWithComponents <- zip typeReferenceIds <$> DefnsQ.expectTypeComponentElementsAndTypeIdsOf codebase traversed typeReferenceIds
    DefnsQ.loadDeclByTypeComponentElementAndTypeIdsOf (traversed . _2) typeIdsWithComponents
      <&> fmap \(refId, v2Decl) ->
        let v1Decl = Cv.decl2to1 (Reference.idToHash refId) v2Decl
         in (refId, v1Decl)

-- Boilerplate conversion: make a "DefinitionDiffs" from the info in a "Mergeblob1".
--
-- We start focusing only on Bob here, the contributor, even though Alice could have a diff as well of course (since
-- the LCA is arbitrarily behind Alice).
makeNamespaceDiffTree ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF3 (Map Name) Merge.DiffOp Synhashed Referent TypeReference ->
  DefnsF (Map Name) (Merge.Updated Referent) (Merge.Updated TypeReference) ->
  Defns Merge.SimpleRenames Merge.SimpleRenames ->
  GNamespaceTreeDiff NameSegment Referent TypeReference Name Name Name Name
makeNamespaceDiffTree lcaDefns diffFromLca propagatedUpdates simpleRenames =
  ( Defns.zipDefnsWith3 f f lcaDefns simpleRenames diffFromLca
      <> bimap g g propagatedUpdates
  )
    -- Convert definition diffs to two uncompressed trees of diffs (one for terms, one for types)
    & bimap definitionDiffsToTree definitionDiffsToTree
    -- Align terms and types trees into one tree (still uncompressed)
    & alignDefnsWith combineTermsAndTypes
  where
    f :: (Ord ref) => BiMultimap ref Name -> Merge.SimpleRenames -> Map Name (Merge.DiffOp (Synhashed ref)) -> DefinitionDiffs Name ref
    f lca renames =
      Map.toList >>> foldMap \(name, op) ->
        case op of
          Merge.DiffOp'Add ref ->
            case Map.lookup name renames.backwards of
              Nothing ->
                case NESet.nonEmptySet (BiMultimap.lookupDom ref.value lca) of
                  Nothing -> mempty {added = Map.singleton name ref.value}
                  Just oldNames -> mempty {newAliases = Map.singleton ref.value (oldNames, NESet.singleton name)}
              Just oldName -> mempty {renamed = Map.singleton ref.value (NESet.singleton oldName, NESet.singleton name)}
          Merge.DiffOp'Delete ref ->
            case Map.lookup name renames.forwards of
              Nothing -> mempty {removed = Map.singleton name ref.value}
              -- we include the rename when handling the add side
              Just _newName -> mempty
          Merge.DiffOp'Update refs -> mempty {updated = Map.singleton name (Updated.toPair (Updated.map (.value) refs))}

    g :: (Ord ref) => Map Name (Merge.Updated ref) -> DefinitionDiffs Name ref
    g propagatedUpdates =
      mempty {propagated = Map.map Updated.toPair propagatedUpdates}
