{-# LANGUAGE DataKinds #-}

-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs2
  ( computeThreeWayNamespaceDiff,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Set.NonEmpty qualified as NESet
import Share.Codebase qualified as Codebase
import Share.Names.Postgres qualified as PGNames
import Share.NamespaceDiffs.Types (DefinitionDiff, DefinitionDiffs (..), DiffAtPath, NamespaceDiffError (..), NamespaceTreeDiff, combineTermsAndTypes, compressNameTree, definitionDiffsToTree)
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Prelude
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration (Decl)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Merge (Mergeblob0, Mergeblob1, ThreeWay (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Mergeblob1 qualified as Mergeblob1
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Name (Name)
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

-- Defns (Set Name) (Set Name),
-- Map NameSegment (LibdepDiffOp BranchHashId)
-- )

computeThreeWayNamespaceDiff ::
  TwoWay Codebase.CodebaseEnv ->
  TwoOrThreeWay BranchHashId ->
  TwoOrThreeWay NameLookupReceipt ->
  PG.Transaction NamespaceDiffError (NamespaceTreeDiff Referent Reference Name Name Name Name)
computeThreeWayNamespaceDiff codebaseEnvs2 branchHashIds3 nameLookupReceipts3 = do
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
      ) <- do
    let hydrateTerm ::
          Codebase.CodebaseEnv ->
          TermReferenceId ->
          PG.Transaction e (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        hydrateTerm codebaseEnv ref =
          Codebase.codebaseMToTransaction codebaseEnv do
            term <- Codebase.expectTerm ref
            pure (ref, term)
        hydrateType ::
          Codebase.CodebaseEnv ->
          TypeReferenceId ->
          PG.Transaction e (TypeReferenceId, Decl Symbol Ann)
        hydrateType codebaseEnv ref =
          Codebase.codebaseMToTransaction codebaseEnv do
            type_ <- Codebase.expectTypeDeclaration ref
            pure (ref, type_)
        f ::
          Codebase.CodebaseEnv ->
          Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
          PG.Transaction
            e
            ( DefnsF
                (Map Name)
                (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
                (TypeReferenceId, Decl Symbol Ann)
            )
        f codebaseEnv =
          bitraverse
            (traverse (hydrateTerm codebaseEnv) . Map.mapMaybe Referent.toTermReferenceId . BiMultimap.range)
            (traverse (hydrateType codebaseEnv) . Map.mapMaybe Reference.toId . BiMultimap.range)

    let -- Here we assume that the LCA is in the same codebase as Alice.
        codebaseEnvs3 :: ThreeWay Codebase.CodebaseEnv
        codebaseEnvs3 =
          ThreeWay
            { alice = codebaseEnvs2.alice,
              bob = codebaseEnvs2.bob,
              lca = codebaseEnvs2.alice
            }
    sequence (f <$> codebaseEnvs3 <*> blob0.defns)

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
    namesPerspectives3 :: TwoOrThreeWay NL.NamesPerspective <-
      for branchHashIds3 \branchHashId ->
        NL.namesPerspectiveForRootAndPath branchHashId (mempty @NL.PathSegments)
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
                  HumanDiffOp'PropagatedUpdate _ -> mempty
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
  let oneUncompressedTree ::
        Cofree
          (Map NameSegment)
          (Map NameSegment (DiffAtPath Referent TypeReference Name Name Name Name))
      oneUncompressedTree =
        alignDefnsWith combineTermsAndTypes twoUncompressedTrees

  -- Compress the name tree
  let oneCompressedTree :: NamespaceTreeDiff Referent TypeReference Name Name Name Name
      oneCompressedTree =
        compressNameTree oneUncompressedTree

  pure oneCompressedTree
