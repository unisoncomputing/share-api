-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs2
  ( computeNamespaceDiff,
    mergeCausals,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Zip qualified as Zip
import Share.Codebase (CodebaseEnv, CodebaseM)
import Share.Codebase qualified as Codebase
import Share.Names.Postgres qualified as PGNames
import Share.NamespaceDiffs.Types (NamespaceTreeDiff)
import Share.Postgres qualified as PG
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Prelude
import U.Codebase.Referent qualified as V2
import Unison.Builtin qualified as Builtin
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (DataDeclaration, Decl, EffectDeclaration)
import Unison.LabeledDependency qualified as LD
import Unison.Merge (EitherWay, IncoherentDeclReason, LibdepDiffOp, Mergeblob1 (Mergeblob1), ThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.Mergeblob1 qualified as Mergeblob1
import Unison.Merge.Mergeblob2 qualified as Mergeblob2
import Unison.Merge.Mergeblob3 qualified as Mergeblob3
import Unison.Merge.Mergeblob4 qualified as Mergeblob4
import Unison.Merge.Mergeblob5 qualified as Mergeblob5
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Nametree (Nametree)

data MergeError
  = IncoherentDecl (EitherWay IncoherentDeclReason)
  | MergeBlob2Error Mergeblob2.Mergeblob2Error
  | -- | This indicates a round-trip error on Share, since the user isn't manually editing the file.
    ParseErr (Parser.Err Symbol)
  | TypeCheckErr (Seq (Result.Note Symbol Ann))
  | LibFoundAtUnexpectedPath Path

makeThreeWayNametree :: (PG.QueryM m) => ThreeWay (BranchHashId, NameLookupReceipt) -> m (ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)))
makeThreeWayNametree roots3 = do
  for roots3 \(bhId, nlReceipt) -> do
    names <- NL.projectNamesWithoutLib nlReceipt bhId
    pure $ Names.lenientToNametree names

type LibDep = BranchHashId

makeThreeWayLibdeps :: (PG.QueryM m) => ThreeWay (BranchHashId, NameLookupReceipt) -> m (Either MergeError (ThreeWay (Map NameSegment LibDep)))
makeThreeWayLibdeps roots3 = runExceptT do
  for roots3 \(bhId, nlReceipt) -> do
    mounts <- lift $ NL.listNameLookupMounts nlReceipt bhId
    libDepsList <- for mounts \(NL.PathSegments path, libBhId) -> do
      case NameSegment.unsafeParseText <$> path of
        [NameSegment.LibSegment, dep] -> pure (dep, libBhId)
        p -> do
          throwError $ LibFoundAtUnexpectedPath (Path.fromList p)
    pure $ Map.fromList libDepsList

makeThreeWayHydratedDefinitions ::
  m
    ( ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        )
    )
makeThreeWayHydratedDefinitions = undefined

computeNamespaceDiff ::
  (PG.QueryM m) =>
  CausalId ->
  CausalId ->
  ExceptT MergeError m (Defns (Set Name) (Set Name), NamespaceTreeDiff V2.Referent Reference, Map NameSegment (LibdepDiffOp LibDep))
computeNamespaceDiff diffFrom diffTo = do
  -- We use the diffFrom as both the LCA and alice, we're just going to ignore the
  -- alice <-> bob diff in this case.
  let threeWay = (ThreeWay {lca = diffFrom, alice = diffFrom, bob = diffTo})
  computeMergeblob1 threeWay <&> \(_names, blob1) -> mergeblob1ToDiff blob1
  where
    mergeblob1ToDiff :: Merge.Mergeblob1 LibDep -> (Defns (Set Name) (Set Name), NamespaceTreeDiff V2.Referent Reference, Map NameSegment (LibdepDiffOp LibDep))
    mergeblob1ToDiff blob1 =
      let Mergeblob1 {conflicts = TwoWay {bob = bobsConflicts}, diffsFromLCA = TwoWay {bob = defnDiffs}, libdepsDiff} = blob1
          setOfConflicts = bimap Map.keysSet Map.keysSet bobsConflicts
          namespaceDiff = error "unimplemented" defnDiffs
       in (setOfConflicts, namespaceDiff, libdepsDiff)

computeMergeblob1 :: (PG.QueryM m) => ThreeWay CausalId -> ExceptT MergeError m ((ThreeWay Names, Merge.Mergeblob1 LibDep))
computeMergeblob1 causals3 = do
  branchHashIds3 <- lift $ HashQ.expectNamespaceIdsByCausalIdsOf traversed causals3
  projectRoots3 <- for branchHashIds3 \bhId -> do
    nlr <- lift $ NL.ensureNameLookupForBranchId bhId
    pure (bhId, nlr)
  nametrees3 <- lift $ makeThreeWayNametree projectRoots3
  libdeps3 <- ExceptT $ makeThreeWayLibdeps projectRoots3
  let blob0 = Merge.makeMergeblob0 nametrees3 libdeps3
  hydratedDefns3 <- makeThreeWayHydratedDefinitions
  let labeledDeps3 = Mergeblob1.hydratedDefnsLabeledDependencies <$> hydratedDefns3
  namesPerspectives3 <- lift $ for branchHashIds3 \bhId -> NL.namesPerspectiveForRootAndPath bhId mempty
  names3 <- lift $ sequenceA $ Zip.zipWith PGNames.namesForReferences namesPerspectives3 labeledDeps3
  blob <- except . mapLeft IncoherentDecl $ Merge.makeMergeblob1 blob0 names3 hydratedDefns3
  pure (names3, blob)

-- Normally computing a type lookup is more involved (e.g. including types transitively
-- because of pattern-matching, etc.), but in this case we know it ONLY needs
-- to contain types for things which are referenced in the original hydrated definitions.
typeLookupFromHydratedDefs :: (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) -> CodebaseM e (TL.TypeLookup Symbol Ann)
typeLookupFromHydratedDefs hydratedDefns@(Defns {terms, types}) = do
  let hydratedTypeOfTerms :: (Map TermReference (Type Symbol Ann))
      hydratedTypeOfTerms = terms & foldMap \(refId, (_trm, typ)) -> Map.singleton (Reference.DerivedId refId) typ
      hydratedDataDecls :: Map TypeReference (DataDeclaration Symbol Ann)
      hydratedEffectDecls :: Map TypeReference (EffectDeclaration Symbol Ann)
      (hydratedDataDecls, hydratedEffectDecls) =
        types & foldMap \(refId, decl) -> case decl of
          Right dd -> (Map.singleton (Reference.DerivedId refId) dd, mempty)
          Left ed -> (mempty, Map.singleton (Reference.DerivedId refId) ed)
  let hydratedTypeLookup = TL.TypeLookup {typeOfTerms = hydratedTypeOfTerms, dataDecls = hydratedDataDecls, effectDecls = hydratedEffectDecls}
  let knownLabeledDependencies =
        (Set.map LD.termRef (Map.keysSet hydratedTypeOfTerms))
          <> (Set.map LD.typeRef (Map.keysSet hydratedDataDecls))
          <> (Set.map LD.typeRef (Map.keysSet hydratedEffectDecls))
  let additionalNeededRefs =
        Mergeblob1.hydratedDefnsLabeledDependencies hydratedDefns
          & Set.filter (`Set.notMember` knownLabeledDependencies)
  additionalTypeLookup <- do
    -- TODO: This is extremely inefficient :'(
    -- Can definitely speed this up with one or more of the following:
    -- - Batch load
    -- - Don't reload the same decl multiple times (e.g. once for each constructor)
    -- - Pipelining?
    -- - Rework how we store term types so we don't need to hydrate the whole term to
    -- get just the type.
    (typeOfTerms, typeOfData, typeOfEffects) <-
      additionalNeededRefs & foldMapM \case
        LD.TermReference ref -> do
          typ <- Codebase.expectTypeOfTerm ref
          pure $ (Map.singleton ref typ, mempty, mempty)
        LD.ConReference (ConstructorReference ref conId) _conType -> do
          typ <- Codebase.expectTypeOfConstructor ref conId
          pure $ (Map.singleton ref typ, mempty, mempty)
        LD.TypeReference ref -> do
          case ref of
            -- Builtins are included separately
            Reference.Builtin {} -> pure mempty
            Reference.DerivedId refId -> do
              decl <- Codebase.expectTypeDeclaration refId
              pure $ case decl of
                Right dd -> (mempty, Map.singleton ref dd, mempty)
                Left ed -> (mempty, mempty, Map.singleton ref ed)
    pure $ TL.TypeLookup {typeOfTerms, dataDecls = typeOfData, effectDecls = typeOfEffects}
  pure $ Builtin.typeLookup <> hydratedTypeLookup <> additionalTypeLookup

-- | Find the references for every dependent on a core dependency which is within
-- alice/bob. These definitions will be loaded into the Unison File to be re-parsed.
coreDependencyTransitiveDependents :: (DefnsF Set TermReference TypeReference) -> m (DefnsF Set TermReferenceId TypeReferenceId)
coreDependencyTransitiveDependents = undefined

causalFromMergeBlob5 :: Mergeblob5.Mergeblob5 -> m CausalId
causalFromMergeBlob5 = undefined

mergeCausals :: ThreeWay CausalId -> ThreeWay CodebaseEnv -> PG.Transaction e (Either MergeError CausalId)
mergeCausals causals3 codebases3 = runExceptT do
  (names3, mergeBlob1) <- computeMergeblob1 causals3
  mergeBlob2 <- except . mapLeft MergeBlob2Error $ Mergeblob2.makeMergeblob2 mergeBlob1

  transitiveDependents2 <- for mergeBlob2.coreDependencies coreDependencyTransitiveDependents
  -- These names are garbage, but just need to have a unique name for every reference in
  -- scope so we can round-trip through a file, no user should ever see them.
  let combinedNames =
        (prefixNames "alice" names3.alice)
          `Names.unionLeftName` (prefixNames "bob" names3.bob)
  let mergeBlob3 = Mergeblob3.makeMergeblob3 mergeBlob2 transitiveDependents2 combinedNames (TwoWay {alice = "alice", bob = "bob"})
  mergeBlob4 <- except . mapLeft ParseErr $ Mergeblob4.makeMergeblob4 mergeBlob3
  let mkTypeLookup codebase defns = Codebase.codebaseMToTransaction codebase $ typeLookupFromHydratedDefs defns
  -- Lookup all the types we need in the respective codebases.
  typeLookup <- lift . sequenceA $ Zip.zipWith mkTypeLookup (ThreeWay.forgetLca codebases3) (ThreeWay.forgetLca mergeBlob1.hydratedDefns)
  mergeBlob5 <- except . mapLeft TypeCheckErr $ Mergeblob5.makeMergeblob5 mergeBlob4 (fold typeLookup)
  lift $ causalFromMergeBlob5 mergeBlob5
  where
    prefixNames :: Text -> Names -> Names
    prefixNames prefix = Names.map (Name.cons (NameSegment.unsafeParseText prefix))
