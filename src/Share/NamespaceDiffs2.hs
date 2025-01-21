{-# LANGUAGE DataKinds #-}

-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs2
  ( computeThreeWayNamespaceDiff,
  )
where

import Control.Monad.Except
import Data.Map qualified as Map
import Share.Codebase qualified as Codebase
import Share.Names.Postgres qualified as PGNames
import Share.NamespaceDiffs.Types (NamespaceTreeDiff)
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Prelude
import U.Codebase.Referent qualified as V2
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration (Decl)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Merge (EitherWay, IncoherentDeclReason, LibdepDiffOp, Mergeblob0, Mergeblob1, ThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.Mergeblob1 qualified as Mergeblob1
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
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Nametree (Nametree)

data MergeError
  = IncoherentDecl (EitherWay IncoherentDeclReason)
  | LibFoundAtUnexpectedPath Path

-- type CodebaseM e = ReaderT CodebaseEnv (PG.Transaction e)
computeThreeWayNamespaceDiff ::
  forall e.
  TwoWay Codebase.CodebaseEnv ->
  ThreeWay BranchHashId ->
  ThreeWay NameLookupReceipt ->
  ExceptT
    MergeError
    (PG.Transaction e)
    ( Defns (Set Name) (Set Name),
      NamespaceTreeDiff V2.Referent Reference Name Name Name Name,
      Map NameSegment (LibdepDiffOp BranchHashId)
    )
computeThreeWayNamespaceDiff codebaseEnvs2 branchHashIds3 nameLookupReceipts3 = do
  -- Load a flat definitions names (no lib) for Alice/Bob/LCA
  defnsNames3 :: ThreeWay Names <-
    lift (sequence (NL.projectNamesWithoutLib <$> nameLookupReceipts3 <*> branchHashIds3))

  -- Unflatten each Names to a Nametree (leniently). Really, only the LCA is "allowed" to break the diff/merge rules of
  -- no conflicted names, but we don't enforce that here. If Alice or Bob have a conflicted name for some reason, we'll
  -- just silently pick one of the refs and move on.
  let defnsNametrees3 :: ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
      defnsNametrees3 =
        Names.lenientToNametree <$> defnsNames3

  -- Load the shallow libdeps for Alice/Bob/LCA. This can fail with "lib at unexpected path"
  libdeps3 :: ThreeWay (Map NameSegment BranchHashId) <- do
    let f :: NameLookupReceipt -> BranchHashId -> ExceptT MergeError (PG.Transaction e) (Map NameSegment BranchHashId)
        f nameLookupReceipt branchHashId = do
          mounts <- lift $ NL.listNameLookupMounts nameLookupReceipt branchHashId
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
        Merge.makeMergeblob0 defnsNametrees3 libdeps3

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
    lift (sequence (f <$> codebaseEnvs3 <*> blob0.defns))

  -- Get a names object that contains just enough names to compute the diff:
  names3 :: ThreeWay Names <-
    lift do
      -- Massage the hydrated definitions into a set of "labeled dependency" that contains the definitions themselves
      -- and their direct references.
      --
      -- FIXME: Mitchell wonders why self is necessary. Aren't direct dependency names enough?
      let labeledDeps3 :: ThreeWay (Set LabeledDependency)
          labeledDeps3 =
            Mergeblob1.hydratedDefnsLabeledDependencies <$> hydratedDefns3
      -- Get a names perspective for Alice/Bob/LCA
      namesPerspectives3 :: ThreeWay NL.NamesPerspective <-
        for branchHashIds3 \branchHashId ->
          NL.namesPerspectiveForRootAndPath branchHashId (mempty @NL.PathSegments)
      sequence (PGNames.namesForReferences <$> namesPerspectives3 <*> labeledDeps3)

  blob1 :: Mergeblob1 BranchHashId <-
    case Merge.makeMergeblob1 blob0 names3 hydratedDefns3 of
      Right blob -> pure blob
      Left err -> throwError (IncoherentDecl err)

  undefined
