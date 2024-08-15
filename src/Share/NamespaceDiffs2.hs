-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs2 (computeNamespaceDiff) where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Share.NamespaceDiffs.Types (NamespaceTreeDiff)
import Share.Postgres.IDs
import Share.Web.App (WebApp)
import U.Codebase.Referent qualified as V2
import Unison.DataDeclaration (Decl)
import Unison.Merge (EitherWay, IncoherentDeclReason, LibdepDiffOp, Mergeblob1 (Mergeblob1), ThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Ann)
import Unison.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Defns (Defns, DefnsF)
import Unison.Util.Nametree (Nametree)

makeThreeWayNametree :: ThreeWay CausalId -> WebApp (ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)))
makeThreeWayNametree = undefined

type LibDep = CausalId

makeThreeWayLibdeps :: ThreeWay CausalId -> WebApp (ThreeWay (Map NameSegment LibDep))
makeThreeWayLibdeps = undefined

makeThreeWayHydratedDefinitions ::
  WebApp
    ( ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        )
    )
makeThreeWayHydratedDefinitions = undefined

mergeblob1ToDiff :: Merge.Mergeblob1 LibDep -> WebApp (Defns (Set Name) (Set Name), NamespaceTreeDiff V2.Referent Reference, Map NameSegment (LibdepDiffOp LibDep))
mergeblob1ToDiff blob1 = do
  let Mergeblob1 {conflicts = TwoWay {bob = bobsConflicts}, diffs = TwoWay {bob = defnDiffs}, libdepsDiff} = blob1
  let setOfConflicts = bimap Map.keysSet Map.keysSet bobsConflicts
  let namespaceDiff = _ defnDiffs
  pure (setOfConflicts, namespaceDiff, libdepsDiff)

computeNamespaceDiff ::
  CausalId ->
  CausalId ->
  WebApp ((Either (EitherWay IncoherentDeclReason) (Defns (Set Name) (Set Name), NamespaceTreeDiff V2.Referent Reference, Map NameSegment (LibdepDiffOp LibDep))))
computeNamespaceDiff diffFrom diffTo = do
  -- We use the diffFrom as both the LCA and alice, we're just going to ignore the
  -- alice <-> bob diff in this case.
  let threeWay = (ThreeWay {lca = diffFrom, alice = diffFrom, bob = diffTo})
  computeMergeblob1 threeWay >>= \case
    Left err -> pure $ Left err
    Right blob1 -> Right <$> mergeblob1ToDiff blob1

computeMergeblob1 :: ThreeWay CausalId -> WebApp (Either (EitherWay IncoherentDeclReason) (Merge.Mergeblob1 LibDep))
computeMergeblob1 causals = do
  nametrees <- makeThreeWayNametree causals
  libdeps <- makeThreeWayLibdeps causals
  let blob0 = Merge.makeMergeblob0 nametrees libdeps
  threeWayDefns <- makeThreeWayHydratedDefinitions
  pure $ Merge.makeMergeblob1 blob0 threeWayDefns
