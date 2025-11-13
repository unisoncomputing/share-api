module Share.Postgres.Sync.Conversions where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Share.Prelude
import U.Codebase.Branch qualified as V2
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull
import U.Codebase.Sqlite.Patch.TermEdit qualified as PatchFullTermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as PatchFullTypeEdit
import U.Codebase.TermEdit qualified as V2TermEdit
import U.Codebase.TypeEdit qualified as V2TypeEdit
import Unison.Hash (Hash)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Util.Map qualified as Map

branchV2ToBF ::
  forall m.
  (Monad m) =>
  V2.Branch m ->
  -- text ComponentHash PatchHash (BranchHash, CausalHash)
  m (BranchFull.Branch' Text Hash Hash (Hash, Hash))
branchV2ToBF (V2.Branch {terms, types, patches, children}) = do
  fullTerms <- convertTerms terms
  fullTypes <- convertTypes types
  let fullPatches = convertPatches patches
  let fullChildren = convertChildren children
  pure $ BranchFull.Branch {terms = fullTerms, types = fullTypes, patches = fullPatches, children = fullChildren}
  where
    convertTerms :: Map NameSegment (Map V2.Referent (m V2.MdValues)) -> m (Map Text (Map (BranchFull.Referent'' Text Hash) (BranchFull.MetadataSetFormat' Text Hash)))
    convertTerms m = do
      Map.mapKeys NameSegment.toUnescapedText m
        & traversed . traversed
          %%~ \mdM -> do
            V2.MdValues md <- mdM
            pure . BranchFull.Inline $ md
    convertTypes :: Map NameSegment (Map V2.Reference (m V2.MdValues)) -> m (Map Text (Map V2.TypeReference (BranchFull.MetadataSetFormat' Text Hash)))
    convertTypes m = do
      Map.mapKeys (coerce @NameSegment @Text) m
        & traversed . traversed
          %%~ \mdM -> do
            V2.MdValues md <- mdM
            pure . BranchFull.Inline $ md

    convertPatches :: Map NameSegment (PatchHash, m V2.Patch) -> (Map Text Hash)
    convertPatches m = Map.bimap (coerce @NameSegment @Text) (unPatchHash . fst) m

    convertChildren :: Map NameSegment (V2.CausalBranch m) -> Map Text (Hash, Hash)
    convertChildren =
      Map.bimap
        NameSegment.toUnescapedText
        ((unBranchHash . Causal.valueHash) &&& (unCausalHash . Causal.causalHash))

patchV2ToPF :: V2.Patch -> PatchFull.Patch' Text Hash Hash
patchV2ToPF V2.Patch {termEdits, typeEdits} =
  let termEdits' = termEdits <&> Set.map cvTermEdit
      typeEdits' = typeEdits <&> Set.map cvTypeEdit
   in PatchFull.Patch {termEdits = termEdits', typeEdits = typeEdits'}
  where
    cvTermEdit = \case
      V2TermEdit.Replace r t -> PatchFullTermEdit.Replace r (cvTyping t)
      V2TermEdit.Deprecate -> PatchFullTermEdit.Deprecate
    cvTypeEdit = \case
      V2TypeEdit.Replace r -> PatchFullTypeEdit.Replace r
      V2TypeEdit.Deprecate -> PatchFullTypeEdit.Deprecate
    cvTyping = \case
      V2TermEdit.Same -> PatchFullTermEdit.Same
      V2TermEdit.Subtype -> PatchFullTermEdit.Subtype
      V2TermEdit.Different -> PatchFullTermEdit.Different

-- data Patch' t h o = Patch
--   { termEdits :: Map (Referent'' t h) (Set (TermEdit' t o)),
--     typeEdits :: Map (Reference' t h) (Set (TypeEdit' t o))
--   }
-- data Patch = Patch
--   { termEdits :: !(Map Referent (Set TermEdit)),
--     typeEdits :: !(Map Reference (Set TypeEdit))
--   }
