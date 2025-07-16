-- | Conversions between the various types of references and referents.
-- Strongly prefer converting in batches using the plural combinators, they're much more
-- efficient than converting in a loop.
module Share.Postgres.NameLookups.Conversions
  ( references1ToPGOf,
    references2ToPGOf,
    referents1ToPGOf,
    referents2ToPGOf,
    referencesPGTo1Of,
    referencesPGTo2Of,
    referentsPGTo1UsingCTOf,
    referentsPGTo2Of,
    labeledDependencies1ToPG,
  )
where

import Control.Lens
import Share.Postgres qualified as PG
import Share.Postgres.Hashes.Queries qualified as Hashes
import Share.Postgres.IDs (ComponentHash (..))
import Share.Postgres.Refs.Types
import Share.Prelude
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Hash qualified as UHash
import Unison.LabeledDependency qualified as LD
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

references1ToPGOf :: (PG.QueryA m) => Traversal s t V1.Reference (Maybe PGReference) -> s -> m t
references1ToPGOf trav s =
  s
    -- This is safe here because we always return the same number of elements as we're given
    & asListOf trav %%~ \ref1s -> do
      let ref2s = map Cv.reference1to2 ref1s
      references2ToPGOf traversed ref2s

references2ToPGOf :: (PG.QueryA m) => Traversal s t V2.Reference (Maybe PGReference) -> s -> m t
references2ToPGOf trav s =
  s
    -- This is safe here because we always return the same number of elements as we're given
    & asListOf trav %%~ \ref2s -> do
      fmap (sequenceAOf (V2.h_)) <$> Hashes.componentHashIdsOf (traversed . V2.h_ . componentHashAsUHash_) ref2s

referents1ToPGOf :: (PG.QueryA m) => Traversal s t V1.Referent (Maybe PGReferent) -> s -> m t
referents1ToPGOf trav s =
  s
    -- This is safe here because we always return the same number of elements as we're given
    & asListOf trav %%~ \ref1s -> do
      let ref2s = map Cv.referent1to2 ref1s
      referents2ToPGOf traversed ref2s

referents2ToPGOf :: (PG.QueryA m) => Traversal s t V2.Referent (Maybe PGReferent) -> s -> m t
referents2ToPGOf trav s =
  s
    -- This is safe here because we always return the same number of elements as we're given
    & asListOf trav %%~ \ref2s -> do
      -- We traverse the hashes on both the referents and references at once, then convert them
      -- all to ComponentHashIDs in a single batch.
      fmap (sequenceAOf (V2.refs_ . V2.h_)) <$> Hashes.componentHashIdsOf (traversed . V2.refs_ . V2.h_ . componentHashAsUHash_) ref2s

referencesPGTo1Of :: (PG.QueryA m) => Traversal s t PGReference V1.Reference -> s -> m t
referencesPGTo1Of trav s =
  s
    -- This is safe here because we always return the same number of elements as we're given
    & Hashes.expectComponentHashesOf (trav . V2.h_ . uHashAsComponentHash_)

referencesPGTo2Of :: (PG.QueryA m) => Traversal s t PGReference V2.Reference -> s -> m t
referencesPGTo2Of trav s =
  s
    -- This is safe here because we always return the same number of elements as we are given
    & asListOf (trav . V2.h_) %%~ fmap coerce . Hashes.expectComponentHashesOf traversed

referentsPGTo1UsingCTOf :: (PG.QueryA m, HasCallStack) => Traversal s t (PGReferent, Maybe V2.ConstructorType) V1.Referent -> s -> m t
referentsPGTo1UsingCTOf trav s =
  s
    -- This is safe here because we always return the same number of elements as we are given
    & asListOf trav %%~ \refs -> do
      (Hashes.expectComponentHashesOf (traversed . _1 . V2.refs_ . V2.h_ . uHashAsComponentHash_) refs)
        <&> fmap \(ref', mayCT) -> (Cv.referent2to1UsingCT (fromMaybe (error "referentsPGTo1UsingCT: missing Constructor Type") mayCT) ref')

referentsPGTo2Of :: (PG.QueryA m) => Traversal s t PGReferent V2.Referent -> s -> m t
referentsPGTo2Of trav s =
  s
    -- This is safe here because we always return the same number of elements as we are given
    & asListOf (trav . V2.refs_ . V2.h_) %%~ (fmap coerce . Hashes.expectComponentHashesOf traversed)

-- | This is similar to `labeledDependencies1ToPG`, but it filters out refs for components
-- which don't exist on Share, e.g. generated accessors.
labeledDependencies1ToPG :: (PG.QueryA m) => [LD.LabeledDependency] -> m [Maybe (Either (V1.Referent, PGReferent) (V1.Reference, PGReference))]
labeledDependencies1ToPG refs =
  refs
    & map \case
      LD.TermReferent r -> Left (r, Cv.referent1to2 r)
      LD.TypeReference r -> Right (r, Cv.reference1to2 r)
    -- This is safe here because we always return the same number of elements as we're given
    -- We traverse the hashes on both the referents and references at once, then convert them
    -- all to ComponentHashIDs in a single batch.
    & asListOf (traversed . beside (_2 . V2.refs_) (_2) . V2.h_) %%~ (Hashes.componentHashIdsOf traversed . fmap ComponentHash)
    <&> fmap (sequenceOf (beside (_2 . V2.refs_) _2 . V2.h_))

-- | Helper iso for coercing between ComponentHash and UHash.Hash
uHashAsComponentHash_ :: Iso r UHash.Hash r ComponentHash
uHashAsComponentHash_ = coerced

-- | Helper iso for coercing between UHash.Hash ComponentHash
componentHashAsUHash_ :: Iso UHash.Hash r ComponentHash r
componentHashAsUHash_ = coerced
