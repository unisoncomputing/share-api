-- | Conversions between the various types of references and referents.
-- Strongly prefer converting in batches using the plural combinators, they're much more
-- efficient than converting in a loop.
module Share.Postgres.NameLookups.Conversions
  ( references1ToPGOf,
    references2ToPG,
    referents1ToPGOf,
    referents2ToPG,
    referencesPGTo1Of,
    referencesPGTo2Of,
    referentsPGTo1UsingCTOf,
    referentsPGTo2Of,
    namedReferentsWithCT2ToPG,
    namedReferents2ToPG,
    namedReferences2ToPG,
    labeledDependencies1ToPG,
    labeledDependencies1ToValidPGRefs,
  )
where

import Control.Lens
import Share.Postgres qualified as PG
import Share.Postgres.Hashes.Queries qualified as Hashes
import Share.Postgres.IDs (ComponentHash (..), ComponentHashId)
import Share.Postgres.NameLookups.Types
import Share.Postgres.Refs.Types
import Share.Prelude
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Hash qualified as UHash
import Unison.LabeledDependency qualified as LD
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

references1ToPGOf :: (PG.QueryA m) => Traversal s t V1.Reference PGReference -> s -> m t
references1ToPGOf trav s =
  s
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf trav %%~ \ref1s -> do
      let ref2s = map Cv.reference1to2 ref1s
      Hashes.ensureComponentHashIdsOf (traversed . V2.h_ . componentHashAsUHash_) ref2s

references2ToPG :: (Traversable t, PG.QueryA m) => t V2.Reference -> m (t PGReference)
references2ToPG refs =
  refs
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

referents1ToPGOf :: (PG.QueryA m) => Traversal s t V1.Referent PGReferent -> s -> m t
referents1ToPGOf trav s =
  s
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf trav %%~ \ref1s -> do
      let ref2s = map Cv.referent1to2 ref1s
      -- We traverse the hashes on both the referents and references at once, then convert them
      -- all to ComponentHashIDs in a single batch.
      Hashes.ensureComponentHashIdsOf (traversed . V2.refs_ . V2.h_ . componentHashAsUHash_) ref2s

referents2ToPG :: (Traversable t, PG.QueryA m) => t V2.Referent -> m (t PGReferent)
referents2ToPG refs =
  refs
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.refs_ . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

referencesPGTo1Of :: (PG.QueryA m) => Traversal s t PGReference V1.Reference -> s -> m t
referencesPGTo1Of trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & Hashes.expectComponentHashesOf (trav . V2.h_ . uHashAsComponentHash_)

referencesPGTo2Of :: (PG.QueryA m) => Traversal s t PGReference V2.Reference -> s -> m t
referencesPGTo2Of trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (trav . V2.h_) %%~ fmap coerce . Hashes.expectComponentHashesOf traversed

referentsPGTo1UsingCTOf :: (PG.QueryA m, HasCallStack) => Traversal s t (PGReferent, Maybe V2.ConstructorType) V1.Referent -> s -> m t
referentsPGTo1UsingCTOf trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf trav %%~ \refs -> do
      (Hashes.expectComponentHashesOf (traversed . _1 . V2.refs_ . V2.h_ . uHashAsComponentHash_) refs)
        <&> fmap \(ref', mayCT) -> (Cv.referent2to1UsingCT (fromMaybe (error "referentsPGTo1UsingCT: missing Constructor Type") mayCT) ref')

referentsPGTo2Of :: (PG.QueryA m) => Traversal s t PGReferent V2.Referent -> s -> m t
referentsPGTo2Of trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (trav . V2.refs_ . V2.h_) %%~ (fmap coerce . Hashes.expectComponentHashesOf traversed)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferentsWithCT2ToPG :: (PG.QueryA m) => [NamedRef (V2.Referent, Maybe V2.ConstructorType)] -> m [NamedRef (PGReferent, Maybe V2.ConstructorType)]
namedReferentsWithCT2ToPG refs =
  refs
    &
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    unsafePartsOf (traversed . ref_ . _1 . V2.refs_ . V2.h_)
      %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferents2ToPG :: (PG.QueryA m) => [NamedRef V2.Referent] -> m [NamedRef PGReferent]
namedReferents2ToPG refs =
  refs
    &
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    unsafePartsOf (traversed . ref_ . V2.refs_ . V2.h_)
      %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferences2ToPG :: (PG.QueryA m) => [NamedRef V2.Reference] -> m [NamedRef PGReference]
namedReferences2ToPG refs =
  refs
    &
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    unsafePartsOf (traversed . ref_ . V2.h_)
      %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

labeledDependencies1ToPG :: (PG.QueryA m) => [LD.LabeledDependency] -> m [Either (V1.Referent, PGReferent) (V1.Reference, PGReference)]
labeledDependencies1ToPG refs =
  refs
    & map \case
      LD.TermReferent r -> Left (r, Cv.referent1to2 r)
      LD.TypeReference r -> Right (r, Cv.reference1to2 r)
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    -- We traverse the hashes on both the referents and references at once, then convert them
    -- all to ComponentHashIDs in a single batch.
    & unsafePartsOf (traversed . beside (_2 . V2.refs_) (_2) . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

-- | This is similar to `labeledDependencies1ToPG`, but it filters out refs for components
-- which don't exist on Share, e.g. generated accessors.
labeledDependencies1ToValidPGRefs :: (PG.QueryA m) => [LD.LabeledDependency] -> m [Either (V1.Referent, PGReferent) (V1.Reference, PGReference)]
labeledDependencies1ToValidPGRefs refs =
  refs
    & map \case
      LD.TermReferent r -> Left (r, Cv.referent1to2 r)
      LD.TypeReference r -> Right (r, Cv.reference1to2 r)
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    -- We traverse the hashes on both the referents and references at once, then convert them
    -- all to ComponentHashIDs in a single batch.
    & unsafePartsOf (traversed . beside (_2 . V2.refs_) (_2) . V2.h_) %%~ (Hashes.componentHashIdsOf traversed . fmap ComponentHash)
    <&> fmap (sequenceOf (beside (_2 . V2.refs_) _2 . V2.h_))
    <&> catMaybes

-- | Helper iso for coercing between ComponentHash and UHash.Hash
uHashAsComponentHash_ :: Iso ComponentHashId UHash.Hash ComponentHashId ComponentHash
uHashAsComponentHash_ = coerced

-- | Helper iso for coercing between UHash.Hash ComponentHash
componentHashAsUHash_ :: Iso UHash.Hash ComponentHashId ComponentHash ComponentHashId
componentHashAsUHash_ = coerced
