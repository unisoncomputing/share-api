-- | Conversions between the various types of references and referents.
-- Strongly prefer converting in batches using the plural combinators, they're much more
-- efficient than converting in a loop.
module Share.Postgres.NameLookups.Conversions
  ( reference1ToPG,
    references1ToPG,
    reference2ToPG,
    references2ToPG,
    referent1ToPG,
    referents1ToPG,
    referent2ToPG,
    referents2ToPG,
    referencePGTo1,
    referencesPGTo1,
    referencePGTo2,
    referencesPGTo2Of,
    referentPGTo1UsingCT,
    referentsPGTo1UsingCT,
    referentPGTo2,
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
import Share.Postgres.IDs (ComponentHash (..))
import Share.Postgres.NameLookups.Types
import Share.Postgres.Refs.Types
import Share.Prelude
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.LabeledDependency qualified as LD
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

reference1ToPG :: (PG.QueryM m) => V1.Reference -> m PGReference
reference1ToPG = fmap runIdentity . references1ToPG . Identity

references1ToPG :: (Traversable t, PG.QueryM m) => t V1.Reference -> m (t PGReference)
references1ToPG refs =
  refs
    & fmap Cv.reference1to2
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

reference2ToPG :: (PG.QueryM m) => V2.Reference -> m PGReference
reference2ToPG = fmap runIdentity . references2ToPG . Identity

references2ToPG :: (Traversable t, PG.QueryM m) => t V2.Reference -> m (t PGReference)
references2ToPG refs =
  refs
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

referent1ToPG :: (PG.QueryM m) => V1.Referent -> m PGReferent
referent1ToPG = fmap runIdentity . referents1ToPG . Identity

referents1ToPG :: (Traversable t, PG.QueryM m) => t V1.Referent -> m (t PGReferent)
referents1ToPG refs =
  refs
    & fmap Cv.referent1to2
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.refs_ . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

referent2ToPG :: (PG.QueryM m) => V2.Referent -> m PGReferent
referent2ToPG = fmap runIdentity . referents2ToPG . Identity

referents2ToPG :: (Traversable t, PG.QueryM m) => t V2.Referent -> m (t PGReferent)
referents2ToPG refs =
  refs
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.refs_ . V2.h_) %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

referencePGTo1 :: (PG.QueryM m) => PGReference -> m V1.Reference
referencePGTo1 = fmap runIdentity . referencesPGTo1 . Identity

referencesPGTo1 :: (PG.QueryM m, Traversable t) => t PGReference -> m (t V1.Reference)
referencesPGTo1 refs =
  refs
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (traversed . V2.h_) %%~ fmap coerce . Hashes.expectComponentHashesOf traversed
    <&> fmap Cv.reference2to1

referencePGTo2 :: (PG.QueryM m) => PGReference -> m V2.Reference
referencePGTo2 = referencesPGTo2Of id

referencesPGTo2Of :: (PG.QueryM m) => Traversal s t PGReference V2.Reference -> s -> m t
referencesPGTo2Of trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (trav . V2.h_) %%~ fmap coerce . Hashes.expectComponentHashesOf traversed

referentPGTo1UsingCT :: (PG.QueryM m, HasCallStack) => (PGReferent, Maybe V2.ConstructorType) -> m V1.Referent
referentPGTo1UsingCT = fmap runIdentity . referentsPGTo1UsingCT . Identity

referentsPGTo1UsingCT :: (PG.QueryM m, Traversable t, HasCallStack) => t (PGReferent, Maybe V2.ConstructorType) -> m (t V1.Referent)
referentsPGTo1UsingCT refs =
  refs
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (traversed . _1 . V2.refs_ . V2.h_) %%~ (fmap coerce . Hashes.expectComponentHashesOf traversed)
    <&> fmap (\(ref, mayCT) -> Cv.referent2to1UsingCT (fromMaybe (error "referentsPGTo1UsingCT: missing Constructor Type") mayCT) ref)

referentPGTo2 :: (PG.QueryM m) => PGReferent -> m V2.Referent
referentPGTo2 = referentsPGTo2Of id

referentsPGTo2Of :: (PG.QueryM m) => Traversal s t PGReferent V2.Referent -> s -> m t
referentsPGTo2Of trav s =
  s
    -- This is safe here because ensureComponentHashes always returns the same number of elements as it is given
    & unsafePartsOf (trav . V2.refs_ . V2.h_) %%~ (fmap coerce . Hashes.expectComponentHashesOf traversed)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferentsWithCT2ToPG :: (PG.QueryM m) => [NamedRef (V2.Referent, Maybe V2.ConstructorType)] -> m [NamedRef (PGReferent, Maybe V2.ConstructorType)]
namedReferentsWithCT2ToPG refs =
  refs
    &
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    unsafePartsOf (traversed . ref_ . _1 . V2.refs_ . V2.h_)
      %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferents2ToPG :: (PG.QueryM m) => [NamedRef V2.Referent] -> m [NamedRef PGReferent]
namedReferents2ToPG refs =
  refs
    &
    -- This is safe here because ensureComponentHashIdsOf always returns the same number of elements as it is given
    unsafePartsOf (traversed . ref_ . V2.refs_ . V2.h_)
      %%~ (Hashes.ensureComponentHashIdsOf traversed . fmap ComponentHash)

-- | Batch convert named refs, ensuring we also have a hash saved for each ref.
namedReferences2ToPG :: (PG.QueryM m) => [NamedRef V2.Reference] -> m [NamedRef PGReference]
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
