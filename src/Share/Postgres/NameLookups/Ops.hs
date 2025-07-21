module Share.Postgres.NameLookups.Ops
  ( fuzzySearchDefinitions,
    termNamesForRefsWithinNamespaceOf,
    typeNamesForRefsWithinNamespaceOf,
    termRefsForExactNamesOf,
    typeRefsForExactNamesOf,
    checkBranchHashNameLookupExists,
    deleteNameLookupsExceptFor,
    Q.ensureNameLookupForBranchId,
    Q.projectTermsWithinRoot,
    Q.projectTypesWithinRoot,
    Q.listNameLookupMounts,
    projectNamesWithoutLib,
    Q.namesPerspectiveForRootAndPath,
    Q.namesPerspectiveForRoot,
    Q.relocateNamesToMountsOf,
    Q.relocateReversedNamesToMountsOf,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursor
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Queries (ShouldSuffixify)
import Share.Postgres.NameLookups.Queries qualified as NameLookupQ
import Share.Postgres.NameLookups.Queries qualified as Q
import Share.Postgres.NameLookups.Types
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Postgres.Refs.Types
import Share.Prelude
import Share.Utils.Lens (asListOfDeduped)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (ConstructorType, Referent)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

-- | Search for term or type names which contain the provided list of segments in order.
-- Search is case insensitive.
fuzzySearchDefinitions ::
  (PG.QueryM m) =>
  Bool ->
  NamesPerspective m ->
  -- | Will return at most n terms and n types; i.e. max number of results is 2n
  Int ->
  NonEmpty Text ->
  Text ->
  m ([(Q.FuzzySearchScore, NameLookups.NamedRef (Referent, Maybe ConstructorType))], [(Q.FuzzySearchScore, NamedRef Reference)])
fuzzySearchDefinitions includeDependencies namesPerspective limit querySegments lastQuerySegment = do
  (pgTermNames, pgTypeNames) <- do
    let relativePerspective = mempty
    pgTermNames <-
      Q.fuzzySearchTerms namesPerspective includeDependencies (into @Int64 limit) relativePerspective querySegments lastQuerySegment
    pgTypeNames <-
      Q.fuzzySearchTypes namesPerspective includeDependencies (into @Int64 limit) relativePerspective querySegments lastQuerySegment
    pure (pgTermNames, pgTypeNames)
  PG.pipelined $ do
    termNames <- pgTermNames & CV.referentsPGTo2Of (traversed . _2 . traversed . _1)
    typeNames <- pgTypeNames & CV.referencesPGTo2Of (traversed . _2 . traversed)
    pure (termNames, typeNames)

-- | Get the list of (fqn, suffixified) names for a given Referent.
-- If 'shouldSuffixify' is 'NoSuffixify', the suffixified name will be the same as the fqn.
termNamesForRefsWithinNamespaceOf :: (PG.QueryM m) => NamesPerspective m -> Maybe ReversedName -> ShouldSuffixify -> Traversal s t PGReferent [(ReversedName, ReversedName)] -> s -> m t
termNamesForRefsWithinNamespaceOf namesPerspective maySuffix shouldSuffixify trav s = do
  s
    & asListOf trav %%~ \refs -> do
      NameLookupQ.termNamesForRefsWithinNamespaceOf namesPerspective maySuffix shouldSuffixify traversed refs
        <&> (fmap . fmap) \(NameWithSuffix {reversedName, suffixifiedName}) -> (reversedName, suffixifiedName)

-- | Get the list of (fqn, suffixified) names for a given Reference.
-- If 'shouldSuffixify' is 'NoSuffixify', the suffixified name will be the same as the fqn.
typeNamesForRefsWithinNamespaceOf :: (PG.QueryM m) => NamesPerspective m -> Maybe ReversedName -> ShouldSuffixify -> Traversal s t PGReference [(ReversedName, ReversedName)] -> s -> m t
typeNamesForRefsWithinNamespaceOf namesPerspective maySuffix shouldSuffixify trav s = do
  s
    & asListOf trav %%~ \refs -> do
      NameLookupQ.typeNamesForRefsWithinNamespaceOf namesPerspective maySuffix shouldSuffixify traversed refs
        <&> (fmap . fmap) \(NameWithSuffix {reversedName, suffixifiedName}) -> (reversedName, suffixifiedName)

termRefsForExactNamesOf :: (PG.QueryM m) => NamesPerspective m -> Traversal s t ReversedName [NamedRef V1.Referent] -> s -> m t
termRefsForExactNamesOf namesPerspective trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      NameLookupQ.termRefsForExactNamesOf namesPerspective traversed names
        >>= CV.referentsPGTo1UsingCTOf (traversed . traversed . traversed)

typeRefsForExactNamesOf :: (PG.QueryM m) => NamesPerspective m -> Traversal s t ReversedName [NamedRef V1.Reference] -> s -> m t
typeRefsForExactNamesOf namesPerspective trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      NameLookupQ.typeRefsForExactNamesOf namesPerspective traversed names
        >>= CV.referencesPGTo1Of (traversed . traversed . traversed)

-- | Check whether we've already got an index for a given branch hash.
checkBranchHashNameLookupExists :: (PG.QueryM m) => BranchHash -> m Bool
checkBranchHashNameLookupExists bh = do
  bhId <- HashQ.ensureBranchHashId bh
  Q.checkBranchHashNameLookupExists bhId

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: Set BranchHash -> PG.Transaction e ()
deleteNameLookupsExceptFor reachable = do
  bhIds <- for (Set.toList reachable) HashQ.ensureBranchHashId
  Q.deleteNameLookupsExceptFor bhIds

-- | Build a 'Names' for all definitions within the given root, without any dependencies.
-- Note: This loads everything into memory at once, so avoid this and prefer streaming when possible.
projectNamesWithoutLib :: (QueryM m) => NameLookupReceipt -> BranchHashId -> m Names
projectNamesWithoutLib !nlr rootBranchHashId = do
  termNamesCursor <- Q.projectTermsWithinRootV1 nlr rootBranchHashId
  allTerms <- Cursor.foldBatched termNamesCursor 1000 (pure . toList)
  typesCursor <- (Q.projectTypesWithinRoot nlr rootBranchHashId)
  allTypes <- Cursor.foldBatched typesCursor 1000 (pure . toList)
  pure $ Names.fromTermsAndTypes allTerms allTypes
