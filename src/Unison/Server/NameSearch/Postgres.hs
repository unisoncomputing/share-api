module Unison.Server.NameSearch.Postgres
  ( NameSearch (..),
    nameSearchForPerspective,

    -- * Searches are also exported A'la carte.
    termRefsByHQNamesOf,
    typeRefsByHQNamesOf,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Ops as NLOps
import Share.Postgres.NameLookups.Queries (ShouldSuffixify (NoSuffixify))
import Share.Postgres.NameLookups.Types
import Share.Postgres.NamesPerspective.Types (NamesPerspective, perspectiveCurrentMountPathPrefix)
import Share.Prelude
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.NamesWithHistory (SearchType (..))
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1Reference
import Unison.Referent qualified as V1Referent
import Unison.Server.NameSearch (NameSearch (..), Search (..))
import Unison.Server.SearchResult qualified as SR
import Unison.ShortHash qualified as V1ShortHash

nameSearchForPerspective :: forall m. (PG.QueryM m) => NamesPerspective m -> NameSearch m
nameSearchForPerspective namesPerspective =
  NameSearch {typeSearch, termSearch}
  where
    -- Some searches will provide a fully-qualified name, so we need to strip off the
    -- mount-path before we search or it will fail to find anything.
    stripMountPathPrefix :: Name -> Name
    stripMountPathPrefix name = Name.tryStripReversedPrefix name (reverse . coerce $ perspectiveCurrentMountPathPrefix namesPerspective)
    typeSearch =
      Search
        { lookupNames = lookupNamesForTypes,
          lookupRelativeHQRefs' = \searchType hqname ->
            case searchType of
              ExactName -> typeRefsByHQNamesOf namesPerspective id . fmap stripMountPathPrefix $ hqname
              -- We can implement this, but it's not currently used anywhere on share.
              IncludeSuffixes -> error "Suffix search not yet implemented on Share",
          makeResult = \hqname r names -> pure $ SR.typeResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReference
        }
    termSearch =
      Search
        { lookupNames = lookupNamesForTerms,
          lookupRelativeHQRefs' = \searchType hqname ->
            case searchType of
              ExactName -> termRefsByHQNamesOf namesPerspective id . fmap stripMountPathPrefix $ hqname
              -- We can implement this, but it's not currently used anywhere on share.
              IncludeSuffixes -> error "Suffix search not yet implemented on Share",
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
        }

    lookupNamesForTypes :: V1.Reference -> m (Set (HQ'.HashQualified Name))
    lookupNamesForTypes ref = fromMaybeT (pure mempty) $ do
      pgRef <- MaybeT $ CV.references1ToPGOf id ref
      names <- lift $ NLOps.typeNamesForRefsWithinNamespaceOf namesPerspective Nothing NoSuffixify id pgRef
      names
        & fmap (\(fqnSegments, _suffixSegments) -> HQ'.HashQualified (reversedSegmentsToName fqnSegments) (V1Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: V1Referent.Referent -> m (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = fromMaybeT (pure mempty) $ do
      pgRef <- MaybeT $ CV.referents1ToPGOf id ref
      names <- lift $ NLOps.termNamesForRefsWithinNamespaceOf namesPerspective Nothing NoSuffixify id pgRef
      names
        & fmap (\(fqnSegments, _suffixSegments) -> HQ'.HashQualified (reversedSegmentsToName fqnSegments) (V1Referent.toShortHash ref))
        & Set.fromList
        & pure

    reversedSegmentsToName :: ReversedName -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

-- | Search the codebase for terms which exactly match the hq name.
termRefsByHQNamesOf ::
  (QueryM m) =>
  NamesPerspective m ->
  Traversal s t (HQ'.HashQualified Name) (Set V1Referent.Referent) ->
  s ->
  m t
termRefsByHQNamesOf namesPerspective trav s = do
  s
    & asListOf trav %%~ \hqNames -> do
      let tupled =
            hqNames <&> \case
              HQ'.NameOnly name -> (coerce @(NonEmpty NameSegment) @ReversedName $ Name.reverseSegments name, Nothing)
              HQ'.HashQualified name sh -> (coerce @(NonEmpty NameSegment) @ReversedName $ Name.reverseSegments name, Just sh)
      foundTermRefs <- NLOps.termRefsForExactNamesOf namesPerspective (traversed . _1) tupled
      foundTermRefs
        & over (traversed . _1 . traversed) (\NamedRef {ref} -> ref)
        <&> ( \case
                (results, Nothing) -> results
                (results, Just sh) ->
                  results
                    & filter (\ref -> sh `V1ShortHash.isPrefixOf` V1Referent.toShortHash ref)
            )
        <&> Set.fromList
        & pure

-- | Search the codebase for types which exactly match the hq name.
typeRefsByHQNamesOf ::
  (QueryM m) =>
  NamesPerspective m ->
  Traversal s t (HQ'.HashQualified Name) (Set V1Reference.Reference) ->
  s ->
  m t
typeRefsByHQNamesOf namesPerspective trav s = do
  s
    & asListOf trav %%~ \hqNames -> do
      let tupled =
            hqNames <&> \case
              HQ'.NameOnly name -> (coerce @(NonEmpty NameSegment) @ReversedName $ Name.reverseSegments name, Nothing)
              HQ'.HashQualified name sh -> (coerce @(NonEmpty NameSegment) @ReversedName $ Name.reverseSegments name, Just sh)
      foundTypeRefs <- NLOps.typeRefsForExactNamesOf namesPerspective (traversed . _1) tupled
      foundTypeRefs
        & over (traversed . _1 . traversed) (\NamedRef {ref} -> ref)
        <&> ( \case
                (results, Nothing) -> results
                (results, Just sh) ->
                  results
                    & filter (\ref -> sh `V1ShortHash.isPrefixOf` V1Reference.toShortHash ref)
            )
        <&> Set.fromList
        & pure
