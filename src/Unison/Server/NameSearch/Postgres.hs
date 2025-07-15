module Unison.Server.NameSearch.Postgres
  ( NameSearch (..),
    nameSearchForPerspective,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Share.Codebase qualified as Codebase
import Share.Postgres qualified as PG
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Ops as NLOps
import Share.Postgres.NameLookups.Types
import Share.Prelude
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.NamesWithHistory (SearchType (..))
import Unison.Reference qualified as V1
import Unison.Reference qualified as V1Reference
import Unison.Referent qualified as V1Referent
import Unison.Server.NameSearch (NameSearch (..), Search (..))
import Unison.Server.SearchResult qualified as SR

nameSearchForPerspective :: forall m. (PG.QueryM m) => NamesPerspective -> NameSearch m
nameSearchForPerspective namesPerspective =
  NameSearch {typeSearch, termSearch}
  where
    -- Some searches will provide a fully-qualified name, so we need to strip off the
    -- mount-path before we search or it will fail to find anything.
    stripMountPathPrefix :: Name -> Name
    stripMountPathPrefix name = Name.tryStripReversedPrefix name (reverse . coerce $ pathToMountedNameLookup namesPerspective)
    typeSearch =
      Search
        { lookupNames = lookupNamesForTypes,
          lookupRelativeHQRefs' = \searchType hqname ->
            case searchType of
              ExactName -> hqTypeSearch . fmap stripMountPathPrefix $ hqname
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
              ExactName -> hqTermSearch . fmap stripMountPathPrefix $ hqname
              -- We can implement this, but it's not currently used anywhere on share.
              IncludeSuffixes -> error "Suffix search not yet implemented on Share",
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
        }

    lookupNamesForTypes :: V1.Reference -> m (Set (HQ'.HashQualified Name))
    lookupNamesForTypes ref = fromMaybeT (pure mempty) $ do
      pgRef <- MaybeT $ CV.references1ToPGOf id ref
      names <- NLOps.typeNamesForRefWithinNamespace namesPerspective pgRef Nothing
      names
        & fmap (\(fqnSegments, _suffixSegments) -> HQ'.HashQualified (reversedSegmentsToName fqnSegments) (V1Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: V1Referent.Referent -> m (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = fromMaybeT (pure mempty) $ do
      pgRef <- MaybeT $ CV.referents1ToPGOf id ref
      names <- NLOps.termNamesForRefsWithinNamespaceOf namesPerspective Nothing id pgRef
      names
        & fmap (\(fqnSegments, _suffixSegments) -> HQ'.HashQualified (reversedSegmentsToName fqnSegments) (V1Referent.toShortHash ref))
        & Set.fromList
        & pure
    -- Search the codebase for matches to the given hq name.
    hqTermSearch :: HQ'.HashQualified Name -> m (Set V1Referent.Referent)
    hqTermSearch hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          namedRefs <- NLOps.termRefsForExactName namesPerspective (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap (\(NamedRef {ref}) -> ref)
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = fullyQualifyName name
          termRefsV1 <-
            Set.toList <$> Codebase.termReferentsByShortHash sh
          termRefsPG <- catMaybes <$> CV.referents1ToPGOf traversed termRefsV1
          fmap Set.fromList . forMaybe (zip termRefsV1 termRefsPG) $ \(termRef, pgTermRef) -> do
            matches <-
              NLOps.termNamesForRefsWithinNamespaceOf namesPerspective (Just . coerce $ Name.reverseSegments name) id pgTermRef
                <&> fmap fst -- Only need the fqn
                -- Return a valid ref if at least one match was found.
            if any (\n -> coerce (Name.reverseSegments fqn) == n) matches
              then pure (Just termRef)
              else pure Nothing

    -- Search the codebase for matches to the given hq name.
    hqTypeSearch :: HQ'.HashQualified Name -> m (Set V1.Reference)
    hqTypeSearch hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          namedRefs <- NLOps.typeRefsForExactName namesPerspective (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap (\NamedRef {ref} -> ref)
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = fullyQualifyName name
          typeRefs <- Set.toList <$> Codebase.typeReferencesByShortHash sh
          pgTypeRefs <- catMaybes <$> CV.references1ToPGOf traversed typeRefs
          fmap Set.fromList . forMaybe (zip typeRefs pgTypeRefs) $ \(typeRef, pgTypeRef) -> do
            matches <-
              NLOps.typeNamesForRefWithinNamespace namesPerspective pgTypeRef (Just . coerce $ Name.reverseSegments name)
                <&> fmap fst -- Only need the fqn
                -- Return a valid ref if at least one match was found.
            if any (\n -> coerce (Name.reverseSegments fqn) == n) matches
              then pure (Just typeRef)
              else pure Nothing

    reversedSegmentsToName :: ReversedName -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

    -- Fully qualify a name by prepending the current namespace perspective's path
    fullyQualifyName :: Name -> Name
    fullyQualifyName name = fromMaybe name $ Path.maybePrefixName (Path.AbsolutePath' $ Path.Absolute (Path.fromList . (fmap NameSegment.NameSegment) . into @[Text] $ pathToMountedNameLookup namesPerspective)) name
