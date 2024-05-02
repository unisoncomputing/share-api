module Unison.PrettyPrintEnvDecl.Postgres (ppedForReferences) where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Enlil.Postgres qualified as PG
import Enlil.Postgres.NameLookups.Conversions qualified as CV
import Enlil.Postgres.NameLookups.Ops qualified as NameLookupOps
import Enlil.Postgres.NameLookups.Types (NamesPerspective)
import Enlil.Postgres.NameLookups.Types qualified as NameLookups
import Enlil.Postgres.Refs.Types
import Enlil.Prelude
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

ppedForReferences :: forall m. PG.QueryM m => NamesPerspective -> Set LabeledDependency -> m PPED.PrettyPrintEnvDecl
ppedForReferences namesPerspective refs = do
  withPGRefs <-
    Set.toList refs
      & CV.labeledDependencies1ToPG
  (termNames, typeNames) <- foldMapM namesForReference withPGRefs
  pure $ ppedFromNamesWithSuffixes termNames typeNames
  where
    namesForReference :: Either (V1.Referent, PGReferent) (V1.Reference, PGReference) -> m ([(Name, Name, V1.Referent)], [(Name, Name, V1.Reference)])
    namesForReference = \case
      Left (ref, pgref) -> do
        termNames <- fmap (bothMap $ Name.fromReverseSegments . coerce) <$> NameLookupOps.termNamesForRefWithinNamespace namesPerspective pgref Nothing
        let termNames' = termNames <&> \(fqn, suffixed) -> (fqn, suffixed, ref)
        pure $ (termNames', [])
      Right (ref, pgref) -> do
        typeNames <- fmap (bothMap $ Name.fromReverseSegments . coerce) <$> NameLookupOps.typeNamesForRefWithinNamespace namesPerspective pgref Nothing
        let typeNames' = typeNames <&> \(fqn, suffixed) -> (fqn, suffixed, ref)
        pure $ ([], typeNames')

-- | Given a list of names and a list of names with suffixes, return a PrettyPrintEnvDecl
-- Note: this type of PPE does not (yet) support hash qualifying conflicted names, because this
-- would require running additional queries when fetching the names.
ppedFromNamesWithSuffixes :: [(Name, Name, V1.Referent)] -> [(Name, Name, V1.Reference)] -> PPED.PrettyPrintEnvDecl
ppedFromNamesWithSuffixes termNames typeNames =
  PPED.PrettyPrintEnvDecl fqnPPE suffixedPPE
  where
    termsMap :: Map V1.Referent [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    termsMap =
      termNames
        & fmap (\(fqn, suffixed, ref) -> (ref, [(HQ'.NameOnly fqn, HQ'.NameOnly suffixed)]))
        -- Flipped <> preserves the ordering returned by the query
        & Map.fromListWith (flip (<>))
    typesMap :: Map V1.Reference [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    typesMap =
      typeNames
        & fmap (\(fqn, suffixed, ref) -> (ref, [(HQ'.NameOnly fqn, HQ'.NameOnly suffixed)]))
        -- Flipped <> preserves the ordering returned by the query
        & Map.fromListWith (flip (<>))

    fqnPPE :: PPE.PrettyPrintEnv
    fqnPPE =
      PPE.PrettyPrintEnv
        { termNames = \r -> fromMaybe [] (Map.lookup r termsMap) <&> \(fqn, _suff) -> (fqn, fqn),
          typeNames = \r -> fromMaybe [] (Map.lookup r typesMap) <&> \(fqn, _suff) -> (fqn, fqn)
        }
    suffixedPPE :: PPE.PrettyPrintEnv
    suffixedPPE =
      PPE.PrettyPrintEnv
        { termNames = \r -> fromMaybe [] (Map.lookup r termsMap),
          typeNames = \r -> fromMaybe [] (Map.lookup r typesMap)
        }
