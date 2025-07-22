module Share.PrettyPrintEnvDecl.Postgres (ppedForReferences) where

import Control.Lens
import Data.Either qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Share.Postgres qualified as PG
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Queries (ShouldSuffixify (..))
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Postgres.Refs.Types
import Share.Prelude
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

ppedForReferences :: forall m. (PG.QueryM m) => NamesPerspective m -> Set LabeledDependency -> m PPED.PrettyPrintEnvDecl
ppedForReferences namesPerspective refs = do
  (pgRefTerms, pgRefTypes) <-
    Set.toList refs
      & CV.labeledDependencies1ToPG
      & fmap catMaybes -- Filter out any missing components
      & fmap List.partitionEithers
      & PG.pipelined
  termNames <- concat <$> termNamesOf traversed pgRefTerms
  typeNames <- concat <$> typeNamesOf traversed pgRefTypes
  pure $ ppedFromNamesWithSuffixes termNames typeNames
  where
    termNamesOf :: Traversal s t (V1.Referent, PGReferent) [(Name, Name, V1.Referent)] -> s -> m t
    termNamesOf trav s =
      s
        & asListOf trav %%~ \refs -> do
          let pgRefs = snd <$> refs
          termNames :: [[(NameLookups.ReversedName, NameLookups.ReversedName)]] <-
            NameLookupOps.termNamesForRefsWithinNamespaceOf namesPerspective Nothing Suffixify traversed pgRefs
          pure $ do
            ((ref, _pgRef), names) <- zip refs termNames
            pure $ do
              (fqn, suffixed) <- names
              pure $ (NameLookups.reversedNameToName fqn, NameLookups.reversedNameToName suffixed, ref)
    typeNamesOf :: Traversal s t (V1.Reference, PGReference) [(Name, Name, V1.Reference)] -> s -> m t
    typeNamesOf trav s =
      s
        & asListOf trav %%~ \refs -> do
          let pgRefs = snd <$> refs
          typeNames :: [[(NameLookups.ReversedName, NameLookups.ReversedName)]] <-
            NameLookupOps.typeNamesForRefsWithinNamespaceOf namesPerspective Nothing Suffixify traversed pgRefs
          pure $ do
            ((ref, _pgRef), names) <- zip refs typeNames
            pure $ do
              (fqn, suffixed) <- names
              pure $ (NameLookups.reversedNameToName fqn, NameLookups.reversedNameToName suffixed, ref)

-- | Given a list of (fqn, suffixified, ref), return a PrettyPrintEnvDecl
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
