-- | Efficiently fetch a Names object for a given set of labeled dependencies.
module Share.Names.Postgres (namesForReferences) where

import Control.Lens
import Data.Set qualified as Set
import Share.Postgres qualified as PG
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Types (NamesPerspective)
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Postgres.Refs.Types
import Share.Prelude
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1

namesForReferences :: forall m. (PG.QueryM m) => NamesPerspective -> Set LabeledDependency -> m Names
namesForReferences namesPerspective refs = do
  withPGRefs <-
    Set.toList refs
      & CV.labeledDependencies1ToPG
  (termNames, typeNames) <- foldMapM namesForReference withPGRefs
  pure $ Names.fromTermsAndTypes termNames typeNames
  where
    -- TODO: Can probably speed this up by skipping suffixification.
    namesForReference :: Either (V1.Referent, PGReferent) (V1.Reference, PGReference) -> m ([(Name, V1.Referent)], [(Name, V1.Reference)])
    namesForReference = \case
      Left (ref, pgref) -> do
        termNames <- fmap (bothMap NameLookups.reversedNameToName) <$> NameLookupOps.termNamesForRefWithinNamespace namesPerspective pgref Nothing
        let termNames' = termNames <&> \(fqn, _suffixed) -> (fqn, ref)
        pure $ (termNames', [])
      Right (ref, pgref) -> do
        typeNames <- fmap (bothMap NameLookups.reversedNameToName) <$> NameLookupOps.typeNamesForRefWithinNamespace namesPerspective pgref Nothing
        let typeNames' = typeNames <&> \(fqn, _suffixed) -> (fqn, ref)
        pure $ ([], typeNames')
