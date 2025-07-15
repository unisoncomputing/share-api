-- | Efficiently fetch a Names object for a given set of labeled dependencies.
module Share.Names.Postgres (namesForReferences) where

import Control.Lens
import Data.Either qualified as List
import Data.Set qualified as Set
import Share.Postgres qualified as PG
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Queries (ShouldSuffixify (NoSuffixify))
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
  (pgRefTerms, pgRefTypes) <-
    Set.toList refs
      & CV.labeledDependencies1ToPG
      & fmap catMaybes -- Filter out any missing components
      & fmap List.partitionEithers
      & PG.pipelined

  termNames <- concat <$> termNamesOf traversed pgRefTerms
  typeNames <- concat <$> typeNamesOf traversed pgRefTypes
  pure $ Names.fromTermsAndTypes termNames typeNames
  where
    typeNamesOf :: Traversal s t (V1.Reference, PGReference) [(Name, V1.Reference)] -> s -> m t
    typeNamesOf trav s = do
      s
        & unsafePartsOf trav %%~ \refs -> do
          let pgRefs = snd <$> refs
          typeNames :: [[(NameLookups.ReversedName, NameLookups.ReversedName)]] <-
            NameLookupOps.typeNamesForRefsWithinNamespaceOf namesPerspective Nothing NoSuffixify traversed pgRefs
          pure $ do
            ((ref, _pgRef), names) <- zip refs typeNames
            pure $ do
              (fqn, _suffixed) <- names
              pure $ (NameLookups.reversedNameToName fqn, ref)

    termNamesOf :: Traversal s t (V1.Referent, PGReferent) [(Name, V1.Referent)] -> s -> m t
    termNamesOf trav s =
      s
        & unsafePartsOf trav %%~ \refs -> do
          let pgRefs = snd <$> refs
          termNames :: [[(NameLookups.ReversedName, NameLookups.ReversedName)]] <-
            NameLookupOps.termNamesForRefsWithinNamespaceOf namesPerspective Nothing NoSuffixify traversed pgRefs
          pure $ do
            ((ref, _pgRef), names) <- zip refs termNames
            pure $ do
              (fqn, _suffixed) <- names
              pure $ (NameLookups.reversedNameToName fqn, ref)
