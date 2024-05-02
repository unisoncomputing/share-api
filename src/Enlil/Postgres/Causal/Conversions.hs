module Enlil.Postgres.Causal.Conversions (namespaceStatsPgToV2) where

import Enlil.Postgres.Causal.Types qualified as PG
import Enlil.Postgres.Causal.Types qualified as PGCausal
import U.Codebase.Branch qualified as V2

namespaceStatsPgToV2 :: PGCausal.NamespaceStats -> V2.NamespaceStats
namespaceStatsPgToV2 PGCausal.NamespaceStats {deepContainedTerms, deepContainedTypes, deepContainedConstructors} =
  V2.NamespaceStats
    { V2.numContainedTerms = deepContainedTerms + deepContainedConstructors,
      V2.numContainedTypes = deepContainedTypes,
      -- Patches are deprecated, we don't care about their counts anywhere on Share.
      V2.numContainedPatches = 0
    }
