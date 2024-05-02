module Enlil.Postgres.Causal.Types
  ( PgNamespace,
    CausalNamespace,
    PgCausalNamespace,
    U.Causal (..),
    NamespaceStats (..),
    namespaceHasDefinitions,
  )
where

import Enlil.Postgres.IDs
import U.Codebase.Branch qualified as U
import U.Codebase.Causal qualified as U
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull

-- | A Namespace (branch) which uses internal ids as references.
type PgNamespace = BranchFull.Branch' TextId ComponentHashId PatchId (BranchHashId, CausalId)

type PgCausalNamespace m = U.Causal m CausalId BranchHashId PgNamespace PgNamespace

-- | Unison Causal specialized for Share's types.
type CausalNamespace m = U.Causal m CausalHash BranchHash (U.Branch m) (U.Branch m)

data NamespaceStats = NamespaceStats
  { -- Number of terms in this namespace, not including terms in children.
    containedTerms :: Int,
    -- Number of terms in this namespace, including terms in children.
    deepContainedTerms :: Int,
    -- Number of types in this namespace, not including types in children.
    containedTypes :: Int,
    -- Number of types in this namespace, including types in children.
    deepContainedTypes :: Int,
    -- Number of constructors in this namespace, not including constructors in children.
    containedConstructors :: Int,
    -- Number of constructors in this namespace, including constructors in children.
    deepContainedConstructors :: Int
  }
  deriving (Show, Eq)

namespaceHasDefinitions :: NamespaceStats -> Bool
namespaceHasDefinitions stats =
  deepContainedTerms stats > 0
    || deepContainedTypes stats > 0
    || deepContainedConstructors stats > 0
