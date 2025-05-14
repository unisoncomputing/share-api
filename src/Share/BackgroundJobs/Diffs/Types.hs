module Share.BackgroundJobs.Diffs.Types (CausalDiffInfo (..)) where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs

data CausalDiffInfo = CausalDiffInfo
  { fromCausalId :: CausalId,
    toCausalId :: CausalId,
    fromCodebaseOwner :: UserId,
    toCodebaseOwner :: UserId
  }

instance PG.DecodeRow CausalDiffInfo where
  decodeRow =
    CausalDiffInfo
      <$> PG.decodeField
      <*> PG.decodeField
      <*> PG.decodeField
      <*> PG.decodeField
