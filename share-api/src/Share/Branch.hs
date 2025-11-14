{-# LANGUAGE RecordWildCards #-}

module Share.Branch
  ( Branch (..),
    defaultBranchName,
    defaultBranchShorthand,
    branchCausals_,
    branchCodebaseUser,
  )
where

import Control.Lens
import Data.Time (UTCTime)
import Hasql.Interpolate qualified as Hasql
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

-- | We don't track default branches in the database at the moment,
-- and haven't decided yet whether we will.
-- For now the default branch is just hardcoded to "main"
defaultBranchShorthand :: BranchShortHand
defaultBranchShorthand =
  BranchShortHand
    { contributorHandle = Nothing,
      branchName = defaultBranchName
    }

defaultBranchName :: BranchName
defaultBranchName = BranchName "main"

data Branch causal = Branch
  { branchId :: BranchId,
    projectId :: ProjectId,
    branchName :: BranchName,
    contributorId :: Maybe UserId,
    causal :: causal,
    mergeTarget :: Maybe BranchId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    creatorId :: UserId
  }
  deriving (Show, Eq, Ord)

instance (Hasql.DecodeValue causal) => Hasql.DecodeRow (Branch causal) where
  decodeRow = do
    branchId <- PG.decodeField
    projectId <- PG.decodeField
    branchName <- PG.decodeField
    contributorId <- PG.decodeField
    causal <- PG.decodeField
    mergeTarget <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    creatorId <- PG.decodeField
    pure $ Branch {..}

branchCausals_ :: Lens (Branch causal) (Branch causal') causal causal'
branchCausals_ f Branch {..} = (\causal -> Branch {causal, ..}) <$> f causal

branchCodebaseUser :: Branch causal -> UserId
branchCodebaseUser Branch {..} = fromMaybe creatorId contributorId
