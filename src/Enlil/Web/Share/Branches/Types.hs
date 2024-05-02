{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Enlil.Web.Share.Branches.Types where

import Data.Aeson
import Data.Time (UTCTime)
import Enlil.Branch (Branch (..))
import Enlil.IDs
import Enlil.IDs qualified as IDs
import Enlil.Postgres.IDs
import Enlil.Web.Share.Contributions.Types (ShareContribution)
import Enlil.Web.Share.Projects.Types
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (..))

branchToShareBranch :: BranchShortHand -> Branch CausalHash -> APIProject -> [ShareContribution] -> ShareBranch
branchToShareBranch branchShortHand Branch {createdAt, updatedAt, causal} project contributions = do
  ShareBranch
    { branchShortHand,
      createdAt,
      updatedAt,
      causalHash = PrefixedHash @"#" causal,
      project,
      contributions
    }

data ShareBranch = ShareBranch
  { branchShortHand :: BranchShortHand,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    causalHash :: PrefixedHash "#" CausalHash,
    project :: APIProject,
    contributions :: [ShareContribution]
  }

instance ToJSON ShareBranch where
  toJSON ShareBranch {..} =
    object
      [ "branchRef" .= IDs.toText @BranchShortHand branchShortHand,
        "createdAt" .= createdAt,
        "updatedAt" .= updatedAt,
        "causalHash" .= causalHash,
        "project" .= project,
        "contributions" .= contributions
      ]

-- | Allows filtering the branches list for contributor or core branches.
data BranchKindFilter
  = AllBranchKinds
  | OnlyCoreBranches
  | OnlyContributorBranches
  deriving stock (Eq, Show)

instance FromHttpApiData BranchKindFilter where
  parseQueryParam "all" = Right AllBranchKinds
  parseQueryParam "core" = Right OnlyCoreBranches
  parseQueryParam "contributor" = Right OnlyContributorBranches
  parseQueryParam _ = Left "Invalid branch kind filter, must be one of: ['all', 'core', contributor']"
