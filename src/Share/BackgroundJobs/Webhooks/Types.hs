module Share.BackgroundJobs.Webhooks.Types
  ( WebhookPayloadData (..),
    ProjectBranchPayload (..),
    ProjectContributionPayload (..),
    ProjectPayload (..),
    BranchPayload (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Web.Share.DisplayInfo (UserDisplayInfo)

data BranchPayload = BranchPayload
  { branchId :: BranchId,
    branchName :: BranchName,
    branchShortHand :: BranchShortHand,
    branchContributorUserId :: Maybe UserId,
    branchContributorHandle :: Maybe UserHandle
  }
  deriving stock (Show, Eq)

instance ToJSON BranchPayload where
  toJSON BranchPayload {branchId, branchName, branchShortHand, branchContributorUserId, branchContributorHandle} =
    Aeson.object
      [ "branchId" Aeson..= branchId,
        "branchName" Aeson..= branchName,
        "branchShortHand" Aeson..= branchShortHand,
        "branchContributorUserId" Aeson..= branchContributorUserId,
        "branchContributorHandle" Aeson..= branchContributorHandle
      ]

data ProjectPayload = ProjectPayload
  { projectId :: ProjectId,
    projectSlug :: ProjectSlug,
    projectShortHand :: ProjectShortHand,
    projectOwnerHandle :: UserHandle,
    projectOwnerUserId :: UserId
  }
  deriving stock (Show, Eq)

instance ToJSON ProjectPayload where
  toJSON ProjectPayload {projectId, projectSlug, projectShortHand, projectOwnerHandle, projectOwnerUserId} =
    Aeson.object
      [ "projectId" Aeson..= projectId,
        "projectSlug" Aeson..= projectSlug,
        "projectShortHand" Aeson..= projectShortHand,
        "projectOwnerHandle" Aeson..= projectOwnerHandle,
        "projectOwnerUserId" Aeson..= projectOwnerUserId
      ]

data ProjectBranchPayload = ProjectBranchPayload
  { projectInfo :: ProjectPayload,
    branchInfo :: BranchPayload
  }
  deriving stock (Show, Eq)

instance ToJSON ProjectBranchPayload where
  toJSON ProjectBranchPayload {projectInfo, branchInfo} =
    Aeson.object
      [ "project" Aeson..= projectInfo,
        "branch" Aeson..= branchInfo
      ]

data ProjectContributionPayload = ProjectContributionPayload
  { projectInfo :: ProjectPayload,
    mergeSourceBranch :: BranchPayload,
    mergeTargetBranch :: BranchPayload,
    contributionId :: ContributionId,
    author :: UserDisplayInfo,
    title :: Text,
    description :: Maybe Text,
    status :: ContributionStatus
  }
  deriving stock (Show, Eq)

instance ToJSON ProjectContributionPayload where
  toJSON ProjectContributionPayload {projectInfo, mergeSourceBranch, mergeTargetBranch, contributionId, author, title, description, status} =
    Aeson.object
      [ "project" Aeson..= projectInfo,
        "mergeSourceBranch" Aeson..= mergeSourceBranch,
        "mergeTargetBranch" Aeson..= mergeTargetBranch,
        "contributionId" Aeson..= contributionId,
        "author" Aeson..= author,
        "title" Aeson..= title,
        "description" Aeson..= description,
        "status" Aeson..= status
      ]

data WebhookPayloadData
  = ProjectBranchUpdatedPayload ProjectBranchPayload
  | ProjectContributionCreatedPayload ProjectContributionPayload
  deriving stock (Show, Eq)

instance ToJSON WebhookPayloadData where
  toJSON = \case
    (ProjectBranchUpdatedPayload payload) -> toJSON payload
    (ProjectContributionCreatedPayload payload) -> toJSON payload
