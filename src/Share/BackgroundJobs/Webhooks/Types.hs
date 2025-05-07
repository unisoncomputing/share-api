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
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (FromJSON)
import Data.Text qualified as Text
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Prelude
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)

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

instance FromJSON BranchPayload where
  parseJSON = Aeson.withObject "BranchPayload" $ \o -> do
    branchId <- o Aeson..: "branchId"
    branchName <- o Aeson..: "branchName"
    branchShortHand <- o Aeson..: "branchShortHand"
    branchContributorUserId <- o Aeson..: "branchContributorUserId"
    branchContributorHandle <- o Aeson..: "branchContributorHandle"
    pure BranchPayload {branchId, branchName, branchShortHand, branchContributorUserId, branchContributorHandle}

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

instance FromJSON ProjectPayload where
  parseJSON = Aeson.withObject "ProjectPayload" $ \o -> do
    projectId <- o Aeson..: "projectId"
    projectSlug <- o Aeson..: "projectSlug"
    projectShortHand <- o Aeson..: "projectShortHand"
    projectOwnerHandle <- o Aeson..: "projectOwnerHandle"
    projectOwnerUserId <- o Aeson..: "projectOwnerUserId"
    pure ProjectPayload {projectId, projectSlug, projectShortHand, projectOwnerHandle, projectOwnerUserId}

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

instance FromJSON ProjectBranchPayload where
  parseJSON = Aeson.withObject "ProjectBranchPayload" $ \o -> do
    projectInfo <- o Aeson..: "project"
    branchInfo <- o Aeson..: "branch"
    pure ProjectBranchPayload {projectInfo, branchInfo}

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

instance FromJSON ProjectContributionPayload where
  parseJSON = Aeson.withObject "ProjectContributionPayload" $ \o -> do
    projectInfo <- o Aeson..: "project"
    mergeSourceBranch <- o Aeson..: "mergeSourceBranch"
    mergeTargetBranch <- o Aeson..: "mergeTargetBranch"
    contributionId <- o Aeson..: "contributionId"
    author <- o Aeson..: "author"
    title <- o Aeson..: "title"
    description <- o Aeson..: "description"
    status <- o Aeson..: "status"
    pure ProjectContributionPayload {projectInfo, mergeSourceBranch, mergeTargetBranch, contributionId, author, title, description, status}

data WebhookPayloadData
  = ProjectBranchUpdatedPayload ProjectBranchPayload
  | ProjectContributionCreatedPayload ProjectContributionPayload
  deriving stock (Show, Eq)

instance ToJSON WebhookPayloadData where
  toJSON = \case
    (ProjectBranchUpdatedPayload payload) ->
      toJSON payload & \case
        Aeson.Object o -> Aeson.Object (o <> KeyMap.singleton "kind" (Aeson.String "projectBranchUpdated"))
        _ -> error "Expected JSON object for ProjectBranchUpdatedPayload"
    (ProjectContributionCreatedPayload payload) ->
      toJSON payload & \case
        Aeson.Object o -> Aeson.Object (o <> KeyMap.singleton "kind" (Aeson.String "projectContributionCreated"))
        _ -> error "Expected JSON object for ProjectContributionCreatedPayload"

instance FromJSON WebhookPayloadData where
  parseJSON = Aeson.withObject "WebhookPayloadData" $ \o -> do
    kind <- o Aeson..: "kind"
    case kind of
      "projectBranchUpdated" -> ProjectBranchUpdatedPayload <$> Aeson.parseJSON (Aeson.Object o)
      "projectContributionCreated" -> ProjectContributionCreatedPayload <$> Aeson.parseJSON (Aeson.Object o)
      _ -> fail $ "Unknown kind: " <> Text.unpack kind
