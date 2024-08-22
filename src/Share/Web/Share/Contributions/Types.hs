{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Share.Web.Share.Contributions.Types where

import Data.Aeson
import Data.Time (UTCTime)
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (..))
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Web.Share.Comments (CommentEvent (..), commentEventTimestamp)
import Share.Web.Share.Types (UserDisplayInfo)

data ShareContribution user = ShareContribution
  { contributionId :: ContributionId,
    projectShortHand :: ProjectShortHand,
    number :: ContributionNumber,
    title :: Text,
    description :: Maybe Text,
    status :: ContributionStatus,
    sourceBranchShortHand :: BranchShortHand,
    targetBranchShortHand :: BranchShortHand,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    -- This is optional so we can delete users without deleting ALL their contributions.
    author :: Maybe user,
    numComments :: Int32
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance PG.DecodeRow (ShareContribution UserId) where
  decodeRow = do
    contributionId <- PG.decodeField
    number <- PG.decodeField
    projectOwnerHandle <- PG.decodeField
    projectSlug <- PG.decodeField
    title <- PG.decodeField
    description <- PG.decodeField
    status <- PG.decodeField
    sourceBranchName <- PG.decodeField
    sourceBranchContributorHandle <- PG.decodeField
    targetBranchName <- PG.decodeField
    targetBranchContributorHandle <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    author <- PG.decodeField
    numComments <- PG.decodeField @Int32
    let projectShortHand = ProjectShortHand {userHandle = projectOwnerHandle, projectSlug}
    -- NOTE: Right now every contribution's branches are restricted to being in the same
    -- project, but if that ever changes we'll need to fetch the proper project short
    -- hands for these branches here:
    let sourceBranchShortHand = BranchShortHand {branchName = sourceBranchName, contributorHandle = sourceBranchContributorHandle}
    let targetBranchShortHand = BranchShortHand {branchName = targetBranchName, contributorHandle = targetBranchContributorHandle}
    pure ShareContribution {..}

instance ToJSON (ShareContribution UserDisplayInfo) where
  toJSON ShareContribution {..} =
    object
      [ "id" .= contributionId,
        "projectRef" .= projectShortHand,
        "number" .= number,
        "title" .= title,
        "description" .= description,
        "status" .= status,
        "sourceBranchRef" .= sourceBranchShortHand,
        "targetBranchRef" .= targetBranchShortHand,
        "createdAt" .= createdAt,
        "updatedAt" .= updatedAt,
        "author" .= author,
        "numComments" .= numComments
      ]

-- | Allows filtering the branches list for contributor or core branches.
data ContributionKindFilter
  = AllContributionKinds
  | OnlyCoreContributions
  | OnlyContributorContributions
  deriving stock (Eq, Show)

instance FromHttpApiData ContributionKindFilter where
  parseQueryParam "all" = Right AllContributionKinds
  parseQueryParam "core" = Right OnlyCoreContributions
  parseQueryParam "contributor" = Right OnlyContributorContributions
  parseQueryParam _ = Left "Invalid contribution kind filter, must be one of: ['all', 'core', contributor']"

data StatusChangeEvent user = StatusChangeEvent
  { oldStatus :: Maybe ContributionStatus,
    newStatus :: ContributionStatus,
    actor :: user,
    timestamp :: UTCTime
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (PG.DecodeField user) => PG.DecodeRow (StatusChangeEvent user) where
  decodeRow = do
    oldStatus <- PG.decodeField
    newStatus <- PG.decodeField
    actor <- PG.decodeField
    timestamp <- PG.decodeField
    pure StatusChangeEvent {..}

data ContributionTimelineEvent user
  = ContributionTimelineStatusChange (StatusChangeEvent user)
  | ContributionTimelineComment (CommentEvent user)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

eventTimestamp :: ContributionTimelineEvent user -> UTCTime
eventTimestamp = \case
  ContributionTimelineStatusChange StatusChangeEvent {timestamp} -> timestamp
  ContributionTimelineComment commentEvent -> commentEventTimestamp commentEvent

instance (ToJSON user) => ToJSON (ContributionTimelineEvent user) where
  toJSON = \case
    ContributionTimelineStatusChange StatusChangeEvent {..} ->
      object
        [ "kind" .= ("statusChange" :: Text),
          "newStatus" .= newStatus,
          "oldStatus" .= oldStatus,
          "timestamp" .= timestamp,
          "actor" .= actor
        ]
    ContributionTimelineComment commentEvent -> toJSON commentEvent

data CreateContributionRequest = CreateContributionRequest
  { title :: Text,
    description :: Maybe Text,
    status :: ContributionStatus,
    sourceBranchShortHand :: BranchShortHand,
    targetBranchShortHand :: BranchShortHand
  }
  deriving (Show)

instance FromJSON CreateContributionRequest where
  parseJSON = withObject "CreateContributionRequest" \o -> do
    title <- o .: "title"
    description <- o .:? "description"
    status <- o .: "status"
    sourceBranchShortHand <- o .: "sourceBranchRef"
    targetBranchShortHand <- o .: "targetBranchRef"
    pure CreateContributionRequest {..}

data UpdateContributionRequest = UpdateContributionRequest
  { title :: Maybe Text,
    description :: NullableUpdate Text,
    status :: Maybe ContributionStatus,
    sourceBranchSH :: Maybe BranchShortHand,
    targetBranchSH :: Maybe BranchShortHand
  }
  deriving (Show)

instance FromJSON UpdateContributionRequest where
  parseJSON = withObject "UpdateContributionRequest" \o -> do
    title <- o .:? "title"
    description <- parseNullableUpdate o "description"
    status <- o .:? "status"
    sourceBranchSH <- o .:? "sourceBranchRef"
    targetBranchSH <- o .:? "targetBranchRef"
    pure UpdateContributionRequest {..}

data Mergability
  = CanFastForward
  | CanMerge
  | Conflicted
  | -- We can presumably remove this once proper server-side merge is implemented
    CantMerge Text
  deriving (Show)

data CheckMergeContributionResponse = CheckMergeContributionResponse
  { mergability :: Mergability
  }
  deriving (Show)

instance ToJSON CheckMergeContributionResponse where
  toJSON CheckMergeContributionResponse {..} =
    object
      [ "mergability" .= case mergability of
          CanFastForward -> object ["kind" .= ("fast_forward" :: Text)]
          CanMerge -> object ["kind" .= ("merge" :: Text)]
          Conflicted -> object ["kind" .= ("conflicted" :: Text)]
          CantMerge msg -> object ["kind" .= ("cant_merge" :: Text), "reason" .= msg]
      ]

data MergeResult
  = MergeSuccess
  | SourceBranchUpdated
  | TargetBranchUpdated
  | MergeConflicted
  | MergeFailed Text
  deriving (Show)

data MergeContributionResponse = MergeContributionResponse
  { result :: MergeResult
  }
  deriving (Show)

instance ToJSON MergeContributionResponse where
  toJSON MergeContributionResponse {..} =
    object
      [ "result" .= case result of
          MergeSuccess -> object ["kind" .= ("success" :: Text)]
          SourceBranchUpdated -> object ["kind" .= ("source_branch_updated" :: Text)]
          TargetBranchUpdated -> object ["kind" .= ("target_branch_updated" :: Text)]
          MergeConflicted -> object ["kind" .= ("conflicted" :: Text)]
          MergeFailed msg -> object ["kind" .= ("failed" :: Text), "reason" .= msg]
      ]
