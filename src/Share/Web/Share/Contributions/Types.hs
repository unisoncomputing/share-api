{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | A module for the types used in the external API.
-- Breaking changes to these require alternations to the share frontend.
module Share.Web.Share.Contributions.Types where

import Data.Aeson
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData (..), ToHttpApiData)
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.API (NullableUpdate, parseNullableUpdate)
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors qualified as Err
import Share.Web.Share.Comments (CommentEvent (..), commentEventTimestamp)
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)
import U.Codebase.HashTags (CausalHash (..))
import Unison.Hash qualified as Hash
import Web.HttpApiData (ToHttpApiData (..))

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

instance FromJSON (ShareContribution UserDisplayInfo) where
  parseJSON = withObject "ShareContribution" \o -> do
    contributionId <- o .: "id"
    projectShortHand <- o .: "projectRef"
    number <- o .: "number"
    title <- o .: "title"
    description <- o .:? "description"
    status <- o .: "status"
    sourceBranchShortHand <- o .: "sourceBranchRef"
    targetBranchShortHand <- o .: "targetBranchRef"
    createdAt <- o .: "createdAt"
    updatedAt <- o .: "updatedAt"
    author <- o .:? "author"
    numComments <- o .: "numComments"
    pure ShareContribution {..}

-- | Allows filtering the branches list for contributor or core branches.
data ContributionKindFilter
  = AllContributionKinds
  | OnlyCoreContributions
  | OnlyContributorContributions
  deriving stock (Eq, Show)

instance FromHttpApiData ContributionKindFilter where
  parseQueryParam = \case
    "all" -> Right AllContributionKinds
    "core" -> Right OnlyCoreContributions
    "contributor" -> Right OnlyContributorContributions
    _ -> Left "Invalid contribution kind filter, must be one of: ['all', 'core', contributor']"

instance ToHttpApiData ContributionKindFilter where
  toQueryParam = \case
    AllContributionKinds -> "all"
    OnlyCoreContributions -> "core"
    OnlyContributorContributions -> "contributor"

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

instance (FromJSON user) => FromJSON (ContributionTimelineEvent user) where
  parseJSON = withObject "ContributionTimelineEvent" \o -> do
    kind <- o .:? "kind"
    case kind of
      (Just ("statusChange" :: Text)) -> do
        newStatus <- o .: "newStatus"
        oldStatus <- o .:? "oldStatus"
        actor <- o .: "actor"
        timestamp <- o .: "timestamp"
        pure $ ContributionTimelineStatusChange StatusChangeEvent {..}
      _ -> ContributionTimelineComment <$> parseJSON (Object o)

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

data Mergeability
  = CanFastForward
  | CanMerge
  | Conflicted
  | AlreadyMerged
  | -- We can presumably remove this once proper server-side merge is implemented
    CantMerge Text
  deriving (Show)

data CheckMergeContributionResponse = CheckMergeContributionResponse
  { mergeability :: Mergeability
  }
  deriving (Show)

instance ToJSON CheckMergeContributionResponse where
  toJSON CheckMergeContributionResponse {..} =
    object
      [ "mergeability" .= case mergeability of
          CanFastForward -> object ["kind" .= ("fast_forward" :: Text)]
          CanMerge -> object ["kind" .= ("merge" :: Text)]
          Conflicted -> object ["kind" .= ("conflicted" :: Text)]
          AlreadyMerged -> object ["kind" .= ("already_merged" :: Text)]
          CantMerge msg -> object ["kind" .= ("cant_merge" :: Text), "reason" .= msg]
      ]

instance FromJSON CheckMergeContributionResponse where
  parseJSON = withObject "CheckMergeContributionResponse" \o -> do
    mergeability <- o .: "mergeability"
    case mergeability of
      Object obj -> do
        kind <- obj .: "kind"
        case kind of
          ("fast_forward" :: Text) -> pure CheckMergeContributionResponse {mergeability = CanFastForward}
          "merge" -> pure CheckMergeContributionResponse {mergeability = CanMerge}
          "conflicted" -> pure CheckMergeContributionResponse {mergeability = Conflicted}
          "already_merged" -> pure CheckMergeContributionResponse {mergeability = AlreadyMerged}
          "cant_merge" -> do
            reason <- obj .: "reason"
            pure $ CheckMergeContributionResponse {mergeability = CantMerge reason}
          _ -> fail $ "Invalid mergeability kind: " <> Text.unpack kind
      _ -> fail "Expected an object for check merge contribution response"

data MergeResult
  = MergeSuccess
  | SourceBranchUpdated
  | TargetBranchUpdated
  | MergeConflicted
  | MergeFailed Text
  deriving (Show)

data MergeContributionRequest = MergeContributionRequest
  { contributionStateToken :: ContributionStateToken
  }
  deriving (Show)

instance FromJSON MergeContributionRequest where
  parseJSON = withObject "MergeContributionRequest" \o -> do
    contributionStateToken <- o .: "contributionStateToken"
    case parseQueryParam contributionStateToken of
      Left err -> fail (Text.unpack err)
      Right stateToken -> pure MergeContributionRequest {contributionStateToken = stateToken}

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

instance FromJSON MergeContributionResponse where
  parseJSON = withObject "MergeContributionResponse" \o -> do
    result <- o .: "result"
    case result of
      Object obj -> do
        kind <- obj .: "kind"
        case kind of
          ("success" :: Text) -> pure MergeContributionResponse {result = MergeSuccess}
          "source_branch_updated" -> pure MergeContributionResponse {result = SourceBranchUpdated}
          "target_branch_updated" -> pure MergeContributionResponse {result = TargetBranchUpdated}
          "conflicted" -> pure MergeContributionResponse {result = MergeConflicted}
          "failed" -> do
            reason <- obj .: "reason"
            pure $ MergeContributionResponse {result = MergeFailed reason}
          _ -> fail $ "Invalid merge result kind: " <> Text.unpack kind
      _ -> fail "Expected an object for merge contribution response"

-- | Token used to ensure that the state of a contribution hasn't changed between
-- rendering the page and the user taking a given action.
--
-- We don't bother signing these, so don't use it for Auth or w/e
--
-- E.g. A new commit is pushed in between loading the page and clicking 'merge'
data ContributionStateToken = ContributionStateToken
  { contributionId :: ContributionId,
    sourceBranchId :: BranchId,
    targetBranchId :: BranchId,
    sourceCausalHash :: CausalHash,
    targetCausalHash :: CausalHash,
    contributionStatus :: ContributionStatus
  }
  deriving (Show, Eq, Ord)

instance ToHttpApiData ContributionStateToken where
  toQueryParam ContributionStateToken {..} =
    Text.intercalate
      ":"
      [ IDs.toText contributionId,
        IDs.toText sourceBranchId,
        IDs.toText targetBranchId,
        into @Text sourceCausalHash,
        into @Text targetCausalHash,
        into @Text contributionStatus
      ]

instance FromHttpApiData ContributionStateToken where
  parseQueryParam token =
    case Text.splitOn ":" token of
      [contributionId, sourceBranchId, targetBranchId, sourceCausalHash, targetCausalHash, contributionStatus] -> do
        contributionId <- IDs.fromText contributionId
        sourceBranchId <- IDs.fromText sourceBranchId
        targetBranchId <- IDs.fromText targetBranchId
        sourceCausalHash <- CausalHash <$> maybeToEither "Invalid source causal hash" (Hash.fromBase32HexText sourceCausalHash)
        targetCausalHash <- CausalHash <$> maybeToEither "Invalid target causal hash" (Hash.fromBase32HexText targetCausalHash)
        contributionStatus <- parseQueryParam contributionStatus
        pure ContributionStateToken {..}
      _ -> Left "Invalid contribution state token"

instance ToJSON ContributionStateToken where
  toJSON ContributionStateToken {..} = String (toQueryParam ContributionStateToken {..})

instance FromJSON ContributionStateToken where
  parseJSON = withText "ContributionStateToken" \t ->
    case parseQueryParam t of
      Left err -> fail (Text.unpack err)
      Right token -> pure token

instance PG.DecodeRow ContributionStateToken where
  decodeRow = do
    contributionId <- PG.decodeField
    sourceBranchId <- PG.decodeField
    targetBranchId <- PG.decodeField
    sourceCausalHash <- PG.decodeField
    targetCausalHash <- PG.decodeField
    contributionStatus <- PG.decodeField
    pure ContributionStateToken {..}

data ContributionStateChangedError = ContributionStateChangedError
  { expectedStateToken :: ContributionStateToken,
    actualStateToken :: ContributionStateToken
  }
  deriving stock (Show)
  deriving (Logging.Loggable) via (Logging.ShowLoggable 'Logging.Error ContributionStateChangedError)
  deriving (Err.ToServerError) via Err.SimpleServerError 417 "contribution:state-changed" "Contribution state changed from what was expected" ContributionStateChangedError
