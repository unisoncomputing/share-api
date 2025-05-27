{-# LANGUAGE RecordWildCards #-}

module Share.Notifications.Types
  ( NotificationTopic (..),
    NotificationTopicGroup (..),
    NotificationFilter (..),
    NotificationEventData (..),
    NotificationEvent (..),
    NewNotificationEvent,
    PGNotificationEvent,
    NotificationSubscription (..),
    SubscriptionFilter (..),
    ProjectBranchData (..),
    ProjectContributionData (..),
    NotificationHubEntry (..),
    NotificationStatus (..),
    DeliveryMethodId (..),
    NotificationDeliveryMethod (..),
    NotificationEmailDeliveryConfig (..),
    NotificationWebhookConfig (..),
    HydratedEventPayload (..),
    HydratedEvent (..),
    BranchPayload (..),
    ProjectPayload (..),
    ContributionPayload (..),
    ProjectBranchUpdatedPayload (..),
    ProjectContributionCreatedPayload (..),
    eventTopic,
    hydratedEventTopic,
    eventData_,
    eventUserInfo_,
    hubEntryUserInfo_,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON, ToJSON (..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Hasql.Decoders qualified as HasqlDecoders
import Hasql.Encoders qualified as HasqlEncoders
import Hasql.Interpolate qualified as Hasql
import Network.URI (URI)
import Servant (FromHttpApiData (..))
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.URI (URIParam (..))
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)

data NotificationTopic
  = ProjectBranchUpdated
  | ProjectContributionCreated
  deriving (Eq, Show, Ord)

instance PG.EncodeValue NotificationTopic where
  encodeValue = HasqlEncoders.enum \case
    ProjectBranchUpdated -> "project:branch:updated"
    ProjectContributionCreated -> "project:contribution:created"

instance PG.DecodeValue NotificationTopic where
  decodeValue = HasqlDecoders.enum \case
    "project:branch:updated" -> Just ProjectBranchUpdated
    "project:contribution:created" -> Just ProjectContributionCreated
    _ -> Nothing

instance Aeson.ToJSON NotificationTopic where
  toJSON = \case
    ProjectBranchUpdated -> "project:branch:updated"
    ProjectContributionCreated -> "project:contribution:created"

instance Aeson.FromJSON NotificationTopic where
  parseJSON = Aeson.withText "NotificationTopic" \case
    "project:branch:updated" -> pure ProjectBranchUpdated
    "project:contribution:created" -> pure ProjectContributionCreated
    s -> fail $ "Invalid notification topic: " <> Text.unpack s

data NotificationTopicGroup
  = WatchProject
  deriving (Eq, Show, Ord)

instance PG.EncodeValue NotificationTopicGroup where
  encodeValue = HasqlEncoders.enum \case
    WatchProject -> "watch_project"

instance PG.DecodeValue NotificationTopicGroup where
  decodeValue = HasqlDecoders.enum \case
    "watch_project" -> Just WatchProject
    _ -> Nothing

instance Aeson.ToJSON NotificationTopicGroup where
  toJSON = \case
    WatchProject -> "watch_project"

instance Aeson.FromJSON NotificationTopicGroup where
  parseJSON = Aeson.withText "NotificationTopicGroup" \case
    "watch_project" -> pure WatchProject
    s -> fail $ "Invalid notification topic group: " <> Text.unpack s

data NotificationStatus
  = Unread
  | Read
  | Archived
  deriving (Eq, Show, Enum, Bounded, Ord)

instance FromHttpApiData NotificationStatus where
  parseQueryParam = \case
    "unread" -> Right Unread
    "read" -> Right Read
    "archived" -> Right Archived
    s -> Left $ "Invalid notification status: " <> s

instance Aeson.ToJSON NotificationStatus where
  toJSON = \case
    Unread -> "unread"
    Read -> "read"
    Archived -> "archived"

instance Aeson.FromJSON NotificationStatus where
  parseJSON = Aeson.withText "NotificationStatus" \case
    "unread" -> pure Unread
    "read" -> pure Read
    "archived" -> pure Archived
    s -> fail $ "Invalid notification status: " <> Text.unpack s

instance PG.EncodeValue NotificationStatus where
  encodeValue = HasqlEncoders.enum \case
    Unread -> "unread"
    Read -> "read"
    Archived -> "archived"

instance PG.DecodeValue NotificationStatus where
  decodeValue = HasqlDecoders.enum \case
    "unread" -> Just Unread
    "read" -> Just Read
    "archived" -> Just Archived
    _ -> Nothing

newtype NotificationFilter = NotificationFilter (Map Text Aeson.Value)
  deriving (Eq, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

instance PG.DecodeValue NotificationFilter where
  decodeValue = do
    HasqlDecoders.jsonb
      & HasqlDecoders.refine \obj ->
        case Aeson.fromJSON obj of
          Aeson.Error e -> Left (Text.pack e)
          Aeson.Success a -> Right a

instance PG.EncodeValue NotificationFilter where
  encodeValue =
    HasqlEncoders.jsonb
      & contramap \(NotificationFilter obj) -> Aeson.toJSON obj

data ProjectBranchData = ProjectBranchData
  { projectId :: ProjectId,
    branchId :: BranchId,
    branchContributorUserId :: Maybe UserId,
    public :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ProjectBranchData where
  toJSON ProjectBranchData {projectId, branchId, branchContributorUserId, public} =
    Aeson.object
      [ "projectId" .= projectId,
        "branchId" .= branchId,
        "branchContributorUserId" .= branchContributorUserId,
        "public" .= public
      ]

instance Aeson.FromJSON ProjectBranchData where
  parseJSON = Aeson.withObject "ProjectBranchData" \o -> do
    projectId <- o .: "projectId"
    branchId <- o .: "branchId"
    branchContributorUserId <- o .: "branchContributorUserId"
    public <- o .: "public"
    pure ProjectBranchData {projectId, branchId, branchContributorUserId, public}

data ProjectContributionData = ProjectContributionData
  { projectId :: ProjectId,
    contributionId :: ContributionId,
    fromBranchId :: BranchId,
    toBranchId :: BranchId,
    contributorUserId :: UserId,
    public :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ProjectContributionData where
  toJSON ProjectContributionData {projectId, contributionId, fromBranchId, toBranchId, contributorUserId, public} =
    Aeson.object
      [ "projectId" .= projectId,
        "contributionId" .= contributionId,
        "fromBranchId" .= fromBranchId,
        "toBranchId" .= toBranchId,
        "contributorUserId" .= contributorUserId,
        "public" .= public
      ]

instance Aeson.FromJSON ProjectContributionData where
  parseJSON = Aeson.withObject "ProjectContributionData" \o -> do
    projectId <- o .: "projectId"
    contributionId <- o .: "contributionId"
    fromBranchId <- o .: "fromBranchId"
    toBranchId <- o .: "toBranchId"
    contributorUserId <- o .: "contributorUserId"
    public <- o .: "public"
    pure ProjectContributionData {projectId, contributionId, fromBranchId, toBranchId, contributorUserId, public}

-- The bare-bones Notification Event Data that's actually stored in the database.
-- It holds unhydrated IDs.
data NotificationEventData
  = ProjectBranchUpdatedData ProjectBranchData
  | ProjectContributionCreatedData ProjectContributionData
  deriving stock (Eq, Show)

instance Aeson.ToJSON NotificationEventData where
  toJSON ned =
    Aeson.object
      [ "topic" Aeson..= topic,
        "data" Aeson..= body
      ]
    where
      topic = eventTopic ned
      body = case ned of
        ProjectBranchUpdatedData d -> Aeson.toJSON d
        ProjectContributionCreatedData d -> Aeson.toJSON d

instance PG.EncodeValue NotificationEventData where
  encodeValue =
    HasqlEncoders.jsonb
      & contramap \case
        ProjectBranchUpdatedData d -> Aeson.toJSON d
        ProjectContributionCreatedData d -> Aeson.toJSON d

instance Hasql.DecodeRow NotificationEventData where
  decodeRow = do
    topic <- PG.decodeField
    Hasql.Jsonb jsonData <- PG.decodeField
    case topic of
      ProjectBranchUpdated -> ProjectBranchUpdatedData <$> parseJsonData jsonData
      ProjectContributionCreated -> ProjectContributionCreatedData <$> parseJsonData jsonData
    where
      parseJsonData v = case Aeson.fromJSON v of
        Aeson.Error e -> fail e
        Aeson.Success a -> pure a

eventTopic :: NotificationEventData -> NotificationTopic
eventTopic = \case
  ProjectBranchUpdatedData {} -> ProjectBranchUpdated
  ProjectContributionCreatedData {} -> ProjectContributionCreated

-- | Description of a notifiable event.
data NotificationEvent id userInfo occurredAt eventPayload = NotificationEvent
  { eventId :: id,
    eventOccurredAt :: occurredAt,
    eventResourceId :: ResourceId,
    eventData :: eventPayload,
    eventScope :: userInfo,
    eventActor :: userInfo
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

eventData_ :: Lens (NotificationEvent id userInfo occurredAt eventPayload) (NotificationEvent id userInfo occurredAt eventPayload') eventPayload eventPayload'
eventData_ f event = (\eventData -> event {eventData}) <$> f (eventData event)

eventUserInfo_ :: Traversal (NotificationEvent id userInfo occurredAt eventPayload) (NotificationEvent id userInfo' occurredAt eventPayload) userInfo userInfo'
eventUserInfo_ f NotificationEvent {eventActor, eventScope, ..} = do
  eventActor' <- f eventActor
  eventScope' <- f eventScope
  pure $ NotificationEvent {eventActor = eventActor', eventScope = eventScope', ..}

instance (Aeson.ToJSON eventPayload, Aeson.ToJSON userInfo) => Aeson.ToJSON (NotificationEvent NotificationEventId userInfo UTCTime eventPayload) where
  toJSON NotificationEvent {eventId, eventOccurredAt, eventData, eventScope, eventActor} =
    Aeson.object
      [ "id" Aeson..= eventId,
        "occurredAt" Aeson..= eventOccurredAt,
        "data" Aeson..= eventData,
        "scope" Aeson..= eventScope,
        "actor" Aeson..= eventActor
      ]

instance Hasql.DecodeRow (NotificationEvent NotificationEventId UserId UTCTime NotificationEventData) where
  decodeRow = do
    eventId <- PG.decodeField
    eventOccurredAt <- PG.decodeField
    eventScope <- PG.decodeField
    eventActor <- PG.decodeField
    eventResourceId <- PG.decodeField
    eventData <- PG.decodeRow
    pure $ NotificationEvent {eventId, eventOccurredAt, eventData, eventScope, eventActor, eventResourceId}

type NewNotificationEvent = NotificationEvent () UserId () NotificationEventData

type PGNotificationEvent = NotificationEvent NotificationEventId UserId UTCTime NotificationEventData

data NotificationEmailDeliveryConfig = NotificationEmailConfig
  { emailDeliveryId :: NotificationEmailDeliveryMethodId,
    emailDeliveryEmail :: Email
  }
  deriving (Eq, Ord, Show)

instance PG.DecodeRow NotificationEmailDeliveryConfig where
  decodeRow = do
    emailDeliveryId <- PG.decodeField
    emailDeliveryEmail <- PG.decodeField
    pure $ NotificationEmailConfig {emailDeliveryId, emailDeliveryEmail}

instance Aeson.ToJSON NotificationEmailDeliveryConfig where
  toJSON NotificationEmailConfig {emailDeliveryId, emailDeliveryEmail} =
    Aeson.object
      [ "id" Aeson..= emailDeliveryId,
        "email" Aeson..= emailDeliveryEmail
      ]

data NotificationWebhookConfig = NotificationWebhookConfig
  { webhookDeliveryId :: NotificationWebhookId,
    webhookDeliveryUrl :: URI
  }
  deriving (Eq, Ord, Show)

instance PG.DecodeRow NotificationWebhookConfig where
  decodeRow = do
    webhookDeliveryId <- PG.decodeField
    URIParam webhookDeliveryUrl <- PG.decodeField
    pure $ NotificationWebhookConfig {webhookDeliveryId, webhookDeliveryUrl}

instance Aeson.ToJSON NotificationWebhookConfig where
  toJSON NotificationWebhookConfig {webhookDeliveryId, webhookDeliveryUrl} =
    Aeson.object
      [ "id" Aeson..= webhookDeliveryId,
        "url" Aeson..= show webhookDeliveryUrl
      ]

data DeliveryMethodId
  = EmailDeliveryMethodId NotificationEmailDeliveryMethodId
  | WebhookDeliveryMethodId NotificationWebhookId
  deriving stock (Show, Eq, Ord)

instance Aeson.FromJSON DeliveryMethodId where
  parseJSON = Aeson.withObject "DeliveryMethodId" $ \o -> do
    deliveryMethodKind <- o .: "kind"
    case deliveryMethodKind of
      "email" -> EmailDeliveryMethodId <$> o .: "id"
      "webhook" -> WebhookDeliveryMethodId <$> o .: "id"
      _ -> fail $ "Unknown delivery method kind: " <> Text.unpack deliveryMethodKind

data NotificationDeliveryMethod
  = EmailDeliveryMethod NotificationEmailDeliveryConfig
  | WebhookDeliveryMethod NotificationWebhookConfig
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON NotificationDeliveryMethod where
  toJSON = \case
    EmailDeliveryMethod config -> Aeson.object ["kind" .= ("email" :: Text), "config" .= config]
    WebhookDeliveryMethod config -> Aeson.object ["kind" .= ("webhook" :: Text), "config" .= config]

data NotificationSubscription id = NotificationSubscription
  { subscriptionId :: id,
    subscriptionScope :: UserId,
    subscriptionTopics :: Set NotificationTopic,
    subscriptionTopicGroups :: Set NotificationTopicGroup,
    subscriptionFilter :: Maybe NotificationFilter
  }

instance PG.DecodeRow (NotificationSubscription NotificationSubscriptionId) where
  decodeRow = do
    subscriptionId <- PG.decodeField
    subscriptionScope <- PG.decodeField
    subscriptionTopics <- Set.fromList <$> PG.decodeField
    subscriptionTopicGroups <- Set.fromList <$> PG.decodeField
    subscriptionFilter <- PG.decodeField
    pure $ NotificationSubscription {subscriptionId, subscriptionScope, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter}

instance Aeson.ToJSON (NotificationSubscription NotificationSubscriptionId) where
  toJSON NotificationSubscription {subscriptionId, subscriptionScope, subscriptionTopics, subscriptionTopicGroups, subscriptionFilter} =
    Aeson.object
      [ "id" Aeson..= subscriptionId,
        "scope" Aeson..= subscriptionScope,
        "topics" Aeson..= subscriptionTopics,
        "topicGroups" Aeson..= subscriptionTopicGroups,
        "filter" Aeson..= subscriptionFilter
      ]

data NotificationHubEntry userInfo eventPayload = NotificationHubEntry
  { hubEntryId :: NotificationHubEntryId,
    hubEntryEvent :: NotificationEvent NotificationEventId userInfo UTCTime eventPayload,
    hubEntryStatus :: NotificationStatus,
    hubEntryCreatedAt :: UTCTime
  }
  deriving stock (Functor, Foldable, Traversable)

instance (Aeson.ToJSON eventPayload, Aeson.ToJSON userInfo) => Aeson.ToJSON (NotificationHubEntry userInfo eventPayload) where
  toJSON NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus, hubEntryCreatedAt} =
    Aeson.object
      [ "id" Aeson..= hubEntryId,
        "event" Aeson..= hubEntryEvent,
        "status" Aeson..= hubEntryStatus,
        "createdAt" Aeson..= hubEntryCreatedAt
      ]

instance Hasql.DecodeRow (NotificationHubEntry UserId NotificationEventData) where
  decodeRow = do
    hubEntryId <- PG.decodeField
    hubEntryStatus <- PG.decodeField
    hubEntryCreatedAt <- PG.decodeField
    hubEntryEvent <- PG.decodeRow
    pure $ NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus, hubEntryCreatedAt}

hubEntryUserInfo_ :: Traversal (NotificationHubEntry userInfo eventPayload) (NotificationHubEntry userInfo' eventPayload) userInfo userInfo'
hubEntryUserInfo_ f (NotificationHubEntry {hubEntryEvent, ..}) = do
  hubEntryEvent' <- hubEntryEvent & eventUserInfo_ %%~ f
  pure $ NotificationHubEntry {hubEntryEvent = hubEntryEvent', ..}

newtype SubscriptionFilter = SubscriptionFilter (Aeson.Value)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via Hasql.Jsonb
  deriving newtype (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

-- Hydrated types

data BranchPayload = BranchPayload
  { branchId :: BranchId,
    branchName :: BranchName,
    branchShortHand :: BranchShortHand,
    projectBranchShortHand :: ProjectBranchShortHand,
    branchContributorUserId :: Maybe UserId,
    branchContributorHandle :: Maybe UserHandle
  }
  deriving stock (Show, Eq)

instance ToJSON BranchPayload where
  toJSON BranchPayload {branchId, branchName, branchShortHand, branchContributorUserId, branchContributorHandle, projectBranchShortHand} =
    Aeson.object
      [ "branchId" Aeson..= branchId,
        "branchName" Aeson..= branchName,
        "branchShortHand" Aeson..= branchShortHand,
        "projectBranchShortHand" Aeson..= projectBranchShortHand,
        "branchContributorUserId" Aeson..= branchContributorUserId,
        "branchContributorHandle" Aeson..= branchContributorHandle
      ]

instance FromJSON BranchPayload where
  parseJSON = Aeson.withObject "BranchPayload" \o -> do
    branchId <- o .: "branchId"
    branchName <- o .: "branchName"
    branchShortHand <- o .: "branchShortHand"
    projectBranchShortHand <- o .: "projectBranchShortHand"
    branchContributorUserId <- o .: "branchContributorUserId"
    branchContributorHandle <- o .: "branchContributorHandle"
    pure BranchPayload {branchId, branchName, branchShortHand, projectBranchShortHand, branchContributorUserId, branchContributorHandle}

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
  parseJSON = Aeson.withObject "ProjectPayload" \o -> do
    projectId <- o .: "projectId"
    projectSlug <- o .: "projectSlug"
    projectShortHand <- o .: "projectShortHand"
    projectOwnerHandle <- o .: "projectOwnerHandle"
    projectOwnerUserId <- o .: "projectOwnerUserId"
    pure ProjectPayload {projectId, projectSlug, projectShortHand, projectOwnerHandle, projectOwnerUserId}

data ProjectBranchUpdatedPayload = ProjectBranchUpdatedPayload
  { projectInfo :: ProjectPayload,
    branchInfo :: BranchPayload
  }
  deriving stock (Show, Eq)

instance ToJSON ProjectBranchUpdatedPayload where
  toJSON ProjectBranchUpdatedPayload {projectInfo, branchInfo} =
    Aeson.object
      [ "project" Aeson..= projectInfo,
        "branch" Aeson..= branchInfo
      ]

instance FromJSON ProjectBranchUpdatedPayload where
  parseJSON = Aeson.withObject "ProjectBranchUpdatedPayload" \o -> do
    projectInfo <- o .: "project"
    branchInfo <- o .: "branch"
    pure ProjectBranchUpdatedPayload {projectInfo, branchInfo}

data ContributionPayload = ContributionPayload
  { contributionId :: ContributionId,
    contributionNumber :: ContributionNumber,
    contributionTitle :: Text,
    contributionDescription :: Maybe Text,
    contributionStatus :: ContributionStatus,
    contributionAuthor :: UserDisplayInfo,
    contributionSourceBranch :: BranchPayload,
    contributionTargetBranch :: BranchPayload
  }
  deriving stock (Show, Eq)

instance ToJSON ContributionPayload where
  toJSON ContributionPayload {contributionId, contributionNumber, contributionTitle, contributionDescription, contributionStatus, contributionAuthor, contributionSourceBranch, contributionTargetBranch} =
    Aeson.object
      [ "contributionId" Aeson..= contributionId,
        "number" Aeson..= contributionNumber,
        "title" Aeson..= contributionTitle,
        "description" Aeson..= contributionDescription,
        "status" Aeson..= contributionStatus,
        "author" Aeson..= contributionAuthor,
        "sourceBranch" Aeson..= contributionSourceBranch,
        "targetBranch" Aeson..= contributionTargetBranch
      ]

instance FromJSON ContributionPayload where
  parseJSON = Aeson.withObject "ContributionPayload" \o -> do
    contributionId <- o .: "contributionId"
    contributionNumber <- o .: "number"
    contributionTitle <- o .: "title"
    contributionDescription <- o .: "description"
    contributionStatus <- o .: "status"
    contributionAuthor <- o .: "author"
    contributionSourceBranch <- o .: "sourceBranch"
    contributionTargetBranch <- o .: "targetBranch"
    pure ContributionPayload {contributionId, contributionNumber, contributionTitle, contributionDescription, contributionStatus, contributionAuthor, contributionSourceBranch, contributionTargetBranch}

data ProjectContributionCreatedPayload = ProjectContributionCreatedPayload
  { projectInfo :: ProjectPayload,
    contributionInfo :: ContributionPayload
  }
  deriving stock (Show, Eq)

instance ToJSON ProjectContributionCreatedPayload where
  toJSON ProjectContributionCreatedPayload {projectInfo, contributionInfo} =
    Aeson.object
      [ "project" Aeson..= projectInfo,
        "contribution" Aeson..= contributionInfo
      ]

instance FromJSON ProjectContributionCreatedPayload where
  parseJSON = Aeson.withObject "ProjectContributionCreatedPayload" \o -> do
    projectInfo <- o .: "project"
    contributionInfo <- o .: "contribution"
    pure ProjectContributionCreatedPayload {projectInfo, contributionInfo}

data HydratedEvent = HydratedEvent
  { hydratedEventPayload :: HydratedEventPayload,
    hydratedEventLink :: URI
  }
  deriving stock (Show, Eq)

instance ToJSON HydratedEvent where
  toJSON he@(HydratedEvent {hydratedEventPayload, hydratedEventLink}) =
    let kind :: Text = case hydratedEventTopic he of
          ProjectBranchUpdated -> "projectBranchUpdated"
          ProjectContributionCreated -> "projectContributionCreated"
        payload = case hydratedEventPayload of
          HydratedProjectBranchUpdatedPayload p -> Aeson.toJSON p
          HydratedProjectContributionCreatedPayload p -> Aeson.toJSON p
     in Aeson.object
          [ "payload" .= payload,
            "link" .= URIParam hydratedEventLink,
            "kind" .= kind
          ]

instance FromJSON HydratedEvent where
  parseJSON = Aeson.withObject "HydratedEvent" \o -> do
    kind <- o .: "kind"
    hydratedEventLink <- o .: "link"
    hydratedEventPayload <- case kind of
      "projectBranchUpdated" -> HydratedProjectBranchUpdatedPayload <$> o .: "payload"
      "projectContributionCreated" -> HydratedProjectContributionCreatedPayload <$> o .: "payload"
      _ -> fail $ "Unknown event kind: " <> Text.unpack kind
    pure HydratedEvent {hydratedEventPayload, hydratedEventLink}

data HydratedEventPayload
  = HydratedProjectBranchUpdatedPayload ProjectBranchUpdatedPayload
  | HydratedProjectContributionCreatedPayload ProjectContributionCreatedPayload
  deriving stock (Show, Eq)

hydratedEventTopic :: HydratedEvent -> NotificationTopic
hydratedEventTopic (HydratedEvent {hydratedEventPayload}) = case hydratedEventPayload of
  HydratedProjectBranchUpdatedPayload _ -> ProjectBranchUpdated
  HydratedProjectContributionCreatedPayload _ -> ProjectContributionCreated
