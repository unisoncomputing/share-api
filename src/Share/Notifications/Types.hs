module Share.Notifications.Types
  ( NotificationTopic (..),
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
    BranchPayload (..),
    ProjectPayload (..),
    ProjectBranchPayload (..),
    ProjectContributionPayload (..),
    eventTopic,
    hydratedEventTopic,
    eventData_,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON (..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (FromJSON)
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

newtype NotificationFilter = NotificationFilter (Map Text Text)
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
    branchContributorUserId :: Maybe UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ProjectBranchData where
  toJSON ProjectBranchData {projectId, branchId, branchContributorUserId} =
    Aeson.object
      [ "projectId" .= projectId,
        "branchId" .= branchId,
        "branchContributorUserId" .= branchContributorUserId
      ]

instance Aeson.FromJSON ProjectBranchData where
  parseJSON = Aeson.withObject "ProjectBranchData" \o -> do
    projectId <- o .: "projectId"
    branchId <- o .: "branchId"
    branchContributorUserId <- o .: "branchContributorUserId"
    pure ProjectBranchData {projectId, branchId, branchContributorUserId}

data ProjectContributionData = ProjectContributionData
  { projectId :: ProjectId,
    contributionId :: ContributionId,
    fromBranchId :: BranchId,
    toBranchId :: BranchId,
    contributorUserId :: UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ProjectContributionData where
  toJSON ProjectContributionData {projectId, contributionId, fromBranchId, toBranchId, contributorUserId} =
    Aeson.object
      [ "projectId" .= projectId,
        "contributionId" .= contributionId,
        "fromBranchId" .= fromBranchId,
        "toBranchId" .= toBranchId,
        "contributorUserId" .= contributorUserId
      ]

instance Aeson.FromJSON ProjectContributionData where
  parseJSON = Aeson.withObject "ProjectContributionData" \o -> do
    projectId <- o .: "projectId"
    contributionId <- o .: "contributionId"
    fromBranchId <- o .: "fromBranchId"
    toBranchId <- o .: "toBranchId"
    contributorUserId <- o .: "contributorUserId"
    pure ProjectContributionData {projectId, contributionId, fromBranchId, toBranchId, contributorUserId}

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
data NotificationEvent id occurredAt eventPayload = NotificationEvent
  { eventId :: id,
    eventOccurredAt :: occurredAt,
    eventResourceId :: ResourceId,
    eventData :: eventPayload,
    eventScope :: UserId
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

eventData_ :: Lens (NotificationEvent id occurredAt eventPayload) (NotificationEvent id occurredAt eventPayload') eventPayload eventPayload'
eventData_ f event = (\eventData -> event {eventData}) <$> f (eventData event)

instance (Aeson.ToJSON eventPayload) => Aeson.ToJSON (NotificationEvent NotificationEventId UTCTime eventPayload) where
  toJSON NotificationEvent {eventId, eventOccurredAt, eventData, eventScope} =
    Aeson.object
      [ "id" Aeson..= eventId,
        "occurredAt" Aeson..= eventOccurredAt,
        "data" Aeson..= eventData,
        "scope" Aeson..= eventScope
      ]

instance Hasql.DecodeRow (NotificationEvent NotificationEventId UTCTime NotificationEventData) where
  decodeRow = do
    eventId <- PG.decodeField
    eventOccurredAt <- PG.decodeField
    eventScope <- PG.decodeField
    eventResourceId <- PG.decodeField
    eventData <- PG.decodeRow
    pure $ NotificationEvent {eventId, eventOccurredAt, eventData, eventScope, eventResourceId}

type NewNotificationEvent = NotificationEvent () () NotificationEventData

type PGNotificationEvent = NotificationEvent NotificationEventId UTCTime NotificationEventData

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
    subscriptionTopic :: Set NotificationTopic,
    subscriptionFilter :: Maybe NotificationFilter
  }

instance PG.DecodeRow (NotificationSubscription NotificationSubscriptionId) where
  decodeRow = do
    subscriptionId <- PG.decodeField
    subscriptionScope <- PG.decodeField
    subscriptionTopic <- Set.fromList <$> PG.decodeField
    subscriptionFilter <- PG.decodeField
    pure $ NotificationSubscription {subscriptionId, subscriptionScope, subscriptionTopic, subscriptionFilter}

instance Aeson.ToJSON (NotificationSubscription NotificationSubscriptionId) where
  toJSON NotificationSubscription {subscriptionId, subscriptionScope, subscriptionTopic, subscriptionFilter} =
    Aeson.object
      [ "id" Aeson..= subscriptionId,
        "scope" Aeson..= subscriptionScope,
        "topic" Aeson..= subscriptionTopic,
        "filter" Aeson..= subscriptionFilter
      ]

data NotificationHubEntry eventPayload = NotificationHubEntry
  { hubEntryId :: NotificationHubEntryId,
    hubEntryEvent :: NotificationEvent NotificationEventId UTCTime eventPayload,
    hubEntryStatus :: NotificationStatus
  }
  deriving stock (Functor, Foldable, Traversable)

instance (Aeson.ToJSON eventPayload) => Aeson.ToJSON (NotificationHubEntry eventPayload) where
  toJSON NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus} =
    Aeson.object
      [ "id" Aeson..= hubEntryId,
        "event" Aeson..= hubEntryEvent,
        "status" Aeson..= hubEntryStatus
      ]

instance Hasql.DecodeRow (NotificationHubEntry NotificationEventData) where
  decodeRow = do
    hubEntryId <- PG.decodeField
    hubEntryStatus <- PG.decodeField
    hubEntryEvent <- PG.decodeRow
    pure $ NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus}

newtype SubscriptionFilter = SubscriptionFilter (Aeson.Value)
  deriving (Hasql.EncodeValue, Hasql.DecodeValue) via Hasql.Jsonb
  deriving newtype (Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

-- Hydrated types

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

data HydratedEventPayload
  = ProjectBranchUpdatedPayload ProjectBranchPayload
  | ProjectContributionCreatedPayload ProjectContributionPayload
  deriving stock (Show, Eq)

hydratedEventTopic :: HydratedEventPayload -> NotificationTopic
hydratedEventTopic = \case
  ProjectBranchUpdatedPayload _ -> ProjectBranchUpdated
  ProjectContributionCreatedPayload _ -> ProjectContributionCreated

instance ToJSON HydratedEventPayload where
  toJSON = \case
    (ProjectBranchUpdatedPayload payload) ->
      toJSON payload & \case
        Aeson.Object o -> Aeson.Object (o <> KeyMap.singleton "kind" (Aeson.String "projectBranchUpdated"))
        _ -> error "Expected JSON object for ProjectBranchUpdatedPayload"
    (ProjectContributionCreatedPayload payload) ->
      toJSON payload & \case
        Aeson.Object o -> Aeson.Object (o <> KeyMap.singleton "kind" (Aeson.String "projectContributionCreated"))
        _ -> error "Expected JSON object for ProjectContributionCreatedPayload"

instance FromJSON HydratedEventPayload where
  parseJSON = Aeson.withObject "HydratedEventPayload" $ \o -> do
    kind <- o Aeson..: "kind"
    case kind of
      "projectBranchUpdated" -> ProjectBranchUpdatedPayload <$> Aeson.parseJSON (Aeson.Object o)
      "projectContributionCreated" -> ProjectContributionCreatedPayload <$> Aeson.parseJSON (Aeson.Object o)
      _ -> fail $ "Unknown kind: " <> Text.unpack kind
