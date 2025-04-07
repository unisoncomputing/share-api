module Share.Notifications.Types
  ( NotificationTopic (..),
    NotificationFilter (..),
    NotificationEventData,
    NotificationEvent (..),
    NewNotificationEvent,
    NotificationSubscription (..),
    ProjectBranchData (..),
    ProjectContributionData (..),
    NotificationHubEntry (..),
    NotificationStatus (..),
    NotificationDeliveryMethod (..),
    NotificationEmailDeliveryConfig (..),
    NotificationWebhookDeliveryConfig (..),
    eventTopic,
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Hasql.Decoders qualified as HasqlDecoders
import Hasql.Encoders qualified as HasqlEncoders
import Hasql.Interpolate qualified as Hasql
import Network.URI (URI)
import Servant (FromHttpApiData (..))
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.URI (URIParam (..))

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

data ProjectBranchData = ProjectBranchData
  { projectId :: ProjectId,
    branchId :: BranchId,
    branchContributorUserId :: UserId
  }

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
    contributorUserId :: Maybe UserId
  }

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

data NotificationEventData
  = ProjectBranchUpdatedData ProjectBranchData
  | ProjectContributionCreatedData ProjectContributionData

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
data NotificationEvent id occurredAt = NotificationEvent
  { eventId :: id,
    eventOccurredAt :: occurredAt,
    eventData :: NotificationEventData,
    eventScope :: UserId
  }

instance Aeson.ToJSON (NotificationEvent NotificationEventId UTCTime) where
  toJSON NotificationEvent {eventId, eventOccurredAt, eventData, eventScope} =
    Aeson.object
      [ "id" Aeson..= eventId,
        "occurredAt" Aeson..= eventOccurredAt,
        "data" Aeson..= eventData,
        "scope" Aeson..= eventScope
      ]

instance Hasql.DecodeRow (NotificationEvent NotificationEventId UTCTime) where
  decodeRow = do
    eventId <- PG.decodeField
    eventOccurredAt <- PG.decodeField
    eventScope <- PG.decodeField
    eventData <- PG.decodeRow
    pure $ NotificationEvent {eventId, eventOccurredAt, eventData, eventScope}

type NewNotificationEvent = NotificationEvent () ()

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

data NotificationWebhookDeliveryConfig = NotificationWebhookConfig
  { webhookDeliveryId :: NotificationWebhookId,
    webhookDeliveryUrl :: URI
  }
  deriving (Eq, Ord, Show)

instance PG.DecodeRow NotificationWebhookDeliveryConfig where
  decodeRow = do
    webhookDeliveryId <- PG.decodeField
    URIParam webhookDeliveryUrl <- PG.decodeField
    pure $ NotificationWebhookConfig {webhookDeliveryId, webhookDeliveryUrl}

instance Aeson.ToJSON NotificationWebhookDeliveryConfig where
  toJSON NotificationWebhookConfig {webhookDeliveryId, webhookDeliveryUrl} =
    Aeson.object
      [ "id" Aeson..= webhookDeliveryId,
        "url" Aeson..= show webhookDeliveryUrl
      ]

data NotificationDeliveryMethod
  = EmailDeliveryMethod NotificationEmailDeliveryConfig
  | WebhookDeliveryMethod NotificationWebhookDeliveryConfig
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON NotificationDeliveryMethod where
  toJSON = \case
    EmailDeliveryMethod config -> Aeson.object ["kind" .= ("email" :: Text), "config" .= config]
    WebhookDeliveryMethod config -> Aeson.object ["kind" .= ("webhook" :: Text), "config" .= config]

data NotificationSubscription id = NotificationSubscription
  { subscriptionId :: id,
    subscriptionScope :: UserId,
    subscriptionTopic :: Set NotificationTopic,
    subscriptionFilter :: NotificationFilter
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

data NotificationHubEntry = NotificationHubEntry
  { hubEntryId :: NotificationHubEntryId,
    hubEntryEvent :: NotificationEvent NotificationEventId UTCTime,
    hubEntryStatus :: NotificationStatus
  }

instance Aeson.ToJSON NotificationHubEntry where
  toJSON NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus} =
    Aeson.object
      [ "id" Aeson..= hubEntryId,
        "event" Aeson..= hubEntryEvent,
        "status" Aeson..= hubEntryStatus
      ]

instance Hasql.DecodeRow NotificationHubEntry where
  decodeRow = do
    hubEntryId <- PG.decodeField
    hubEntryStatus <- PG.decodeField
    hubEntryEvent <- PG.decodeRow
    pure $ NotificationHubEntry {hubEntryId, hubEntryEvent, hubEntryStatus}
