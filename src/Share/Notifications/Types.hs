{-# LANGUAGE DataKinds #-}
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
    ProjectData (..),
    BranchData (..),
    ContributionData (..),
    TicketData (..),
    CommentData (..),
    ReleaseData (..),
    StatusUpdateData (..),
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
    TicketPayload (..),
    CommentPayload (..),
    ReleasePayload (..),
    StatusUpdatePayload (..),
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
import Share.Ticket (TicketStatus)
import Share.Utils.API
import Share.Utils.URI (URIParam (..))
import Share.Web.Share.DisplayInfo.Types (UserDisplayInfo)

data NotificationTopic
  = ProjectBranchUpdated
  | ProjectContributionCreated
  | ProjectContributionStatusUpdated
  | ProjectContributionComment
  | ProjectTicketCreated
  | ProjectTicketStatusUpdated
  | ProjectTicketComment
  | ProjectReleaseCreated
  deriving (Eq, Show, Ord)

instance PG.EncodeValue NotificationTopic where
  encodeValue = HasqlEncoders.enum \case
    ProjectBranchUpdated -> "project:branch:updated"
    ProjectContributionCreated -> "project:contribution:created"
    ProjectContributionStatusUpdated -> "project:contribution:updated"
    ProjectContributionComment -> "project:contribution:comment"
    ProjectTicketCreated -> "project:ticket:created"
    ProjectTicketStatusUpdated -> "project:ticket:updated"
    ProjectTicketComment -> "project:ticket:comment"
    ProjectReleaseCreated -> "project:release:created"

instance PG.DecodeValue NotificationTopic where
  decodeValue = HasqlDecoders.enum \case
    "project:branch:updated" -> Just ProjectBranchUpdated
    "project:contribution:created" -> Just ProjectContributionCreated
    "project:contribution:updated" -> Just ProjectContributionStatusUpdated
    "project:contribution:comment" -> Just ProjectContributionComment
    "project:ticket:created" -> Just ProjectTicketCreated
    "project:ticket:updated" -> Just ProjectTicketStatusUpdated
    "project:ticket:comment" -> Just ProjectTicketComment
    "project:release:created" -> Just ProjectReleaseCreated
    _ -> Nothing

instance Aeson.ToJSON NotificationTopic where
  toJSON = \case
    ProjectBranchUpdated -> "project:branch:updated"
    ProjectContributionCreated -> "project:contribution:created"
    ProjectContributionStatusUpdated -> "project:contribution:updated"
    ProjectContributionComment -> "project:contribution:comment"
    ProjectTicketCreated -> "project:ticket:created"
    ProjectTicketStatusUpdated -> "project:ticket:updated"
    ProjectTicketComment -> "project:ticket:comment"
    ProjectReleaseCreated -> "project:release:created"

instance Aeson.FromJSON NotificationTopic where
  parseJSON = Aeson.withText "NotificationTopic" \case
    "project:branch:updated" -> pure ProjectBranchUpdated
    "project:contribution:created" -> pure ProjectContributionCreated
    "project:contribution:updated" -> pure ProjectContributionStatusUpdated
    "project:contribution:comment" -> pure ProjectContributionComment
    "project:ticket:created" -> pure ProjectTicketCreated
    "project:ticket:updated" -> pure ProjectTicketStatusUpdated
    "project:ticket:comment" -> pure ProjectTicketComment
    "project:release:created" -> pure ProjectReleaseCreated
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

data BranchData = BranchData
  { branchId :: BranchId,
    branchContributorUserId :: Maybe UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON BranchData where
  toJSON BranchData {branchId, branchContributorUserId} =
    Aeson.object
      [ "branchId" .= branchId,
        "branchContributorUserId" .= branchContributorUserId
      ]

instance Aeson.FromJSON BranchData where
  parseJSON = Aeson.withObject "ProjectBranchData" \o -> do
    branchId <- o .: "branchId"
    branchContributorUserId <- o .: "branchContributorUserId"
    pure BranchData {branchId, branchContributorUserId}

data CommentData = CommentData
  { commentId :: CommentId,
    commentAuthorUserId :: UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON CommentData where
  toJSON CommentData {commentAuthorUserId, commentId} =
    Aeson.object
      [ "commentId" .= commentId,
        "commentAuthorUserId" .= commentAuthorUserId
      ]

instance Aeson.FromJSON CommentData where
  parseJSON = Aeson.withObject "CommentData" \o -> do
    commentId <- o .: "commentId"
    commentAuthorUserId <- o .: "commentAuthorUserId"
    pure CommentData {commentId, commentAuthorUserId}

data ReleaseData = ReleaseData
  { releaseId :: ReleaseId,
    releaseVersion :: ReleaseVersion
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ReleaseData where
  toJSON ReleaseData {releaseId, releaseVersion} =
    Aeson.object
      [ "releaseId" .= releaseId,
        "releaseVersion" .= releaseVersion
      ]

instance Aeson.FromJSON ReleaseData where
  parseJSON = Aeson.withObject "ReleaseData" \o -> do
    releaseId <- o .: "releaseId"
    releaseVersion <- o .: "releaseVersion"
    pure ReleaseData {releaseId, releaseVersion}

data TicketData = TicketData
  { ticketId :: TicketId,
    ticketAuthorUserId :: UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON TicketData where
  toJSON TicketData {ticketId, ticketAuthorUserId} =
    Aeson.object
      [ "ticketId" .= ticketId,
        "ticketAuthorUserId" .= ticketAuthorUserId
      ]

instance Aeson.FromJSON TicketData where
  parseJSON = Aeson.withObject "ProjectTicketData" \o -> do
    ticketId <- o .: "ticketId"
    ticketAuthorUserId <- o .: "ticketAuthorUserId"
    pure TicketData {ticketId, ticketAuthorUserId}

data ProjectData = ProjectData
  { projectId :: ProjectId,
    public :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ProjectData where
  toJSON ProjectData {projectId, public} =
    Aeson.object
      [ "projectId" .= projectId,
        "public" .= public
      ]

instance Aeson.FromJSON ProjectData where
  parseJSON = Aeson.withObject "ProjectData" \o -> do
    projectId <- o .: "projectId"
    public <- o .: "public"
    pure ProjectData {projectId, public}

data ContributionData = ContributionData
  { contributionId :: ContributionId,
    fromBranchId :: BranchId,
    toBranchId :: BranchId,
    contributorUserId :: UserId
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ContributionData where
  toJSON ContributionData {contributionId, fromBranchId, toBranchId, contributorUserId} =
    Aeson.object
      [ "contributionId" .= contributionId,
        "fromBranchId" .= fromBranchId,
        "toBranchId" .= toBranchId,
        "contributorUserId" .= contributorUserId
      ]

instance Aeson.FromJSON ContributionData where
  parseJSON = Aeson.withObject "ProjectContributionData" \o -> do
    contributionId <- o .: "contributionId"
    fromBranchId <- o .: "fromBranchId"
    toBranchId <- o .: "toBranchId"
    contributorUserId <- o .: "contributorUserId"
    pure ContributionData {contributionId, fromBranchId, toBranchId, contributorUserId}

data StatusUpdateData status
  = StatusUpdateData
  { oldStatus :: status,
    newStatus :: status
  }
  deriving (Eq, Show)

instance (ToJSON status) => ToJSON (StatusUpdateData status) where
  toJSON StatusUpdateData {oldStatus, newStatus} =
    Aeson.object
      [ "oldStatus" .= oldStatus,
        "newStatus" .= newStatus
      ]

instance (FromJSON status) => FromJSON (StatusUpdateData status) where
  parseJSON = Aeson.withObject "StatusUpdateData" \o -> do
    oldStatus <- o .: "oldStatus"
    newStatus <- o .: "newStatus"
    pure StatusUpdateData {oldStatus, newStatus}

-- The bare-bones Notification Event Data that's actually stored in the database.
-- It holds unhydrated IDs.
data NotificationEventData
  = ProjectBranchUpdatedData ProjectData BranchData
  | ProjectContributionCreatedData ProjectData ContributionData
  | ProjectContributionStatusUpdatedData ProjectData ContributionData (StatusUpdateData ContributionStatus)
  | ProjectContributionCommentData ProjectData ContributionData CommentData
  | ProjectTicketCreatedData ProjectData TicketData
  | ProjectTicketStatusUpdatedData ProjectData TicketData (StatusUpdateData TicketStatus)
  | ProjectTicketCommentData ProjectData TicketData CommentData
  | ProjectReleaseCreatedData ProjectData ReleaseData
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
        ProjectBranchUpdatedData project branch -> Aeson.toJSON (project :++ branch)
        ProjectContributionCreatedData project c -> Aeson.toJSON (project :++ c)
        ProjectContributionStatusUpdatedData project contr status -> Aeson.toJSON (project :++ contr :++ status)
        ProjectContributionCommentData project contr comm -> Aeson.toJSON (project :++ contr :++ comm)
        ProjectTicketCreatedData project ticket -> Aeson.toJSON (project :++ ticket)
        ProjectTicketStatusUpdatedData project ticket status -> Aeson.toJSON (project :++ ticket :++ status)
        ProjectTicketCommentData project ticket comm -> Aeson.toJSON (project :++ ticket :++ comm)
        ProjectReleaseCreatedData project release -> Aeson.toJSON (project :++ release)

instance PG.EncodeValue NotificationEventData where
  encodeValue =
    HasqlEncoders.jsonb
      & contramap \case
        ProjectBranchUpdatedData project branch -> Aeson.toJSON (project :++ branch)
        ProjectContributionCreatedData project contr -> Aeson.toJSON (project :++ contr)
        ProjectContributionStatusUpdatedData project contr status -> Aeson.toJSON (project :++ contr :++ status)
        ProjectContributionCommentData project contr comm -> Aeson.toJSON (project :++ contr :++ comm)
        ProjectTicketCreatedData project ticket -> Aeson.toJSON (project :++ ticket)
        ProjectTicketStatusUpdatedData project ticket status -> Aeson.toJSON (project :++ ticket :++ status)
        ProjectTicketCommentData project ticket comm -> Aeson.toJSON (project :++ ticket :++ comm)
        ProjectReleaseCreatedData project release -> Aeson.toJSON (project :++ release)

instance Hasql.DecodeRow NotificationEventData where
  decodeRow = do
    topic <- PG.decodeField
    Hasql.Jsonb jsonData <- PG.decodeField
    case topic of
      ProjectBranchUpdated -> do
        (project :++ branch) <- parseJsonData jsonData
        pure $ ProjectBranchUpdatedData project branch
      ProjectContributionCreated -> do
        (project :++ contr) <- parseJsonData jsonData
        pure $ ProjectContributionCreatedData project contr
      ProjectContributionStatusUpdated -> do
        (project :++ contr :++ status) <- parseJsonData jsonData
        pure $ ProjectContributionStatusUpdatedData project contr status
      ProjectContributionComment -> do
        (project :++ contr :++ comm) <- parseJsonData jsonData
        pure $ ProjectContributionCommentData project contr comm
      ProjectTicketCreated -> do
        (project :++ ticket) <- parseJsonData jsonData
        pure $ ProjectTicketCreatedData project ticket
      ProjectTicketStatusUpdated -> do
        (project :++ ticket :++ status) <- parseJsonData jsonData
        pure $ ProjectTicketStatusUpdatedData project ticket status
      ProjectTicketComment -> do
        (project :++ ticket :++ comm) <- parseJsonData jsonData
        pure $ ProjectTicketCommentData project ticket comm
      ProjectReleaseCreated -> do
        (project :++ release) <- parseJsonData jsonData
        pure $ ProjectReleaseCreatedData project release
    where
      parseJsonData v = case Aeson.fromJSON v of
        Aeson.Error e -> fail e
        Aeson.Success a -> pure a

eventTopic :: NotificationEventData -> NotificationTopic
eventTopic = \case
  ProjectBranchUpdatedData {} -> ProjectBranchUpdated
  ProjectContributionCreatedData {} -> ProjectContributionCreated
  ProjectContributionStatusUpdatedData {} -> ProjectContributionStatusUpdated
  ProjectContributionCommentData {} -> ProjectContributionComment
  ProjectTicketCreatedData {} -> ProjectTicketCreated
  ProjectTicketStatusUpdatedData {} -> ProjectTicketStatusUpdated
  ProjectTicketCommentData {} -> ProjectTicketComment
  ProjectReleaseCreatedData {} -> ProjectReleaseCreated

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

data CommentPayload = CommentPayload
  { commentId :: CommentId,
    commentContent :: Text,
    commentCreatedAt :: UTCTime,
    commentAuthor :: UserDisplayInfo
  }
  deriving stock (Show, Eq)

instance ToJSON CommentPayload where
  toJSON CommentPayload {commentId, commentContent, commentCreatedAt, commentAuthor} =
    Aeson.object
      [ "commentId" Aeson..= commentId,
        "content" Aeson..= commentContent,
        "createdAt" Aeson..= commentCreatedAt,
        "author" Aeson..= commentAuthor
      ]

instance FromJSON CommentPayload where
  parseJSON = Aeson.withObject "CommentPayload" \o -> do
    commentId <- o .: "commentId"
    commentContent <- o .: "content"
    commentCreatedAt <- o .: "createdAt"
    commentAuthor <- o .: "author"
    pure CommentPayload {commentId, commentContent, commentCreatedAt, commentAuthor}

data TicketPayload = TicketPayload
  { ticketId :: TicketId,
    ticketTitle :: Text,
    ticketNumber :: TicketNumber,
    ticketDescription :: Maybe Text,
    ticketStatus :: TicketStatus,
    ticketAuthor :: UserDisplayInfo,
    ticketCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance ToJSON TicketPayload where
  toJSON TicketPayload {ticketId, ticketTitle, ticketNumber, ticketDescription, ticketStatus, ticketAuthor, ticketCreatedAt} =
    Aeson.object
      [ "ticketId" Aeson..= ticketId,
        "title" Aeson..= ticketTitle,
        "number" Aeson..= ticketNumber,
        "description" Aeson..= ticketDescription,
        "status" Aeson..= ticketStatus,
        "author" Aeson..= ticketAuthor,
        "createdAt" Aeson..= ticketCreatedAt
      ]

instance FromJSON TicketPayload where
  parseJSON = Aeson.withObject "TicketPayload" \o -> do
    ticketId <- o .: "ticketId"
    ticketTitle <- o .: "title"
    ticketNumber <- o .: "number"
    ticketDescription <- o .: "description"
    ticketStatus <- o .: "status"
    ticketAuthor <- o .: "author"
    ticketCreatedAt <- o .: "createdAt"
    pure TicketPayload {ticketId, ticketTitle, ticketNumber, ticketDescription, ticketStatus, ticketAuthor, ticketCreatedAt}

data ReleasePayload = ReleasePayload
  { releaseId :: ReleaseId,
    releaseVersion :: ReleaseVersion,
    releaseCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance ToJSON ReleasePayload where
  toJSON ReleasePayload {releaseId, releaseVersion, releaseCreatedAt} =
    Aeson.object
      [ "releaseId" Aeson..= releaseId,
        "version" Aeson..= releaseVersion,
        "createdAt" Aeson..= releaseCreatedAt
      ]

instance FromJSON ReleasePayload where
  parseJSON = Aeson.withObject "ReleasePayload" \o -> do
    releaseId <- o .: "releaseId"
    releaseVersion <- o .: "version"
    releaseCreatedAt <- o .: "createdAt"
    pure ReleasePayload {releaseId, releaseVersion, releaseCreatedAt}

data ContributionPayload = ContributionPayload
  { contributionId :: ContributionId,
    contributionNumber :: ContributionNumber,
    contributionTitle :: Text,
    contributionDescription :: Maybe Text,
    contributionStatus :: ContributionStatus,
    contributionAuthor :: UserDisplayInfo,
    contributionSourceBranch :: BranchPayload,
    contributionTargetBranch :: BranchPayload,
    contributionCreatedAt :: UTCTime
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
    contributionCreatedAt <- o .: "createdAt"
    pure ContributionPayload {contributionId, contributionNumber, contributionTitle, contributionDescription, contributionStatus, contributionAuthor, contributionSourceBranch, contributionTargetBranch, contributionCreatedAt}

data StatusUpdatePayload status
  = StatusUpdatePayload
  { oldStatus :: status,
    newStatus :: status
  }
  deriving (Eq, Show)

instance (ToJSON status) => ToJSON (StatusUpdatePayload status) where
  toJSON StatusUpdatePayload {oldStatus, newStatus} =
    Aeson.object
      [ "oldStatus" .= oldStatus,
        "newStatus" .= newStatus
      ]

instance (FromJSON status) => FromJSON (StatusUpdatePayload status) where
  parseJSON = Aeson.withObject "StatusUpdatedPayload" \o -> do
    oldStatus <- o .: "oldStatus"
    newStatus <- o .: "newStatus"
    pure StatusUpdatePayload {oldStatus, newStatus}

data HydratedEvent = HydratedEvent
  { hydratedEventPayload :: HydratedEventPayload,
    hydratedEventLink :: URI
  }
  deriving stock (Show, Eq)

instance ToJSON HydratedEvent where
  toJSON he@(HydratedEvent {hydratedEventPayload, hydratedEventLink}) =
    let kind = hydratedEventTopic he
        payload = case hydratedEventPayload of
          HydratedProjectBranchUpdatedPayload p b ->
            Aeson.object ["project" .= p, "branch" .= b]
          HydratedProjectContributionCreatedPayload p c ->
            Aeson.object ["project" .= p, "contribution" .= c]
          HydratedProjectContributionStatusUpdatedPayload p c status ->
            Aeson.object ["project" .= p, "contribution" .= c, "status_update" .= status]
          HydratedProjectContributionCommentPayload p c comm ->
            Aeson.object ["project" .= p, "contribution" .= c, "comment" .= comm]
          HydratedProjectTicketCreatedPayload p t ->
            Aeson.object ["project" .= p, "ticket" .= t]
          HydratedProjectTicketStatusUpdatedPayload p t status ->
            Aeson.object ["project" .= p, "ticket" .= t, "status_update" .= status]
          HydratedProjectTicketCommentPayload p t comm ->
            Aeson.object ["project" .= p, "ticket" .= t, "comment" .= comm]
          HydratedProjectReleaseCreatedPayload p r ->
            Aeson.object ["project" .= p, "release" .= r]
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
      ProjectBranchUpdated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        branch <- payload .: "branch"
        pure $ HydratedProjectBranchUpdatedPayload project branch
      ProjectContributionCreated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        contribution <- payload .: "contribution"
        pure $ HydratedProjectContributionCreatedPayload project contribution
      ProjectContributionStatusUpdated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        contribution <- payload .: "contribution"
        status <- payload .: "status_update"
        pure $ HydratedProjectContributionStatusUpdatedPayload project contribution status
      ProjectContributionComment -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        contribution <- payload .: "contribution"
        comment <- payload .: "comment"
        pure $ HydratedProjectContributionCommentPayload project contribution comment
      ProjectTicketCreated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        ticket <- payload .: "ticket"
        pure $ HydratedProjectTicketCreatedPayload project ticket
      ProjectTicketStatusUpdated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        ticket <- payload .: "ticket"
        statusUpdate <- payload .: "status_update"
        pure $ HydratedProjectTicketStatusUpdatedPayload project ticket statusUpdate
      ProjectTicketComment -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        ticket <- payload .: "ticket"
        comment <- payload .: "comment"
        pure $ HydratedProjectTicketCommentPayload project ticket comment
      ProjectReleaseCreated -> do
        payload <- o .: "payload"
        project <- payload .: "project"
        release <- payload .: "release"
        pure $ HydratedProjectReleaseCreatedPayload project release

    pure HydratedEvent {hydratedEventPayload, hydratedEventLink}

data HydratedEventPayload
  = HydratedProjectBranchUpdatedPayload ProjectPayload BranchPayload
  | HydratedProjectContributionCreatedPayload ProjectPayload ContributionPayload
  | HydratedProjectContributionStatusUpdatedPayload ProjectPayload ContributionPayload (StatusUpdatePayload ContributionStatus)
  | HydratedProjectContributionCommentPayload ProjectPayload ContributionPayload CommentPayload
  | HydratedProjectTicketCreatedPayload ProjectPayload TicketPayload
  | HydratedProjectTicketStatusUpdatedPayload ProjectPayload TicketPayload (StatusUpdatePayload TicketStatus)
  | HydratedProjectTicketCommentPayload ProjectPayload TicketPayload CommentPayload
  | HydratedProjectReleaseCreatedPayload ProjectPayload ReleasePayload
  deriving stock (Show, Eq)

hydratedEventTopic :: HydratedEvent -> NotificationTopic
hydratedEventTopic (HydratedEvent {hydratedEventPayload}) = case hydratedEventPayload of
  HydratedProjectBranchUpdatedPayload {} -> ProjectBranchUpdated
  HydratedProjectContributionCreatedPayload {} -> ProjectContributionCreated
  HydratedProjectContributionStatusUpdatedPayload {} -> ProjectContributionStatusUpdated
  HydratedProjectContributionCommentPayload {} -> ProjectContributionComment
  HydratedProjectTicketCreatedPayload {} -> ProjectTicketCreated
  HydratedProjectTicketStatusUpdatedPayload {} -> ProjectTicketStatusUpdated
  HydratedProjectTicketCommentPayload {} -> ProjectTicketComment
  HydratedProjectReleaseCreatedPayload {} -> ProjectReleaseCreated
