module Share.Notifications.Types
  ( NotificationTopic (..),
    NotificationFilter (..),
    NotificationEventData,
    NotificationEvent (..),
    NewNotificationEvent,
    Subscription (..),
    ProjectBranchData (..),
    ProjectContributionData (..),
    NotificationHubEntry (..),
    eventTopic,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Hasql.Decoders qualified as HasqlDecoders
import Hasql.Encoders qualified as HasqlEncoders
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

data NotificationTopic
  = ProjectBranchUpdated
  | ProjectContributionCreated

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

instance Aeson.ToJSON NotificationStatus where
  toJSON = \case
    Unread -> "unread"
    Read -> "read"
    Archived -> "archived"

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

type NewNotificationEvent = NotificationEvent () ()

data Subscription id = Subscription
  { subscriptionId :: id,
    subscriber :: UserId,
    subscriptionScope :: UserId,
    subscriptionTopic :: NotificationTopic,
    subscriptionFilter :: NotificationFilter
  }

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
