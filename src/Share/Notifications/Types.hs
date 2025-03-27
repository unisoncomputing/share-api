module Share.Notifications.Types
  ( NotificationTopic (..),
    NotificationFilter (..),
    NotificationEventData,
    NotificationEvent (..),
    NewNotificationEvent,
    Subscription (..),
    ProjectBranchData (..),
    ProjectContributionData (..),
    eventTopic,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
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

type NewNotificationEvent = NotificationEvent () ()

data Subscription id = Subscription
  { subscriptionId :: id,
    subscriber :: UserId,
    subscriptionScope :: UserId,
    subscriptionTopic :: NotificationTopic,
    subscriptionFilter :: NotificationFilter
  }
