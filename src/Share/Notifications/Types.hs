module Share.Notifications.Types
  ( NotificationTopic (..),
    NotificationFilter (..),
    NotificationEventData,
    NotificationEvent (..),
    NewNotificationEvent,
    Subscription (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Share.IDs

data NotificationTopic
  = ProjectBranchPushed
  | ContributionSubmitted

newtype NotificationFilter = NotificationFilter (Map Text Text)

data NotificationEventData

-- | Description of a notifiable event.
data NotificationEvent id occurredAt = NotificationEvent
  { eventId :: id,
    eventOccurredAt :: occurredAt,
    eventTopic :: NotificationTopic,
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
