module Share.Notifications.Ops
  ( listNotificationDeliveryMethods,
    addWebhookDeliveryMethod,
    updateWebhookDeliveryMethod,
    deleteWebhookDeliveryMethod,
    listProjectWebhooks,
    createProjectWebhook,
    deleteProjectWebhook,
    updateProjectWebhook,
    expectProjectWebhook,
    hydrateEvent,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types
import Share.Notifications.Webhooks.Secrets (WebhookConfig (..))
import Share.Notifications.Webhooks.Secrets qualified as WebhookSecrets
import Share.Postgres qualified as PG
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Project (Project (..))
import Share.Utils.URI (URIParam (..))
import Share.Web.App (WebApp)
import Share.Web.Share.Projects.Types (ProjectWebhook (..), ProjectWebhookTopics (..))
import Share.Web.UI.Links qualified as Links

listNotificationDeliveryMethods :: UserId -> Maybe NotificationSubscriptionId -> WebApp [NotificationDeliveryMethod]
listNotificationDeliveryMethods userId maySubscriptionId = do
  PG.runTransactionOrRespondError do
    emailDeliveryMethods <- NotifQ.listEmailDeliveryMethods userId maySubscriptionId
    -- Only lists webhooks on subscriptions owned by this user.
    webhookIds <- NotifQ.listWebhooks userId maySubscriptionId
    webhookDeliveryMethods <- for webhookIds \webhookId -> do
      WebhookSecrets.fetchWebhookConfig webhookId >>= \case
        Left err -> throwError err
        Right (WebhookConfig {uri = URIParam uri}) -> do
          pure $ (NotificationWebhookConfig webhookId uri)
    pure $ (EmailDeliveryMethod <$> emailDeliveryMethods) <> (WebhookDeliveryMethod <$> webhookDeliveryMethods)

-- | Note that this doesn't check that the caller is allowed to add a delivery method to the
-- given subscription, the caller is responsible for having resolved the subscription in a way
-- which proves that.
addWebhookDeliveryMethod :: URIParam -> Text -> NotificationSubscriptionId -> PG.Transaction e NotificationWebhookId
addWebhookDeliveryMethod uriParam webhookName notificationSubscriptionId = do
  NotifQ.createWebhookDeliveryMethod webhookName uriParam notificationSubscriptionId

-- | Delete a webhook delivery method, if it's owned by the given subscriber.
deleteWebhookDeliveryMethod :: SubscriptionOwner -> NotificationWebhookId -> WebApp ()
deleteWebhookDeliveryMethod owner webhookDeliveryMethodId = do
  let ownerFilter = case owner of
        UserSubscriptionOwner userId -> [PG.sql| ns.subscriber_user_id = #{userId} |]
        ProjectSubscriptionOwner projectId -> [PG.sql| ns.subscriber_project_id = #{projectId} |]
  isValid <- PG.runTransaction $ do
    PG.queryExpect1Col
      [PG.sql|
        SELECT EXISTS(
          SELECT FROM notification_webhooks nw
            JOIN notification_subscriptions ns
              ON nw.subscription_id = ns.id
            WHERE nw.id = #{webhookDeliveryMethodId}
              AND ^{ownerFilter}
        )
      |]
  when isValid $ do
    PG.runTransaction $ NotifQ.deleteWebhookDeliveryMethod owner webhookDeliveryMethodId

hydrateEvent :: HydratedEventPayload -> PG.Transaction e HydratedEvent
hydrateEvent hydratedEventPayload = do
  hydratedEventLink <- Links.notificationLink hydratedEventPayload
  pure $ HydratedEvent {hydratedEventPayload, hydratedEventLink}

-- | We provide a wrapper layer on top of notification subscriptions and webhooks
-- to make the frontend experience a bit more intuitive.
listProjectWebhooks :: ProjectId -> WebApp [ProjectWebhook]
listProjectWebhooks projectId = do
  results <- PG.runTransactionOrRespondError do
    -- Only lists webhooks on subscriptions owned by this project.
    projectWebhooks <- NotifQ.listProjectWebhooks projectId
    projectWebhooks
      & traversed . _1 %%~ \webhookId -> do
        WebhookSecrets.fetchWebhookConfig webhookId >>= \case
          Left err -> throwError err
          Right (WebhookConfig {uri = URIParam uri}) -> do
            pure $ (NotificationWebhookConfig webhookId uri)
  let webhooks =
        results <&> \(NotificationWebhookConfig {webhookDeliveryUrl = url}, _name, NotificationSubscription {subscriptionTopics, subscriptionTopicGroups, subscriptionId, subscriptionCreatedAt, subscriptionUpdatedAt}) ->
          let webhookTopics = case (Set.toList subscriptionTopicGroups, NESet.nonEmptySet subscriptionTopics) of
                ([], Just topics) -> SelectedTopics topics
                _ -> AllTopicsInProject
           in ProjectWebhook
                { projectWebhookUri = URIParam url,
                  projectWebhookTopics = webhookTopics,
                  projectWebhookNotificationSubscriptionId = subscriptionId,
                  projectWebhookCreatedAt = subscriptionCreatedAt,
                  projectWebhookUpdatedAt = subscriptionUpdatedAt
                }
  pure webhooks

createProjectWebhook :: ProjectId -> URIParam -> ProjectWebhookTopics -> WebApp ProjectWebhook
createProjectWebhook projectId uri webhookTopics = do
  let (topics, topicGroups) = case webhookTopics of
        AllTopicsInProject -> (mempty, Set.singleton AllProjectTopics)
        SelectedTopics ts -> (NESet.toSet ts, mempty)
  let filter = Nothing
  subscriptionId <- PG.runTransaction $ do
    Project {ownerUserId = projectOwner} <- Q.expectProjectById projectId
    subscriptionId <- NotifQ.createNotificationSubscription (ProjectSubscriptionOwner projectId) projectOwner (Just projectId) topics topicGroups filter
    _webhookId <- addWebhookDeliveryMethod uri "Project Webhook" subscriptionId
    pure subscriptionId
  expectProjectWebhook projectId subscriptionId

expectProjectWebhook :: ProjectId -> NotificationSubscriptionId -> WebApp ProjectWebhook
expectProjectWebhook projectId subscriptionId = do
  (uri, subscription) <- PG.runTransactionOrRespondError do
    -- Only resolves a webhook on a subscription owned by this project.
    (webhookId, _name) <- NotifQ.expectProjectWebhook projectId subscriptionId
    uri <-
      WebhookSecrets.fetchWebhookConfig webhookId >>= \case
        Left err -> throwError err
        Right (WebhookConfig {uri}) -> pure uri
    subscription <- NotifQ.expectNotificationSubscription (ProjectSubscriptionOwner projectId) subscriptionId
    pure (uri, subscription)
  let subscriptionTopics = case (Set.toList $ subscription.subscriptionTopicGroups, NESet.nonEmptySet subscription.subscriptionTopics) of
        ([], Just topics) -> SelectedTopics topics
        _ -> AllTopicsInProject
  pure $
    ProjectWebhook
      { projectWebhookUri = uri,
        projectWebhookTopics = subscriptionTopics,
        projectWebhookNotificationSubscriptionId = subscription.subscriptionId,
        projectWebhookCreatedAt = subscription.subscriptionCreatedAt,
        projectWebhookUpdatedAt = subscription.subscriptionUpdatedAt
      }

deleteProjectWebhook :: ProjectId -> NotificationSubscriptionId -> WebApp ()
deleteProjectWebhook projectId subscriptionId = do
  let owner = ProjectSubscriptionOwner projectId
  -- Deleting the subscription cascades to its webhooks, URIs and all.
  -- The delete is a no-op unless the subscription is owned by this project.
  PG.runTransaction $ do NotifQ.deleteNotificationSubscription owner subscriptionId

updateProjectWebhook :: SubscriptionOwner -> NotificationSubscriptionId -> Maybe URIParam -> (Maybe ProjectWebhookTopics) -> WebApp ()
updateProjectWebhook subscriptionOwner subscriptionId mayURIUpdate webhookTopics = do
  let (topics, topicGroups) = case webhookTopics of
        Nothing -> (Nothing, Nothing)
        Just AllTopicsInProject -> (Just mempty, Just $ Set.singleton AllProjectTopics)
        Just (SelectedTopics ts) -> (Just $ NESet.toSet ts, Just $ mempty)
  PG.runTransaction do
    for_ mayURIUpdate \uri -> do
      -- Scoped to the owner, so we never update a webhook hanging off of somebody else's
      -- subscription, even if the caller hands us a subscription id they don't own.
      webhooks <- NotifQ.webhooksForSubscription subscriptionOwner subscriptionId
      for_ webhooks \webhookId -> do
        WebhookSecrets.putWebhookConfig webhookId (WebhookConfig uri)
    NotifQ.updateNotificationSubscription subscriptionOwner subscriptionId topics topicGroups Nothing

-- | Update the URI of a webhook, if it's owned by the given user.
updateWebhookDeliveryMethod :: UserId -> NotificationWebhookId -> URIParam -> WebApp ()
updateWebhookDeliveryMethod notificationUser webhookDeliveryMethodId url = do
  PG.runTransaction do
    isValid <-
      PG.queryExpect1Col
        [PG.sql|
          SELECT EXISTS(
            SELECT FROM notification_webhooks nw
              WHERE nw.id = #{webhookDeliveryMethodId}
                AND nw.subscriber_user_id = #{notificationUser}
          )
        |]
    when isValid $ do
      WebhookSecrets.putWebhookConfig webhookDeliveryMethodId (WebhookConfig url)
