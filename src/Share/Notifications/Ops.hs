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
import Share.App (AppM)
import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types
import Share.Notifications.Webhooks.Secrets (WebhookConfig (..))
import Share.Notifications.Webhooks.Secrets qualified as WebhookSecrets
import Share.Notifications.Webhooks.Secrets qualified as Webhooks
import Share.Postgres qualified as PG
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Project (Project (..))
import Share.Utils.URI (URIParam (..))
import Share.Web.App (WebApp)
import Share.Web.Errors (respondError)
import Share.Web.Share.Projects.Types (ProjectWebhook (..), ProjectWebhookTopics (..))
import Share.Web.UI.Links qualified as Links
import UnliftIO qualified

listNotificationDeliveryMethods :: UserId -> Maybe NotificationSubscriptionId -> WebApp [NotificationDeliveryMethod]
listNotificationDeliveryMethods userId maySubscriptionId = do
  (emailDeliveryMethods, webhookIds) <- PG.runTransaction do
    emailDeliveryMethods <- NotifQ.listEmailDeliveryMethods userId maySubscriptionId
    webhookIds <- NotifQ.listWebhooks userId maySubscriptionId
    pure (emailDeliveryMethods, webhookIds)
  webhookDeliveryMethods <- for webhookIds \webhookId -> do
    WebhookSecrets.fetchWebhookConfig webhookId >>= \case
      Left err -> respondError err
      Right (WebhookConfig {uri = URIParam uri}) -> do
        pure $ (NotificationWebhookConfig webhookId uri)

  pure $ (EmailDeliveryMethod <$> emailDeliveryMethods) <> (WebhookDeliveryMethod <$> webhookDeliveryMethods)

addWebhookDeliveryMethod :: URIParam -> Text -> NotificationSubscriptionId -> (AppM r (Either Webhooks.WebhookSecretError ()) -> IO (Either Webhooks.WebhookSecretError ())) -> PG.Transaction Webhooks.WebhookSecretError NotificationWebhookId
addWebhookDeliveryMethod uriParam webhookName notificationSubscriptionId runInIO = do
  let webhookConfig = WebhookConfig {uri = uriParam}
  -- Note that we can't be completely transactional between postgres and vault here.
  webhookId <- NotifQ.createWebhookDeliveryMethod webhookName notificationSubscriptionId
  -- We run this inside the transaction such that, if it fails, the transaction
  -- will be rolled back.
  --
  -- In the case where it succeeds, but the transaction fails to commit (which is unlikely)
  -- we may have a dangling secret in Vault, which isn't ideal, but is not so bad.
  PG.transactionUnsafeIO (runInIO $ WebhookSecrets.putWebhookConfig webhookId webhookConfig) >>= \case
    Left err -> do
      throwError err
    Right _ -> pure ()
  pure webhookId

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
    -- Delete the webhook config in Vault
    WebhookSecrets.deleteWebhookConfig webhookDeliveryMethodId >>= \case
      Left err -> respondError err
      Right _ -> do
        PG.runTransaction $ do
          NotifQ.deleteWebhookDeliveryMethod owner webhookDeliveryMethodId

hydrateEvent :: HydratedEventPayload -> PG.Transaction e HydratedEvent
hydrateEvent hydratedEventPayload = do
  hydratedEventLink <- Links.notificationLink hydratedEventPayload
  pure $ HydratedEvent {hydratedEventPayload, hydratedEventLink}

-- | We provide a wrapper layer on top of notification subscriptions and webhooks
-- to make the frontend experience a bit more intuitive.
listProjectWebhooks :: ProjectId -> WebApp [ProjectWebhook]
listProjectWebhooks projectId = do
  projectWebhooks <- PG.runTransaction $ do NotifQ.listProjectWebhooks projectId
  results <-
    projectWebhooks
      & asListOf (traversed . _1) %%~ \webhookIds ->
        do
          UnliftIO.pooledForConcurrently webhookIds \webhookId -> do
            Webhooks.fetchWebhookConfig webhookId >>= \case
              Left err -> respondError err
              Right (WebhookConfig {uri = URIParam uri}) -> do
                pure $ (NotificationWebhookConfig webhookId uri)
  let webhooks =
        results <&> \(NotificationWebhookConfig {webhookDeliveryUrl = url}, _name, NotificationSubscription {subscriptionTopics, subscriptionTopicGroups, subscriptionId, subscriptionCreatedAt, subscriptionUpdatedAt}) ->
          let webhookTopics = case (Set.toList subscriptionTopicGroups, NESet.nonEmptySet subscriptionTopics) of
                ([], Just topics) -> SelectedTopics topics
                _ -> AllProjectTopics
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
        AllProjectTopics -> (mempty, Set.singleton WatchProject)
        SelectedTopics ts -> (NESet.toSet ts, mempty)
  let filter = Nothing
  runInIO <- UnliftIO.askRunInIO
  subscriptionId <- PG.runTransactionOrRespondError $ do
    Project {ownerUserId = projectOwner} <- Q.expectProjectById projectId
    subscriptionId <- NotifQ.createNotificationSubscription (ProjectSubscriptionOwner projectId) projectOwner (Just projectId) topics topicGroups filter
    addWebhookDeliveryMethod uri "Project Webhook" subscriptionId runInIO
    pure subscriptionId
  expectProjectWebhook projectId subscriptionId

expectProjectWebhook :: ProjectId -> NotificationSubscriptionId -> WebApp ProjectWebhook
expectProjectWebhook projectId subscriptionId = do
  (webhookId, _name) <- PG.runTransaction $ do NotifQ.expectProjectWebhook projectId subscriptionId
  uri <-
    WebhookSecrets.fetchWebhookConfig webhookId >>= \case
      Left err -> respondError err
      Right (WebhookConfig {uri}) -> pure uri
  subscription <- PG.runTransaction $ do NotifQ.expectNotificationSubscription (ProjectSubscriptionOwner projectId) subscriptionId
  let subscriptionTopics = case (Set.toList $ subscription.subscriptionTopicGroups, NESet.nonEmptySet subscription.subscriptionTopics) of
        ([], Just topics) -> SelectedTopics topics
        _ -> AllProjectTopics
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
  -- First fetch the webhook id associated with this subscription
  webhooks <- PG.runTransaction $ do NotifQ.webhooksForSubscription subscriptionId
  for_ webhooks \webhookId -> do
    deleteWebhookDeliveryMethod owner webhookId
  PG.runTransaction $ do NotifQ.deleteNotificationSubscription owner subscriptionId

updateProjectWebhook :: SubscriptionOwner -> NotificationSubscriptionId -> Maybe URIParam -> (Maybe ProjectWebhookTopics) -> WebApp ()
updateProjectWebhook subscriptionOwner subscriptionId mayURIUpdate webhookTopics = do
  for_ mayURIUpdate \uri -> do
    -- First fetch the webhook ids associated with this subscription
    webhooks <- PG.runTransaction $ do NotifQ.webhooksForSubscription subscriptionId
    for_ webhooks \webhookId -> do
      -- Update the webhook config in Vault
      WebhookSecrets.putWebhookConfig webhookId (WebhookConfig uri) >>= \case
        Left err -> respondError err
        Right _ -> pure ()
  let (topics, topicGroups) = case webhookTopics of
        Nothing -> (Nothing, Nothing)
        Just AllProjectTopics -> (Just mempty, Just $ Set.singleton WatchProject)
        Just (SelectedTopics ts) -> (Just $ NESet.toSet ts, Just $ mempty)
  PG.runTransaction $ NotifQ.updateNotificationSubscription subscriptionOwner subscriptionId topics topicGroups Nothing

updateWebhookDeliveryMethod :: UserId -> NotificationWebhookId -> URIParam -> WebApp ()
updateWebhookDeliveryMethod notificationUser webhookDeliveryMethodId url = do
  isValid <- PG.runTransaction $ do
    PG.queryExpect1Col
      [PG.sql|
        SELECT EXISTS(
          SELECT FROM notification_webhooks nw
            WHERE nw.id = #{webhookDeliveryMethodId}
              AND nw.subscriber_user_id = #{notificationUser}
        )
      |]
  when isValid $ do
    -- Update the webhook config in Vault
    WebhookSecrets.putWebhookConfig webhookDeliveryMethodId (WebhookConfig url) >>= \case
      Left err -> respondError err
      Right _ -> pure ()
