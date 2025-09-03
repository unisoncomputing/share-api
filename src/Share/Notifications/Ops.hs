module Share.Notifications.Ops
  ( listNotificationDeliveryMethods,
    addWebhookDeliveryMethod,
    updateWebhookDeliveryMethod,
    deleteWebhookDeliveryMethod,
    listProjectWebhooks,
    createProjectWebhook,
    deleteProjectWebhook,
    updateProjectWebhook,
    hydrateEvent,
  )
where

import Control.Lens
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
import Share.Web.Share.Projects.Types (ProjectWebhook (..))
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

deleteWebhookDeliveryMethod :: UserId -> NotificationWebhookId -> WebApp ()
deleteWebhookDeliveryMethod notificationUser webhookDeliveryMethodId = do
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
    -- Delete the webhook config in Vault
    WebhookSecrets.deleteWebhookConfig webhookDeliveryMethodId >>= \case
      Left err -> respondError err
      Right _ -> do
        PG.runTransaction $ do
          NotifQ.deleteWebhookDeliveryMethod notificationUser webhookDeliveryMethodId

hydrateEvent :: HydratedEventPayload -> PG.Transaction e HydratedEvent
hydrateEvent hydratedEventPayload = do
  hydratedEventLink <- Links.notificationLink hydratedEventPayload
  pure $ HydratedEvent {hydratedEventPayload, hydratedEventLink}

-- | We provide a wrapper layer on top of notification subscriptions and webhooks
-- to make the frontend experience a bit more intuitive.
listProjectWebhooks :: UserId -> ProjectId -> WebApp [ProjectWebhook]
listProjectWebhooks caller projectId = do
  projectWebhooks <- PG.runTransaction $ do NotifQ.listProjectWebhooks caller projectId
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
        results <&> \(NotificationWebhookConfig {webhookDeliveryUrl = url}, _name, NotificationSubscription {subscriptionTopics, subscriptionId, subscriptionCreatedAt, subscriptionUpdatedAt}) ->
          ProjectWebhook
            { projectWebhookUri = URIParam url,
              projectWebhookEvents = subscriptionTopics,
              projectWebhookNotificationSubscriptionId = subscriptionId,
              projectWebhookCreatedAt = subscriptionCreatedAt,
              projectWebhookUpdatedAt = subscriptionUpdatedAt
            }
  pure webhooks

createProjectWebhook :: ProjectId -> URIParam -> Set NotificationTopic -> WebApp ProjectWebhook
createProjectWebhook projectId uri topics = do
  let topicGroups = mempty
  let filter = Nothing
  runInIO <- UnliftIO.askRunInIO
  subscriptionId <- PG.runTransactionOrRespondError $ do
    Project {ownerUserId = projectOwner} <- Q.expectProjectById projectId
    subscriptionId <- NotifQ.createNotificationSubscription subscriberUserId projectOwner (Just projectId) topics topicGroups filter
    addWebhookDeliveryMethod uri "Project Webhook" subscriptionId runInIO
    pure subscriptionId
  expectProjectWebhook subscriberUserId projectId subscriptionId

expectProjectWebhook :: UserId -> ProjectId -> NotificationSubscriptionId -> WebApp ProjectWebhook
expectProjectWebhook caller projectId subscriptionId = do
  (webhookId, _name) <- PG.runTransaction $ do NotifQ.expectProjectWebhook projectId subscriptionId
  uri <-
    WebhookSecrets.fetchWebhookConfig webhookId >>= \case
      Left err -> respondError err
      Right (WebhookConfig {uri}) -> pure uri
  subscription <- PG.runTransaction $ do NotifQ.expectNotificationSubscription (ProjectSubscriptionOwner projectId) subscriptionId
  pure $
    ProjectWebhook
      { projectWebhookUri = uri,
        projectWebhookEvents = subscription.subscriptionTopics,
        projectWebhookNotificationSubscriptionId = subscription.subscriptionId,
        projectWebhookCreatedAt = subscription.subscriptionCreatedAt,
        projectWebhookUpdatedAt = subscription.subscriptionUpdatedAt
      }

deleteProjectWebhook :: UserId -> NotificationSubscriptionId -> WebApp ()
deleteProjectWebhook caller subscriptionId = do
  -- First fetch the webhook id associated with this subscription
  webhooks <- PG.runTransaction $ do NotifQ.webhooksForSubscription subscriptionId
  for_ webhooks \webhookId -> do
    deleteWebhookDeliveryMethod caller webhookId
  PG.runTransaction $ do NotifQ.deleteNotificationSubscription caller subscriptionId

updateProjectWebhook :: SubscriptionOwner -> NotificationSubscriptionId -> URIParam -> (Maybe (Set NotificationTopic)) -> WebApp ()
updateProjectWebhook subscriptionOwner subscriptionId uri topics = do
  -- First fetch the webhook ids associated with this subscription
  webhooks <- PG.runTransaction $ do NotifQ.webhooksForSubscription subscriptionId
  for_ webhooks \webhookId -> do
    -- Update the webhook config in Vault
    WebhookSecrets.putWebhookConfig webhookId (WebhookConfig uri) >>= \case
      Left err -> respondError err
      Right _ -> pure ()
  PG.runTransaction $ NotifQ.updateNotificationSubscription subscriptionOwner subscriptionId topics mempty Nothing

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
