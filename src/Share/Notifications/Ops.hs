module Share.Notifications.Ops
  ( listNotificationDeliveryMethods,
    createWebhookDeliveryMethod,
    updateWebhookDeliveryMethod,
    deleteWebhookDeliveryMethod,
    hydrateEvent,
  )
where

import Share.IDs
import Share.Notifications.Queries qualified as NotifQ
import Share.Notifications.Types
import Share.Notifications.Webhooks.Secrets (WebhookConfig (..))
import Share.Notifications.Webhooks.Secrets qualified as WebhookSecrets
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.URI (URIParam (..))
import Share.Web.App (WebApp)
import Share.Web.Errors (respondError)
import Share.Web.UI.Links qualified as Links

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

createWebhookDeliveryMethod :: UserId -> URIParam -> Text -> WebApp NotificationWebhookId
createWebhookDeliveryMethod userId uriParam webhookName = do
  -- Note that we can't be completely transactional between postgres and vault here.
  webhookId <- PG.runTransaction do
    NotifQ.createWebhookDeliveryMethod userId webhookName
  let webhookConfig = WebhookConfig {uri = uriParam}
  WebhookSecrets.putWebhookConfig webhookId webhookConfig
  pure webhookId

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
