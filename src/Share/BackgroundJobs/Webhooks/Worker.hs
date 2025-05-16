{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides the background worker for sending notification webhooks.
module Share.BackgroundJobs.Webhooks.Worker (worker) where

import Control.Lens hiding ((.=))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types ((.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List.Extra qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Ki.Unlifted qualified as Ki
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI)
import Network.URI qualified as URI
import Share.App (AppM)
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Webhooks.Queries qualified as WQ
import Share.BackgroundJobs.Workers (newWorker)
import Share.Env qualified as Env
import Share.IDs
import Share.IDs qualified as IDs
import Share.JWT (JWTParam (..))
import Share.JWT qualified as JWT
import Share.Metrics qualified as Metrics
import Share.Notifications.Queries qualified as NQ
import Share.Notifications.Types
import Share.Notifications.Webhooks.Secrets (WebhookConfig (..), WebhookSecretError)
import Share.Notifications.Webhooks.Secrets qualified as Webhooks
import Share.Postgres qualified as PG
import Share.Postgres.Notifications qualified as Notif
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.URI (URIParam (..), uriToText)
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Share.DisplayInfo.Queries qualified as DisplayInfoQ
import Share.Web.Share.DisplayInfo.Types (UnifiedDisplayInfo)
import Share.Web.Share.DisplayInfo.Types qualified as DisplayInfo
import Share.Web.UI.Links qualified as Links
import UnliftIO qualified

data WebhookSendFailure
  = ReceiverError NotificationEventId NotificationWebhookId HTTP.Status BL.ByteString
  | InvalidRequest NotificationEventId NotificationWebhookId UnliftIO.SomeException
  | WebhookSecretFetchError NotificationEventId NotificationWebhookId WebhookSecretError
  | JWTError NotificationEventId NotificationWebhookId JWTError
  deriving stock (Show)

instance Logging.Loggable WebhookSendFailure where
  toLog = \case
    ReceiverError eventId webhookId status body ->
      Logging.textLog
        ( "Webhook receiver error: "
            <> Text.pack (show status)
            <> " "
            <> Text.decodeUtf8 (BL.toStrict body)
        )
        & Logging.withTag ("status", tShow status)
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.UserFault
    InvalidRequest eventId webhookId err ->
      Logging.textLog ("Invalid request: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.UserFault
    WebhookSecretFetchError eventId webhookId err ->
      Logging.textLog ("Failed to fetch webhook secret: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.Error
    JWTError eventId webhookId err ->
      Logging.textLog ("JWT error: " <> Text.pack (show err))
        & Logging.withTag ("event_id", tShow eventId)
        & Logging.withTag ("webhook_id", tShow webhookId)
        & Logging.withSeverity Logging.Error

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60 -- 10 minutes

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "notifications:webhooks" $ forever do
    Notif.waitOnChannel Notif.WebhooksChannel (maxPollingIntervalSeconds * 1000000)
    processWebhooks authZReceipt
  where
    processWebhooks :: AuthZ.AuthZReceipt -> Background ()
    processWebhooks authZReceipt = do
      toIO <- UnliftIO.askRunInIO
      -- Need to unlift so we can use this in transactions
      let tryWebhookIO eventData webhookId = toIO $ tryWebhook eventData webhookId
      mayResult <- Metrics.recordWebhookSendingDuration $ PG.runTransaction $ runMaybeT $ do
        webhookInfo@(eventId, webhookId) <- MaybeT WQ.getUnsentWebhook
        mayErr <- lift $ attemptWebhookSend authZReceipt tryWebhookIO eventId webhookId
        pure (mayErr, webhookInfo)
      case mayResult of
        Just (Just err, _) -> do
          case err of
            WebhookSecretFetchError {} -> reportError err
            _ -> Logging.logErrorText $ "Webhook send errors: " <> tShow err
          -- If we got an error, we can retry it.
          -- TODO: Add some sort of backoff?
          processWebhooks authZReceipt
        (Just (Nothing, (eventId, webhookId))) -> do
          Logging.textLog ("Webhook sent successfully: " <> Text.pack (show webhookId))
            & Logging.withTag ("webhook_id", tShow webhookId)
            & Logging.withTag ("event_id", tShow eventId)
            & Logging.withSeverity Logging.Info
            & Logging.logMsg
          -- Keep processing releases until we run out of them.
          processWebhooks authZReceipt
        Nothing -> do
          -- No webhooks ready to try at the moment, we can wait till more are available.
          pure ()

--
webhookTimeout :: HTTPClient.ResponseTimeout
webhookTimeout = HTTPClient.responseTimeoutMicro (20 * 1000000 {- 20 seconds -})

data WebhookEventPayload jwt = WebhookEventPayload
  { -- | The event ID of the notification event.
    eventId :: NotificationEventId,
    -- | The time at which the event occurred.
    occurredAt :: UTCTime,
    -- | The topic of the notification event.
    topic :: NotificationTopic,
    -- | The data associated with the notification event.
    data_ :: HydratedEventPayload,
    -- | A signed token containing all of the same data.
    jwt :: jwt
  }
  deriving stock (Show, Eq)

deriving via JWT.JSONJWTClaims (WebhookEventPayload ()) instance JWT.AsJWTClaims (WebhookEventPayload ())

instance ToJSON (WebhookEventPayload JWTParam) where
  toJSON WebhookEventPayload {eventId, occurredAt, topic, data_, jwt} =
    Aeson.object
      [ "eventId" Aeson..= eventId,
        "occurredAt" Aeson..= occurredAt,
        "topic" Aeson..= topic,
        "data" Aeson..= data_,
        "signed" Aeson..= jwt
      ]

instance ToJSON (WebhookEventPayload ()) where
  toJSON WebhookEventPayload {eventId, occurredAt, topic, data_} =
    Aeson.object
      [ "eventId" Aeson..= eventId,
        "occurredAt" Aeson..= occurredAt,
        "topic" Aeson..= topic,
        "data" Aeson..= data_
      ]

instance FromJSON (WebhookEventPayload ()) where
  parseJSON = Aeson.withObject "WebhookEventPayload" $ \o ->
    WebhookEventPayload
      <$> o Aeson..: "eventId"
      <*> o Aeson..: "occurredAt"
      <*> o Aeson..: "topic"
      <*> o Aeson..: "data"
      <*> pure ()

tryWebhook ::
  NotificationEvent NotificationEventId UnifiedDisplayInfo UTCTime HydratedEventPayload ->
  NotificationWebhookId ->
  Background (Maybe WebhookSendFailure)
tryWebhook event webhookId = UnliftIO.handleAny (\someException -> pure $ Just $ InvalidRequest event.eventId webhookId someException) do
  fmap (either Just (const Nothing)) $ runExceptT do
    proxiedHTTPManager <- asks Env.proxiedHttpClient
    WebhookConfig {uri = URIParam uri} <-
      lift (Webhooks.fetchWebhookConfig webhookId) >>= \case
        Left err -> throwError $ WebhookSecretFetchError event.eventId webhookId err
        Right config -> pure config
    jwtSettings <- asks Env.jwtSettings
    let payload =
          WebhookEventPayload
            { eventId = event.eventId,
              occurredAt = event.eventOccurredAt,
              topic = hydratedEventTopic event.eventData,
              data_ = event.eventData,
              jwt = ()
            }
    payloadJWT <-
      JWT.signJWT jwtSettings payload >>= \case
        Left jwtErr -> throwError $ JWTError event.eventId webhookId jwtErr
        Right jwt -> pure jwt
    let payloadWithJWT = payload {jwt = JWTParam payloadJWT}
    req <- ExceptT $ buildWebhookRequest webhookId uri event payloadWithJWT
    resp <- liftIO $ HTTPClient.httpLbs req proxiedHTTPManager
    case HTTPClient.responseStatus resp of
      httpStatus@(HTTP.Status status _)
        | status >= 400 -> throwError $ ReceiverError event.eventId webhookId httpStatus $ HTTPClient.responseBody resp
        | otherwise -> pure ()

buildWebhookRequest :: NotificationWebhookId -> URI -> NotificationEvent NotificationEventId UnifiedDisplayInfo UTCTime HydratedEventPayload -> WebhookEventPayload JWTParam -> AppM reqCtx (Either WebhookSendFailure HTTPClient.Request)
buildWebhookRequest webhookId uri event defaultPayload = do
  if
    | isSlackWebhook uri -> buildSlackWebhookRequest uri
    | isDiscordWebhook uri ->
        let nonSlackPath =
              List.stripSuffix "/slack" (URI.uriPath uri)
                & fromMaybe ((URI.uriPath uri))
            slackifiedDiscordWebhookURI = uri {URI.uriPath = nonSlackPath <> "/slack"}
         in buildSlackWebhookRequest slackifiedDiscordWebhookURI
    | otherwise -> pure $ buildDefaultPayload
  where
    isSlackWebhook :: URI -> Bool
    isSlackWebhook uri =
      case URI.uriRegName <$> URI.uriAuthority uri of
        Nothing -> False
        Just regName -> List.isPrefixOf "hooks.slack.com" regName

    isDiscordWebhook :: URI -> Bool
    isDiscordWebhook uri =
      case (URI.uriRegName <$> URI.uriAuthority uri, URI.uriPath uri) of
        (Just regName, path) | List.isPrefixOf path "api/webhooks" -> Text.isPrefixOf "discord.com" (Text.pack regName)
        _ -> False

    buildDefaultPayload :: Either WebhookSendFailure HTTPClient.Request
    buildDefaultPayload =
      HTTPClient.requestFromURI uri
        & mapLeft (\e -> InvalidRequest event.eventId webhookId e)
        <&> \req ->
          req
            { HTTPClient.method = "POST",
              HTTPClient.responseTimeout = webhookTimeout,
              HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
              HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode defaultPayload
            }

    mkSlackAttachment :: Text -> URI -> Text -> URI -> Maybe URI -> Text -> Text -> UTCTime -> Aeson.Value
    mkSlackAttachment preText mainURI authorName authorLink authorAvatarUrl title msg timestamp =
      let epochSeconds :: Int64
          epochSeconds = round (POSIX.utcTimeToPOSIXSeconds timestamp)
          t :: Text -> Text
          t x = x
       in Aeson.object $
            [ "mrkdwn_in" Aeson..= [t "text"],
              "color" .= t "#36a64f",
              "pretext" .= preText,
              "author_name" .= authorName,
              "author_link" .= uriToText authorLink,
              "title" .= title,
              "title_link" .= uriToText mainURI,
              "text" .= msg,
              "thumb_url" .= uriToText Links.unisonLogoImage,
              -- "footer" .= "footer",
              -- "footer_icon" .= "https://platform.slack-edge.com/img/default_application_icon.png",
              "ts" .= epochSeconds
            ]
              <> (authorAvatarUrl & foldMap \uri -> ["author_icon" .= uriToText uri])
    buildSlackWebhookRequest :: URI -> AppM reqCtx (Either WebhookSendFailure HTTPClient.Request)
    buildSlackWebhookRequest uri = do
      let actorName = event.eventActor ^. DisplayInfo.name_
          actorHandle = "(" <> IDs.toText (PrefixedID @"@" $ event.eventActor ^. DisplayInfo.handle_) <> ")"
          actorAuthor = maybe "" (<> " ") actorName <> actorHandle
          actorAvatarUrl = event.eventActor ^. DisplayInfo.avatarUrl_
      actorLink <- Links.userProfilePage (event.eventActor ^. DisplayInfo.handle_)
      slackPayload <- case event.eventData of
        HydratedProjectBranchUpdatedPayload payload -> do
          let pbShorthand = (projectBranchShortHandFromParts payload.projectInfo.projectShortHand payload.branchInfo.branchShortHand)
              title = "Branch " <> IDs.toText pbShorthand <> " was just updated."
              msg = ""
              preText = title
          link <- Links.notificationLink event.eventData
          pure $
            Aeson.object
              [ "text" Aeson..= title,
                "attachments"
                  Aeson..= [ mkSlackAttachment preText link actorAuthor actorLink actorAvatarUrl title msg event.eventOccurredAt
                           ]
              ]
        HydratedProjectContributionCreatedPayload payload -> do
          let pbShorthand = (projectBranchShortHandFromParts payload.projectInfo.projectShortHand payload.contributionInfo.contributionSourceBranch.branchShortHand)
              title = payload.contributionInfo.contributionTitle
              msg = fromMaybe "" payload.contributionInfo.contributionDescription
              preText = "New Contribution in " <> IDs.toText pbShorthand
          link <- Links.notificationLink event.eventData
          pure $
            Aeson.object
              [ "text" Aeson..= preText,
                "attachments"
                  Aeson..= [ mkSlackAttachment preText link actorAuthor actorLink actorAvatarUrl title msg event.eventOccurredAt
                           ]
              ]
      pure $
        HTTPClient.requestFromURI uri
          & mapLeft (\e -> InvalidRequest event.eventId webhookId e)
          <&> ( \req ->
                  req
                    { HTTPClient.method = "POST",
                      HTTPClient.responseTimeout = webhookTimeout,
                      HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
                      HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode slackPayload
                    }
              )

attemptWebhookSend ::
  AuthZ.AuthZReceipt ->
  (NotificationEvent NotificationEventId UnifiedDisplayInfo UTCTime HydratedEventPayload -> NotificationWebhookId -> IO (Maybe WebhookSendFailure)) ->
  NotificationEventId ->
  NotificationWebhookId ->
  PG.Transaction e (Maybe WebhookSendFailure)
attemptWebhookSend _authZReceipt tryWebhookIO eventId webhookId = do
  event <- NQ.expectEvent eventId
  hydratedEvent <- forOf eventData_ event NQ.hydrateEventData
  populatedEvent <- hydratedEvent & DisplayInfoQ.unifiedDisplayInfoForUserOf eventUserInfo_
  PG.transactionUnsafeIO (tryWebhookIO populatedEvent webhookId) >>= \case
    Just err -> do
      WQ.recordFailedDeliveryAttempt eventId webhookId
      pure $ Just err
    Nothing -> do
      WQ.markWebhookAsDelivered eventId webhookId
      pure Nothing
