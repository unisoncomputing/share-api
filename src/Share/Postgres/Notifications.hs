{-# LANGUAGE DataKinds #-}

-- | Note: this is for detecting notifications from the postgres instance, not anything
-- related to Share notifications.
module Share.Postgres.Notifications
  ( initialize,
    waitOnChannel,
    notifyChannel,
    ChannelKind (..),
  )
where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Hasql.ListenNotify qualified as Hasql
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Errors qualified as Background
import Share.BackgroundJobs.Monad (Background)
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO qualified

data NotificationError
  = UnknownChannel Text
  | NotificationWorkerException UnliftIO.SomeException
  deriving stock (Show)
  deriving (Logging.Loggable) via (Logging.ShowLoggable Logging.Error NotificationError)

-- Initialize the set of channel kinds to check with every channel kind, so that on server startup, workers do any
-- outstanding work.
notifs :: TVar (Set ChannelKind)
notifs = unsafePerformIO $ STM.newTVarIO allChannels
{-# NOINLINE notifs #-}

-- | Initializes the notification worker, which listens for notifications from the database.
-- Must be called once at the beginning of the program.
initialize ::
  Ki.Scope -> -- The scope of the server
  Background ()
initialize scope = Ki.fork_ scope $ forever do
  result <- UnliftIO.try $ do
    PG.runSession $ do
      for_ allChannels \kind -> do
        PG.statement () $ Hasql.listen (Hasql.Identifier . Text.encodeUtf8 $ toChannelText kind)
      -- Wait for notifications
      let loop = do
            Hasql.Notification {channel} <- lift $ Hasql.await
            fromChannelText channel & \case
              Just kind -> do
                liftIO . STM.atomically $ STM.modifyTVar' notifs $ Set.insert kind
                loop
              Nothing -> pure $ UnknownChannel channel
      loop

  case result of
    Left (e :: UnliftIO.SomeException) -> do
      Background.reportError $ NotificationWorkerException e
    Right err ->
      Background.reportError err

data ChannelKind
  = DefinitionSyncChannel
  | CausalDiffChannel
  | WebhooksChannel
  deriving stock (Eq, Ord, Show, Bounded, Enum)

toChannelText :: ChannelKind -> Text
toChannelText = \case
  DefinitionSyncChannel -> "definition_sync"
  CausalDiffChannel -> "contribution_diff"
  WebhooksChannel -> "webhooks"

fromChannelText :: Text -> Maybe ChannelKind
fromChannelText = \case
  "definition_sync" -> Just DefinitionSyncChannel
  "contribution_diff" -> Just CausalDiffChannel
  "webhooks" -> Just WebhooksChannel
  _ -> Nothing

allChannels :: Set ChannelKind
allChannels =
  Set.fromList [minBound .. maxBound]

-- | Block waiting on a channel until either we get a notification OR until the max polling time has been reached
--
-- The channel notifications can help ensure we process items as they come in, but they're not
-- robust in spite of restarts or within a cluster setting, so we also have a max polling time
-- to ensure any missed notifications are eventually processed.
--
-- For this reason, when the wait resolves, you can only assume that something _might_ have
-- happened.
waitOnChannel :: (MonadIO m) => ChannelKind -> Int -> m ()
waitOnChannel kind maxPollInterval =
  liftIO . void $ UnliftIO.timeout maxPollInterval $ STM.atomically do
    channels <- STM.readTVar notifs
    guard (Set.member kind channels)
    STM.modifyTVar' notifs $ Set.delete kind

-- | Send a notification to a channel, workers will only be notified if the transaction
-- commits.
notifyChannel :: (PG.QueryA m) => ChannelKind -> m ()
notifyChannel kind = do
  let payload = ""
  PG.statement (Hasql.Notify (toChannelText kind) payload) $ Hasql.notify
