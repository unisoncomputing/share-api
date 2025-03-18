{-# LANGUAGE DataKinds #-}

-- | Note: this is for detecting notifications from the postgres instance, not anything
-- related to Share notifications.
module Share.Postgres.Notifications
  ( initialize,
    waitOnChannel,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Data.Set qualified as Set
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

notifs :: TVar (Set ChannelKind)
notifs = unsafePerformIO $ STM.newTVarIO mempty
{-# NOINLINE notifs #-}

-- | Initializes the notification worker, which listens for notifications from the database.
-- Must be called once at the beginning of the program.
initialize ::
  Ki.Scope -> -- The scope of the server
  Background ()
initialize scope = void $ Ki.fork scope $ forever do
  result <- UnliftIO.try $ do
    PG.runSession $ do
      for_ [minBound .. maxBound] \kind -> do
        PG.statement () $ Hasql.listen (toChannelId kind)
      -- Wait for notifications
      let loop = do
            Hasql.Notification {channel} <- lift $ Hasql.await
            fromChannelId channel & \case
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
  | ContributionDiffChannel
  deriving stock (Eq, Ord, Show, Bounded, Enum)

toChannelId :: ChannelKind -> Hasql.Identifier
toChannelId = \case
  DefinitionSyncChannel -> Hasql.Identifier "definition_sync"
  ContributionDiffChannel -> Hasql.Identifier "contribution_diff"

fromChannelId :: Text -> Maybe ChannelKind
fromChannelId = \case
  "definition_sync" -> Just DefinitionSyncChannel
  "contribution_diff" -> Just ContributionDiffChannel
  _ -> Nothing

-- | Wait on a channel for notifications, OR until the max polling time has been reached
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
