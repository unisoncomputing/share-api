{-# LANGUAGE KindSignatures #-}

module Share.Utils.Servant.Websockets
  ( withQueues,
    Queues (..),
  )
where

import Conduit
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TBMQueue
import Control.Lens (Profunctor (..))
import Control.Monad
import Data.Text (Text)
import Network.WebSockets
import UnliftIO

-- | Allows interfacing with a websocket as a pair of bounded queues.
data Queues i o = Queues
  { -- Receive from the client
    receive :: STM o,
    -- Send to the client
    send :: i -> STM (),
    shutdown :: IO ()
  }

instance Profunctor Queues where
  dimap f g (Queues {receive, send, shutdown}) =
    Queues
      { receive = g <$> receive,
        send = send . f,
        shutdown
      }

withQueues :: forall i o m a. (MonadUnliftIO m, WebSocketsData i, WebSocketsData o) => Int -> Int -> Connection -> (Queues i o -> m a) -> m a
withQueues inputBuffer outputBuffer conn action = do
  receiveQ <- liftIO $ newTBMQueueIO inputBuffer
  sendQ <- liftIO $ newTBMQueueIO outputBuffer
  let receive = do
        readTBMQueue receiveQ >>= \case
          Nothing -> retry
          Just msg -> pure msg
  let send msg = writeTBMQueue sendQ msg
  isClosedVar <- newTVarIO False

  let triggerClose :: IO ()
      triggerClose = do
        alreadyClosed <- atomically $ do
          isClosed <- readTVar isClosedVar
          when (not isClosed) $ do
            writeTVar isClosedVar True
            closeTBMQueue sendQ
            closeTBMQueue receiveQ
            pure ()
          pure isClosed
        when (not alreadyClosed) $ do
          sendClose conn ("Server is shutting down" :: Text)

  let queues = Queues {receive, send, shutdown = triggerClose}
  a <- withAsync (recvWorker receiveQ) $ \_ ->
    withAsync (sendWorker sendQ) $ \_ -> action queues
  liftIO $ triggerClose
  pure a
  where
    recvWorker :: TBMQueue o -> m ()
    recvWorker q = UnliftIO.handle handler $ do
      msg <- liftIO $ receiveData conn
      atomically $ writeTBMQueue q msg
      recvWorker q

    handler :: ConnectionException -> m ()
    handler = \case
      CloseRequest _ _ -> liftIO $ sendClose conn ("Client closed connection" :: Text)
      ConnectionClosed -> pure ()
      err -> throwIO err

    sendWorker :: TBMQueue i -> m ()
    sendWorker q = UnliftIO.handle handler $ do
      outMsg <- atomically $ readTBMQueue q
      case outMsg of
        Nothing -> pure () -- Queue closed, exit the worker
        Just outMsg' -> do
          -- TODO: send multiple at once
          liftIO $ sendBinaryDatas conn [outMsg']
          sendWorker q
