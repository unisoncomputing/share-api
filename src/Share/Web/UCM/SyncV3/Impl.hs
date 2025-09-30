{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.Impl where

import Control.Lens hiding ((.=))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Vector (Vector)
import GHC.Natural
import Ki.Unlifted qualified as Ki
import Network.WebSockets qualified as WS
import Share.Codebase (CodebaseEnv)
import Share.IDs (UserId)
import Share.Prelude
import Share.Utils.Servant.Websockets (Queues (..), withQueues)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.UCM.SyncV3.API qualified as SyncV3
import Share.Web.UCM.SyncV3.Queries qualified as Q
import Share.Web.UCM.SyncV3.Types
import U.Codebase.Sqlite.Orphans ()
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWT)
import UnliftIO qualified
import UnliftIO.STM

-- Amount of entities to buffer from the network into the send/recv queues.
sendBufferSize :: Natural
sendBufferSize = 100

recvBufferSize :: Natural
recvBufferSize = 100

data StreamInitInfo = StreamInitInfo

streamSettings :: StreamInitInfo
streamSettings = StreamInitInfo

server :: Maybe UserId -> SyncV3.Routes WebAppServer
server mayUserId =
  SyncV3.Routes
    { downloadEntities = downloadEntitiesImpl mayUserId
    }

downloadEntitiesImpl :: Maybe UserId -> WS.Connection -> WebApp ()
downloadEntitiesImpl _mayCallerUserId conn = do
  -- Auth is currently done via HashJWTs
  _authZReceipt <- AuthZ.checkDownloadFromUserCodebase
  doSyncEmitter shareEmitter conn

-- | Given a helper which understands how to wire things into its backend, This
-- implements the sync emitter logic which is independent of the backend.
doSyncEmitter ::
  forall m.
  (MonadUnliftIO m) =>
  ( (SyncState HashTag Hash32) ->
    Queues (FromEmitterMessage Hash32 Text) (MsgOrError SyncError (FromReceiverMessage HashJWT Hash32)) ->
    m (Maybe SyncError)
  ) ->
  WS.Connection ->
  m ()
doSyncEmitter emitterImpl conn = do
  withQueues @(FromEmitterMessage Hash32 Text) @(MsgOrError SyncError (FromReceiverMessage HashJWT Hash32))
    recvBufferSize
    sendBufferSize
    conn
    \(q@Queues {receive}) -> handleErr q $ do
      let recvM :: ExceptT SyncError m (FromReceiverMessage HashJWT Hash32)
          recvM = do
            result <- liftIO $ atomically receive
            case result of
              Msg msg -> pure msg
              Err err -> throwError err
      initMsg <- recvM
      syncState <- case initMsg of
        InitStream initMsg -> lift $ initialize initMsg
        other -> throwError $ InitializationError ("Expected InitStream message, got: " <> tShow other)
      lift (emitterImpl syncState q) >>= \case
        Nothing -> pure ()
        Just err -> throwError err
  where
    handleErr (Queues {send, shutdown}) action = do
      runExceptT action >>= \case
        Left err -> do
          atomically $ do
            send (ErrorMsg err)
          liftIO $ shutdown
        Right r -> pure r

-- | Given a helper which understands how to wire things into its backend, This
-- implements the sync receiver logic which is independent of the backend.
doSyncReceiver ::
  Queues (FromEmitterMessage Hash32 Text) (MsgOrError SyncError (FromReceiverMessage HashJWT HashTag)) ->
  m ()
doSyncReceiver _receiverImpl = do
  _

initialize :: InitMsg ah -> m (SyncState sh hash)
initialize = undefined

data SyncState sh hash = SyncState
  { codebase :: CodebaseEnv,
    -- To avoid needing to sign HashJWTs for every hash we can just keep track of which hashes we've referenced and check
    -- against this set when receiving requests.
    validRequestsVar :: TVar (Set (EntityKind, hash)),
    -- Entities which have been requested by the client but not yet sent.
    requestedEntitiesVar :: TVar (Set (EntityKind, hash)),
    -- Hashes which have been sent to the client
    entitiesAlreadySentVar :: TVar (Set (EntityKind, hash))
    -- Hash mappings we've already sent to the client.
    -- mappedHashesVar :: TVar (Map sh hash)
  }

shareEmitter ::
  (SyncState HashTag Hash32) ->
  Queues (FromEmitterMessage Hash32 Text) (MsgOrError SyncError (FromReceiverMessage HashJWT Hash32)) ->
  WebApp (Maybe SyncError)
shareEmitter SyncState {requestedEntitiesVar, entitiesAlreadySentVar, validRequestsVar, codebase} (Queues {send, receive, shutdown}) = Ki.scoped $ \scope -> do
  errVar <- newEmptyTMVarIO
  let onErr :: SyncError -> STM ()
      onErr e = do
        UnliftIO.putTMVar errVar e
  Ki.fork scope $ sendWorker onErr
  Ki.fork scope $ receiveWorker onErr
  r <- atomically $ (Ki.awaitAll scope $> Nothing) <|> (Just <$> UnliftIO.takeTMVar errVar)
  liftIO $ shutdown
  pure r
  where
    sendWorker :: (SyncError -> STM ()) -> WebApp ()
    sendWorker onErr = forever $ do
      reqs <- atomically $ do
        reqs <- readTVar requestedEntitiesVar
        validRequests <- readTVar validRequestsVar
        let forbiddenRequests = Set.difference reqs validRequests
        validRequests <-
          if not (Set.null forbiddenRequests)
            then do
              onErr (ForbiddenEntityRequest forbiddenRequests)
              pure $ Set.difference validRequests forbiddenRequests
            else do
              pure validRequests
        guard (not $ Set.null validRequests)
        -- TODO: Add reasonable batch sizes
        modifyTVar' requestedEntitiesVar (const Set.empty)
        sent <- readTVar entitiesAlreadySentVar
        let unsent = Set.difference reqs sent
        guard (not $ Set.null unsent)
        pure unsent
      newEntities <- fetchEntities codebase reqs
      -- let hashMappings :: Map HashTag Hash32
      --     hashMappings =
      --       newEntities
      --         & toListOf (folded . entityHashesGetter_)
      --         & Map.fromList
      -- atomically $ do
      --   alreadyMapped <- readTVar mappedHashesVar
      --   let newMappings = Map.difference hashMappings alreadyMapped
      --   modifyTVar' mappedHashesVar (Map.union newMappings)
      --   send (HashMappingsMsg (HashMappings (newMappings)))

      atomically $ do
        let newHashes = setOf (folded . to snd) newEntities
        modifyTVar' entitiesAlreadySentVar (Set.union newHashes)
        for newEntities \entity -> do
          send (EntityMsg entity)

    receiveWorker :: (SyncError -> STM ()) -> WebApp ()
    receiveWorker onErr = forever $ do
      atomically $ do
        receive >>= \case
          Err err -> onErr err
          Msg (InitStream {}) -> onErr (InitializationError "Received duplicate InitStream message")
          Msg (EntityRequest (EntityRequestMsg {hashes})) -> do
            modifyTVar' requestedEntitiesVar (\s -> Set.union s (Set.fromList hashes))

fetchEntities :: CodebaseEnv -> Set (EntityKind, Hash32) -> WebApp (Vector (Entity Hash32 Text))
fetchEntities codebase reqs = do
  Q.fetchSerialisedEntities codebase reqs
