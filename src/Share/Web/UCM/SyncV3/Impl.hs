{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.Impl (server) where

import Control.Lens hiding ((.=))
import Control.Monad.Cont (ContT (..), MonadCont (..))
import Control.Monad.Except (runExceptT)
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Vector (Vector)
import GHC.Natural
import Ki.Unlifted qualified as Ki
import Network.WebSockets qualified as WS
import Share.Codebase (CodebaseEnv)
import Share.IDs (UserId)
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.App
import Share.Web.Authentication.Types qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.SyncCommon.Impl (codebaseForBranchRef)
import Share.Web.UCM.SyncCommon.Types
import Share.Web.UCM.SyncV3.API qualified as SyncV3
import Share.Web.UCM.SyncV3.Queries qualified as Q
import U.Codebase.Sqlite.Orphans ()
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWT, HashJWTClaims (..))
import Unison.Share.API.Hash qualified as HashJWT
import Unison.SyncV3.Types
import Unison.SyncV3.Utils (entityDependencies)
import Unison.Util.Websockets (Queues (..), withQueues)
import UnliftIO qualified
import UnliftIO.STM

-- Amount of entities to buffer from the network into the send/recv queues.
sendBufferSize :: Natural
sendBufferSize = 100

recvBufferSize :: Natural
recvBufferSize = 100

-- data StreamInitInfo = StreamInitInfo

-- streamSettings :: StreamInitInfo
-- streamSettings = StreamInitInfo

server :: Maybe UserId -> SyncV3.Routes WebAppServer
server mayUserId =
  SyncV3.Routes
    { downloadEntities = downloadEntitiesImpl mayUserId
    }

type SyncM = ContT (Either SyncError ()) WebApp

downloadEntitiesImpl :: Maybe UserId -> WS.Connection -> WebApp ()
downloadEntitiesImpl mayCallerUserId conn = do
  Debug.debugLogM Debug.Temp "Got connection"
  -- Auth is currently done via HashJWTs
  _authZReceipt <- AuthZ.checkDownloadFromUserCodebase
  doSyncEmitter mayCallerUserId conn

-- | Given a helper which understands how to wire things into its backend, This
-- implements the sync emitter logic which is independent of the backend.
doSyncEmitter ::
  Maybe UserId ->
  WS.Connection ->
  WebApp ()
doSyncEmitter mayCallerUserId conn = do
  withQueues @(MsgOrError SyncError (FromEmitterMessage Hash32 Text)) @(MsgOrError SyncError (FromReceiverMessage HashJWT Hash32))
    recvBufferSize
    sendBufferSize
    conn
    \(q@Queues {receive}) -> do
      handleErr q $ do
        withErrorCont \onErr -> do
          Debug.debugLogM Debug.Temp "Got queues"
          let recvM :: SyncM (FromReceiverMessage HashJWT Hash32)
              recvM = do
                result <- liftIO $ atomically receive
                Debug.debugM Debug.Temp "Received: " result
                case result of
                  Msg msg -> pure msg
                  Err err -> onErr err

          Debug.debugLogM Debug.Temp "Waiting for init message"
          initMsg <- recvM
          Debug.debugM Debug.Temp "Got init: " initMsg
          syncState <- case initMsg of
            ReceiverInitStream initMsg -> initialize onErr mayCallerUserId initMsg
            other -> onErr $ InitializationError ("Expected ReceiverInitStream message, got: " <> tShow other)
          Debug.debugLogM Debug.Temp "Initialized sync state, starting sync process."
          lift (shareEmitter syncState q)
            >>= maybe (pure ()) (onErr)
  where
    -- Given a continuation-based action, run it in the base monad, capturing any early exits
    withErrorCont ::
      ((forall x. SyncError -> SyncM x) -> SyncM ()) ->
      WebApp (Either SyncError ())
    withErrorCont action = do
      flip runContT pure $ callCC \cc -> do
        Right <$> action (fmap absurd . cc . Left)
    -- If we get an error, send it to the client then shut down.
    handleErr ::
      (Show err) =>
      Queues (MsgOrError err a) o ->
      WebApp (Either err ()) ->
      WebApp ()
    handleErr (Queues {send, shutdown}) action = do
      action >>= \case
        Left err -> do
          Debug.debugM Debug.Temp "Sync error, shutting down: " err
          atomically $ do
            send (Err err)
          liftIO $ shutdown
        Right r -> pure r

initialize :: (forall x. SyncError -> SyncM x) -> (Maybe UserId) -> InitMsg HashJWT -> SyncM (SyncState sh Hash32)
initialize onErr caller InitMsg {initMsgRootCausal, initMsgBranchRef} = do
  let decoded = HashJWT.decodeHashJWT initMsgRootCausal
  Debug.debugM Debug.Temp "Decoded root causal hash jwt" decoded
  Debug.debugM Debug.Temp "Caller: " caller
  HashJWTClaims {hash = initialCausalHash} <-
    lift (HashJWT.verifyHashJWT caller initMsgRootCausal) >>= \case
      Right ch -> pure ch
      Left err -> onErr $ HashJWTVerificationError (AuthN.authErrMsg err)
  validRequestsVar <- newTVarIO (Set.singleton (CausalEntity, initialCausalHash))
  requestedEntitiesVar <- newTVarIO (Set.singleton (CausalEntity, initialCausalHash))
  entitiesAlreadySentVar <- newTVarIO Set.empty
  (lift . runExceptT $ codebaseForBranchRef initMsgBranchRef) >>= \case
    Left err -> case err of
      CodebaseLoadingErrorNoReadPermission {} -> onErr $ NoReadPermission initMsgBranchRef
      CodebaseLoadingErrorProjectNotFound {} -> onErr $ ProjectNotFound initMsgBranchRef
      CodebaseLoadingErrorUserNotFound {} -> onErr $ UserNotFound initMsgBranchRef
      CodebaseLoadingErrorInvalidBranchRef msg _ -> onErr $ InvalidBranchRef msg initMsgBranchRef
    Right codebase ->
      pure $
        SyncState
          { codebase,
            validRequestsVar,
            requestedEntitiesVar,
            entitiesAlreadySentVar
          }

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
  Queues (MsgOrError SyncError (FromEmitterMessage Hash32 Text)) (MsgOrError SyncError (FromReceiverMessage HashJWT Hash32)) ->
  WebApp (Maybe SyncError)
shareEmitter SyncState {requestedEntitiesVar, entitiesAlreadySentVar, validRequestsVar, codebase} (Queues {send, receive}) = do
  Ki.scoped $ \scope -> do
    errVar <- newEmptyTMVarIO
    let onErrSTM :: SyncError -> STM ()
        onErrSTM e = do
          UnliftIO.putTMVar errVar e

    Debug.debugLogM Debug.Temp "Launching workers"
    Ki.fork scope $ sendWorker onErrSTM
    Ki.fork scope $ receiveWorker onErrSTM
    Debug.debugLogM Debug.Temp "Waiting on errors or completion..."
    atomically ((Ki.awaitAll scope $> Nothing) <|> (Just <$> UnliftIO.takeTMVar errVar))
  where
    sendWorker :: (SyncError -> STM ()) -> WebApp ()
    sendWorker onErrSTM = forever $ do
      (validRequests, reqs) <- atomically $ do
        reqs <- readTVar requestedEntitiesVar
        writeTVar requestedEntitiesVar Set.empty
        alreadySent <- readTVar entitiesAlreadySentVar
        Debug.debugM Debug.Temp "Processing Requested entities: " reqs
        validRequests <- readTVar validRequestsVar
        pure (validRequests, reqs `Set.difference` alreadySent)
      let forbiddenRequests = Set.difference reqs validRequests
      validatedRequests <-
        if not (Set.null forbiddenRequests)
          then do
            atomically (onErrSTM (ForbiddenEntityRequest forbiddenRequests))
            pure $ Set.difference validRequests forbiddenRequests
          else do
            pure validRequests
      Debug.debugM Debug.Temp "Validated requests: " validatedRequests
      when (not $ Set.null validatedRequests) $ do
        Debug.debugM Debug.Temp "Fetching Entities." validatedRequests
        newEntities <- fetchEntities codebase validatedRequests
        Debug.debugM Debug.Temp "Fetched entities: " (length newEntities)
        -- Do work outside of transactions to avoid conflicts
        deps <- UnliftIO.evaluate $ foldMap entityDependencies newEntities
        Debug.debugLogM Debug.Temp "Adding new valid requests"
        atomically $ modifyTVar' validRequestsVar (\s -> Set.union s deps)
        Debug.debugM Debug.Temp "Sending entities: " (length newEntities)
        atomically $ do
          let newHashes = setOf (folded . to (entityKind &&& entityHash)) newEntities
          modifyTVar' entitiesAlreadySentVar (Set.union newHashes)
          for_ newEntities \entity -> do
            send $ Msg (EmitterEntityMsg entity)

    receiveWorker :: (SyncError -> STM ()) -> WebApp ()
    receiveWorker onErrSTM = forever $ do
      atomically $ do
        receive >>= \case
          Err err -> onErrSTM err
          Msg (ReceiverInitStream {}) -> onErrSTM (InitializationError "Received duplicate ReceiverInitStream message")
          Msg (ReceiverEntityRequest (EntityRequestMsg {hashes})) -> do
            Debug.debugM Debug.Temp "Got new entity requests" hashes
            modifyTVar' requestedEntitiesVar (\s -> Set.union s (Set.fromList hashes))

fetchEntities :: CodebaseEnv -> Set (EntityKind, Hash32) -> WebApp (Vector (Entity Hash32 Text))
fetchEntities codebase reqs = do
  PG.runTransaction $ Q.fetchSerialisedEntities codebase reqs

-- entityDependencies :: Entity hash text -> Set (EntityKind, Hash32)
-- entityDependencies (Entity {entityData}) = do
