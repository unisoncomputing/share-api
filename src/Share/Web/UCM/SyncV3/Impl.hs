{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.Impl where

import Codec.Serialise qualified as CBOR
import Control.Lens hiding ((.=))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.HashSet.Lens (setOf)
import GHC.Natural
import Ki.Unlifted qualified as Ki
import Network.WebSockets (WebSocketsData)
import Network.WebSockets qualified as WS
import Share.IDs (ProjectId, UserId)
import Share.Prelude
import Share.Utils.Servant.Websockets (Queues (..), withQueues)
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.UCM.SyncV3.API qualified as SyncV3
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.TempEntity
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWT)
import Unison.Util.Servant.CBOR qualified as CBOR
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

data InitMsg authedHash = InitMsg
  { initMsgClientVersion :: Text,
    initMsgProjectId :: ProjectId,
    initMsgRootCausal :: authedHash,
    initMsgRequestedDepth :: Maybe Int64
  }
  deriving (Show, Eq)

instance (ToJSON authedHash) => ToJSON (InitMsg authedHash) where
  toJSON (InitMsg {initMsgClientVersion, initMsgProjectId, initMsgRootCausal, initMsgRequestedDepth}) =
    object
      [ "clientVersion" .= initMsgClientVersion,
        "projectId" .= initMsgProjectId,
        "rootCausal" .= initMsgRootCausal,
        "requestedDepth" .= initMsgRequestedDepth
      ]

instance (FromJSON authedHash) => FromJSON (InitMsg authedHash) where
  parseJSON = withObject "InitMsg" $ \o ->
    InitMsg
      <$> o .: "clientVersion"
      <*> o .: "projectId"
      <*> o .: "rootCausal"
      <*> o .:? "requestedDepth"

data EntityRequestMsg sh = EntityRequestMsg
  { hashes :: [sh]
  }
  deriving (Show, Eq)

instance (CBOR.Serialise sh) => CBOR.Serialise (EntityRequestMsg sh) where
  encode (EntityRequestMsg {hashes}) =
    CBOR.encode hashes

  decode = do
    hashes <- CBOR.decode @[sh]
    pure $ EntityRequestMsg {hashes}

data EntityRef

data SubsequentEntityRequestMsg = SubsequentEntityRequestMsg
  -- TODO: Should probably use internal Int references to things we've previously referenced to
  -- avoid unnecessary back and forth, and make for more efficient DB lookups by using literal
  -- DB Ids.
  { entityHashes :: [EntityRef]
  }

data FromReceiverMessageTag
  = InitStreamTag
  | EntityRequestTag

instance CBOR.Serialise FromReceiverMessageTag where
  encode = \case
    InitStreamTag -> CBOR.encode (0 :: Int)
    EntityRequestTag -> CBOR.encode (1 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure InitStreamTag
      1 -> pure EntityRequestTag
      _ -> fail $ "Unknown FromReceiverMessageTag: " <> show tag

-- A message sent from the downloader to the emitter.
data FromReceiverMessage ah hash
  = InitStream (InitMsg ah)
  | EntityRequest (EntityRequestMsg hash)
  deriving (Show, Eq)

instance (ToJSON ah, FromJSON ah) => CBOR.Serialise (InitMsg ah) where
  encode msg = do
    -- This is dumb, but there's currently no reasonable way to encode a heterogenous Map
    -- using Haskell's CBOR library :|
    --
    -- See https://github.com/well-typed/cborg/issues/369
    CBOR.encode $ Aeson.encode msg

  decode = do
    bs <- CBOR.decode @BL.ByteString
    case Aeson.eitherDecode bs of
      Left err -> fail $ "Error decoding InitMsg from JSON: " <> err
      Right msg -> pure msg

instance (CBOR.Serialise h, ToJSON ah, FromJSON ah) => CBOR.Serialise (FromReceiverMessage ah h) where
  encode = \case
    InitStream initMsg ->
      CBOR.encode InitStreamTag
        <> CBOR.encode initMsg
    EntityRequest msg ->
      CBOR.encode EntityRequestTag
        <> CBOR.encode msg
  decode = do
    tag <- CBOR.decode @FromReceiverMessageTag
    case tag of
      InitStreamTag -> InitStream <$> CBOR.decode @(InitMsg ah)
      EntityRequestTag -> EntityRequest <$> CBOR.decode @(EntityRequestMsg h)

data SyncError
  = InitializationError Text
  | UnexpectedMessage BL.ByteString
  | EncodingFailure Text

instance CBOR.Serialise SyncError where
  encode = \case
    InitializationError msg ->
      CBOR.encode (0 :: Int) <> CBOR.encode msg
    UnexpectedMessage msg ->
      CBOR.encode (1 :: Int) <> CBOR.encode (BL.toStrict msg)
    EncodingFailure msg ->
      CBOR.encode (2 :: Int) <> CBOR.encode msg

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> InitializationError <$> CBOR.decode
      1 -> do
        bs <- CBOR.decode @ByteString
        pure $ UnexpectedMessage (BL.fromStrict bs)
      2 -> EncodingFailure <$> CBOR.decode
      _ -> fail $ "Unknown SyncError tag: " <> show tag

-- A message sent from the emitter to the downloader.
data FromEmitterMessage hash text
  = ErrorMsg SyncError
  | -- | HashMappingsMsg (HashMappings hash smallHash)
    EntityMsg (Entity hash text)

instance (CBOR.Serialise hash, CBOR.Serialise text) => WebSocketsData (FromEmitterMessage hash text) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
      & either (\err -> ErrorMsg . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) id

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

data HashMappings hash smallHash = HashMappings
  { hashMappings :: Map smallHash hash
  }

data EntityKind
  = CausalEntity
  | NamespaceEntity
  | TermEntity
  | TypeEntity
  | PatchEntity
  deriving (Show, Eq, Ord)

instance CBOR.Serialise EntityKind where
  encode = \case
    CausalEntity -> CBOR.encode (0 :: Int)
    NamespaceEntity -> CBOR.encode (1 :: Int)
    TermEntity -> CBOR.encode (2 :: Int)
    TypeEntity -> CBOR.encode (3 :: Int)
    PatchEntity -> CBOR.encode (4 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure CausalEntity
      1 -> pure NamespaceEntity
      2 -> pure TermEntity
      3 -> pure TypeEntity
      4 -> pure PatchEntity
      _ -> fail $ "Unknown EntityKind tag: " <> show tag

-- | The number of _levels_ of dependencies an entity has,
-- this has no real semantic meaning on its own, but provides the
-- property that out of a given set of synced entities, if you process
-- them in order of increasing EntityDepth, you will always have
-- processed an entity's dependencies before you see the entity itself.
newtype EntityDepth = EntityDepth {unEntityDepth :: Int64}
  deriving (Show, Eq, Ord)
  deriving newtype (CBOR.Serialise)

data Entity hash text = Entity
  { entityHash :: hash,
    entityKind :: EntityKind,
    entityDepth :: EntityDepth,
    entityData :: CBOR.CBORBytes TempEntity
  }

-- entityTexts_ :: Traversal (Entity smallHash text) (Entity smallHash text') text text'
-- entityTexts_ f (Entity {entityData, ..}) =
--   (\entityData' -> Entity {entityData = entityData', ..}) <$> Entity.texts_ f entityData

-- entityHashesSetter_ :: (Monad m) => LensLike m (Entity smallHash text) (Entity smallHash' text) smallHash smallHash'
-- entityHashesSetter_ f (Entity {entityHash, entityData, ..}) =
--   (\entityHash' entityData' -> Entity {entityHash = entityHash', entityData = entityData', ..})
--     <$> f entityHash
--     <*> ( entityData
--             & Entity.hashes_ f
--             >>= Entity.defns_ f
--             >>= Entity.patches_ f
--             >>= Entity.branchHashes_ f
--             >>= Entity.branches_ f
--             >>= Entity.causalHashes_ f
--         )

-- -- | It's technically possible to implement entityHashesGetter_ and entityHashesSetter_
-- -- as a single Traversal, but it's a ton of extra unpacking/packing that's probably not worth
-- -- it.
-- entityHashesGetter_ :: Fold (Entity smallHash text) smallHash
-- entityHashesGetter_ f (Entity {entityHash, entityData}) =
--   phantom (f entityHash)
--     *> phantom (Entity.hashes_ f entityData)
--     *> phantom (Entity.defns_ f entityData)
--     *> phantom (Entity.patches_ f entityData)
--     *> phantom (Entity.branchHashes_ f entityData)
--     *> phantom (Entity.branches_ f entityData)
--     *> phantom (Entity.causalHashes_ f entityData)

instance (CBOR.Serialise smallHash, CBOR.Serialise text) => CBOR.Serialise (Entity smallHash text) where
  encode (Entity {entityHash, entityKind, entityDepth, entityData}) =
    CBOR.encode entityHash
      <> CBOR.encode entityKind
      <> CBOR.encode entityDepth
      <> CBOR.encode entityData

  decode = do
    entityHash <- CBOR.decode @smallHash
    entityKind <- CBOR.decode @EntityKind
    entityDepth <- CBOR.decode @EntityDepth
    entityData <- CBOR.decode @(CBOR.CBORBytes TempEntity)

    pure $ Entity {entityHash, entityKind, entityData, entityDepth}

instance (Ord smallHash, CBOR.Serialise hash, CBOR.Serialise smallHash) => CBOR.Serialise (HashMappings hash smallHash) where
  encode (HashMappings {hashMappings}) =
    CBOR.encode hashMappings

  decode = do
    hashMappings <- CBOR.decode @(Map smallHash hash)
    pure $ HashMappings {hashMappings}

instance (CBOR.Serialise hash, CBOR.Serialise text) => CBOR.Serialise (FromEmitterMessage hash text) where
  encode = \case
    ErrorMsg err -> CBOR.encode ErrorMsgTag <> CBOR.encode err
    -- HashMappingsMsg msg -> CBOR.encode HashMappingsTag <> CBOR.encode msg
    EntityMsg msg -> CBOR.encode EntityTag <> CBOR.encode msg

  decode = do
    tag <- CBOR.decode @FromEmitterMessageTag
    case tag of
      ErrorMsgTag -> ErrorMsg <$> CBOR.decode
      -- HashMappingsTag -> HashMappingsMsg <$> CBOR.decode
      EntityTag -> EntityMsg <$> CBOR.decode

data FromEmitterMessageTag
  = ErrorMsgTag
  | -- | HashMappingsTag
    EntityTag

instance CBOR.Serialise FromEmitterMessageTag where
  encode = \case
    ErrorMsgTag -> CBOR.encode (0 :: Int)
    -- HashMappingsTag -> CBOR.encode (1 :: Int)
    EntityTag -> CBOR.encode (2 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure ErrorMsgTag
      -- 1 -> pure HashMappingsTag
      2 -> pure EntityTag
      _ -> fail $ "Unknown FromEmitterMessageTag: " <> show tag

data MsgOrError err a
  = Msg a
  | Err err

instance (CBOR.Serialise a, CBOR.Serialise err) => CBOR.Serialise (MsgOrError err a) where
  encode = \case
    Msg a -> CBOR.encode (0 :: Int) <> CBOR.encode a
    Err e -> CBOR.encode (1 :: Int) <> CBOR.encode e

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> Msg <$> CBOR.decode
      1 -> Err <$> CBOR.decode
      _ -> fail $ "Unknown MsgOrError tag: " <> show tag

instance (CBOR.Serialise sh, ToJSON ah, FromJSON ah) => WebSocketsData (MsgOrError SyncError (FromReceiverMessage ah sh)) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
      & either (\err -> Err . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) Msg

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

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
  { -- Entities which have been requested by the client but not yet sent.
    requestedEntitiesVar :: TVar (HashSet hash),
    -- Hashes which have been sent to the client
    entitiesAlreadySentVar :: TVar (HashSet hash)
    -- Hash mappings we've already sent to the client.
    -- mappedHashesVar :: TVar (Map sh hash)
  }

shareEmitter ::
  (SyncState HashTag Hash32) ->
  Queues (FromEmitterMessage Hash32 Text) (MsgOrError SyncError (FromReceiverMessage HashJWT Hash32)) ->
  WebApp (Maybe SyncError)
shareEmitter SyncState {requestedEntitiesVar, entitiesAlreadySentVar} (Queues {send, receive, shutdown}) = Ki.scoped $ \scope -> do
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
    sendWorker _onErr = forever $ do
      reqs <- atomically $ do
        reqs <- readTVar requestedEntitiesVar
        guard (not $ HashSet.null reqs)
        -- TODO: Add reasonable batch sizes
        modifyTVar' requestedEntitiesVar (const HashSet.empty)
        sent <- readTVar entitiesAlreadySentVar
        let unsent = HashSet.difference reqs sent
        guard (not $ HashSet.null unsent)
        pure unsent
      newEntities <- fetchEntities reqs
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
        let newHashes = setOf (folded . to entityHash) newEntities
        modifyTVar' entitiesAlreadySentVar (HashSet.union newHashes)
        for newEntities \entity -> do
          send (EntityMsg entity)

    receiveWorker :: (SyncError -> STM ()) -> WebApp ()
    receiveWorker onErr = forever $ do
      atomically $ do
        receive >>= \case
          Err err -> onErr err
          Msg (InitStream {}) -> onErr (InitializationError "Received duplicate InitStream message")
          Msg (EntityRequest (EntityRequestMsg {hashes})) -> do
            modifyTVar' requestedEntitiesVar (\s -> HashSet.union s (HashSet.fromList hashes))

fetchEntities :: HashSet Hash32 -> WebApp [Entity Hash32 Text]
fetchEntities shs = _

-- Application level compression of Hash references.
-- We can send a mapping of Hash <-> HashTag at the start of the stream,
-- and then use the smaller HashTag in all subsequent messages.
data HashTag = HashTag (EntityKind, Int64)
  deriving (Show, Eq, Ord)

instance CBOR.Serialise HashTag where
  encode (HashTag (kind, idx)) =
    CBOR.encode (kind, idx)

  decode = do
    (kind, idx) <- CBOR.decode @(EntityKind, Int64)
    pure $ HashTag (kind, idx)
