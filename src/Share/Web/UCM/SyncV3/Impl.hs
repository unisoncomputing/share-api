{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.Impl where

import Codec.Serialise qualified as CBOR
import Control.Lens hiding ((.=))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
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
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.Orphans ()
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
data FromReceiverMessage ah sh
  = InitStream (InitMsg ah)
  | EntityRequest (EntityRequestMsg sh)

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
data FromEmitterMessage hash smallHash text
  = ErrorMsg SyncError
  | HashMappingsMsg (HashMappings hash smallHash)
  | EntityMsg (Entity smallHash hash text)

instance (CBOR.Serialise hash, CBOR.Serialise smallHash) => WebSocketsData (FromEmitterMessage hash smallHash text) where
  fromLazyByteString bytes =
    CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
      & either (\err -> ErrorMsg . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) id

  toLazyByteString = CBOR.serialise

  fromDataMessage dm = do
    case dm of
      WS.Text bytes _ -> WS.fromLazyByteString bytes
      WS.Binary bytes -> WS.fromLazyByteString bytes

data HashMappings hash smallHash = HashMappings
  { hashMappings :: [(smallHash, hash)]
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

data Entity smallHash hash text = Entity
  { entityHash :: smallHash,
    entityKind :: EntityKind,
    entityData :: Entity.SyncEntity' text hash hash hash hash hash hash
  }

instance (CBOR.Serialise smallHash, CBOR.Serialise hash, CBOR.Serialise text) => CBOR.Serialise (Entity smallHash hash text) where
  encode (Entity {entityHash, entityKind, entityData}) =
    CBOR.encode (entityHash, entityKind, entityData)

  decode = do
    (entityHash, entityKind, entityData) <- CBOR.decode @(smallHash, EntityKind, ByteString)
    pure $ Entity {entityHash, entityKind, entityData}

instance (CBOR.Serialise hash, CBOR.Serialise smallHash) => CBOR.Serialise (HashMappings hash smallHash) where
  encode (HashMappings {hashMappings}) =
    CBOR.encode hashMappings

  decode = do
    hashMappings <- CBOR.decode @[(smallHash, hash)]
    pure $ HashMappings {hashMappings}

instance (CBOR.Serialise hash, CBOR.Serialise smallHash) => CBOR.Serialise (FromEmitterMessage hash smallHash text) where
  encode = \case
    ErrorMsg err -> CBOR.encode ErrorMsgTag <> CBOR.encode err
    HashMappingsMsg msg -> CBOR.encode HashMappingsTag <> CBOR.encode msg
    EntityMsg msg -> CBOR.encode EntityTag <> CBOR.encode msg

  decode = do
    tag <- CBOR.decode @FromEmitterMessageTag
    case tag of
      ErrorMsgTag -> ErrorMsg <$> CBOR.decode
      HashMappingsTag -> HashMappingsMsg <$> CBOR.decode
      EntityTag -> EntityMsg <$> CBOR.decode

data FromEmitterMessageTag
  = ErrorMsgTag
  | HashMappingsTag
  | EntityTag

instance CBOR.Serialise FromEmitterMessageTag where
  encode = \case
    ErrorMsgTag -> CBOR.encode (0 :: Int)
    HashMappingsTag -> CBOR.encode (1 :: Int)
    EntityTag -> CBOR.encode (2 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure ErrorMsgTag
      1 -> pure HashMappingsTag
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

instance (CBOR.Serialise h, ToJSON ah, FromJSON ah) => WebSocketsData (MsgOrError SyncError (FromReceiverMessage ah h)) where
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
  withQueues @(FromEmitterMessage Hash32 HashTag Text) @(MsgOrError SyncError (FromReceiverMessage HashJWT Hash32))
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
      lift (shareSync syncState q) >>= \case
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

initialize :: InitMsg ah -> WebApp (SyncState sh hash)
initialize = undefined

data SyncState sh hash = SyncState
  { -- Entities which have been requested by the client but not yet sent.
    requestedEntitiesVar :: TVar (Set sh),
    -- Hashes which have been sent to the client
    hashesAlreadySentVar :: TVar (Set sh),
    -- Hash mappings we've already sent to the client.
    mappedHashesVar :: TVar (Map sh hash)
  }

shareSync ::
  (SyncState HashTag Hash32) ->
  Queues (FromEmitterMessage Hash32 HashTag Text) (MsgOrError SyncError (FromReceiverMessage HashJWT Hash32)) ->
  WebApp (Maybe SyncError)
shareSync SyncState {requestedEntitiesVar, hashesAlreadySentVar, mappedHashesVar} (Queues {send, receive, shutdown}) = Ki.scoped $ \scope -> do
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
    sendWorker onErr = do
      reqs <- atomically $ do
        reqs <- readTVar requestedEntitiesVar
        guard (not $ Set.null reqs)
        -- TODO: Add reasonable batch sizes
        modifyTVar' requestedEntitiesVar (const Set.empty)
        sent <- readTVar hashesAlreadySentVar
        let unsent = Set.difference reqs sent
        guard (not $ Set.null unsent)
        pure unsent
      newEntities <- fetchEntities reqs
      let hashMappings :: Map HashTag Hash32
          hashMappings =
            newEntities
              & toListOf (folded . folded)
              & Map.fromList
      atomically $ do
        alreadyMapped <- readTVar mappedHashesVar
        let newMappings = Map.difference hashMappings alreadyMapped
        modifyTVar' mappedHashesVar (Map.union newMappings)
        send (HashMappingsMsg (HashMappings (Set.toList newMappings)))

      atomically $ do
        modifyTVar' hashesAlreadySentVar (\s -> s <> Set.fromList (map entityHash newEntities))
        for newEntities \entity -> do
          send (EntityMsg entity)

    receiveWorker :: (SyncError -> STM ()) -> WebApp ()
    receiveWorker onErr = forever do
      atomically $ do
        receive >>= \case
          Err err -> onErr err
          Msg (InitStream {}) -> onErr (InitializationError "Received duplicate InitStream message")
          Msg (EntityRequest (EntityRequestMsg {hashes})) -> do
            modifyTVar' requestedEntitiesVar (\s -> s <> Set.fromList hashes)

    recvM :: ExceptT SyncError m (FromReceiverMessage ah hash)
    recvM = do
      result <- liftIO $ atomically receive
      case result of
        Msg msg -> pure msg
        Err err -> throwError err

fetchEntities :: Set sh -> WebApp [Entity HashTag (HashTag, Hash32) Text]
fetchEntities shs = _

-- Application level compression of Hash references.
-- We can send a mapping of Hash <-> HashTag at the start of the stream,
-- and then use the smaller HashTag in all subsequent messages.
data HashTag = HashTag (EntityKind, Int64)

instance CBOR.Serialise HashTag where
  encode (HashTag (kind, idx)) =
    CBOR.encode (kind, idx)

  decode = do
    (kind, idx) <- CBOR.decode @(EntityKind, Int64)
    pure $ HashTag (kind, idx)
