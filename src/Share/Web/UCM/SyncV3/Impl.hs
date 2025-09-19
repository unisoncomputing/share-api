{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncV3.Impl where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Either
import Network.WebSockets (WebSocketsData)
import Network.WebSockets qualified as WS
import Share.IDs (ProjectId, UserId)
import Share.Prelude
import Share.Utils.Servant.Websockets (Queues (..), withQueues)
import Share.Web.App
import Share.Web.UCM.SyncV3.API qualified as SyncV3
import U.Codebase.Sqlite.Orphans ()
import Unison.Share.API.Hash (HashJWT)
import Unison.Util.Servant.CBOR qualified as CBOR
import UnliftIO.STM

-- Amount of entities to buffer from the network into the send/recv queues.
sendBufferSize :: Int
sendBufferSize = 100

recvBufferSize :: Int
recvBufferSize = 100

data StreamInitInfo = StreamInitInfo

streamSettings :: StreamInitInfo
streamSettings = StreamInitInfo

server :: Maybe UserId -> SyncV3.Routes WebAppServer
server mayUserId =
  SyncV3.Routes
    { downloadEntities = downloadEntitiesImpl mayUserId
    }

data InitMsg = InitMsg
  { initMsgClientVersion :: Text,
    initMsgProjectId :: ProjectId,
    initMsgRootCausal :: HashJWT
  }
  deriving (Show, Eq)

instance ToJSON InitMsg where
  toJSON (InitMsg {..}) =
    object
      [ "clientVersion" .= initMsgClientVersion,
        "projectId" .= initMsgProjectId,
        "rootCausal" .= initMsgRootCausal
      ]

instance FromJSON InitMsg where
  parseJSON = withObject "InitMsg" $ \o ->
    InitMsg
      <$> o .: "clientVersion"
      <*> o .: "projectId"
      <*> o .: "rootCausal"

data JWTEntityRequestMsg = JWTEntityRequestMsg
  { jwt :: [HashJWT]
  }

data EntityRef

data SubsequentEntityRequestMsg = SubsequentEntityRequestMsg
  -- TODO: Should probably use internal Int references to things we've previously referenced to
  -- avoid unnecessary back and forth, and make for more efficient DB lookups by using literal
  -- DB Ids.
  { entityHashes :: [EntityRef]
  }

data FromReceiverMessageTag
  = InitStreamTag
  | JWTEntityRequestTag
  | SubsequentEntityRequestTag

instance CBOR.Serialise FromReceiverMessageTag where
  encode = \case
    InitStreamTag -> CBOR.encode (0 :: Int)
    JWTEntityRequestTag -> CBOR.encode (1 :: Int)
    SubsequentEntityRequestTag -> CBOR.encode (2 :: Int)

  decode = do
    tag <- CBOR.decode @Int
    case tag of
      0 -> pure InitStreamTag
      1 -> pure JWTEntityRequestTag
      2 -> pure SubsequentEntityRequestTag
      _ -> fail $ "Unknown FromReceiverMessageTag: " <> show tag

-- A message sent from the downloader to the emitter.
data FromReceiverMessage
  = InitStream InitMsg
  | JWTEntityRequest JWTEntityRequestMsg
  | SubsequentEntityRequest SubsequentEntityRequestMsg



instance CBOR.Serialise InitMsg where
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

instance CBOR.Serialise FromReceiverMessage where
  encode = \case
    InitStream initMsg ->
      CBOR.encode InitStreamTag
        CBOR.encode initMsg
    JWTEntityRequest msg ->
      CBOR.encode JWTEntityRequestTag
        <> CBOR.encode msg
    SubsequentEntityRequest msg ->
      CBOR.encode SubsequentEntityRequestTag
        <> CBOR.encode msg

data SyncError
  = MissingInitialization Text
  | UnexpectedMessage BL.ByteString
  | EncodingFailure Text

-- A message sent from the emitter to the downloader.
data FromEmitterMessage
  = ErrorMsg SyncError

instance WebSocketsData (Either SyncError FromReceiverMessage) where
  fromDataMessage dm = do
    let bytes = WS.fromDataMessage @BL.ByteString dm
     in CBOR.deserialiseOrFailCBORBytes (CBOR.CBORBytes bytes)
          & either (\err -> Left . EncodingFailure $ "Error decoding CBOR message from bytes: " <> tShow err) Right

downloadEntitiesImpl :: Maybe UserId -> WS.Connection -> WebApp ()
downloadEntitiesImpl mayCallerUserId conn = do
  withQueues @FromEmitterMessage @FromReceiverMessage
    recvBufferSize
    sendBufferSize
    conn
    \(q@Queues {receive, send}) -> handleErr q $ do
      initMsg <- liftIO $ atomically receive
      syncState <- case initMsg of
        InitStream initMsg -> lift $ initialize initMsg
        other -> throwError $ MissingInitialization ("Expected InitStream message, got: " <> tShow other)
      sync syncState q
  where
    handleErr :: (MonadIO m) => (Queues FromEmitterMessage FromReceiverMessage) -> ExceptT SyncError m () -> m ()
    handleErr (Queues {send, shutdown}) action = do
      runExceptT action >>= \case
        Left err -> do
          atomically $ do
            send (ErrorMsg err)
          liftIO $ shutdown
        Right r -> pure r

initialize :: InitMsg -> WebApp SyncState
initialize = undefined

data SyncState

sync :: SyncState -> Queues FromEmitterMessage FromReceiverMessage -> ExceptT SyncError WebApp ()
sync syncState (Queues {send, receive, shutdown}) = do
  _
