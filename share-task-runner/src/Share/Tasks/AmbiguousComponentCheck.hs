module Share.Tasks.AmbiguousComponentCheck (run) where

import Codec.Serialise qualified as CBOR
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as PG
import Share.Prelude
import Share.Utils.Logging (Loggable (..))
import Share.Utils.Logging qualified as Logging
import U.Codebase.Sqlite.TempEntity
import Unison.Hash32
import Unison.Sync.EntityValidation qualified as EV
import Unison.Sync.Types qualified as Sync
import Unison.Util.Servant.CBOR
import Unison.Util.Servant.CBOR qualified as CBOR

data AmbiguousComponentCheckError
  = TaskAmbiguousComponentCheckError Hash32
  | TaskEntityValidationError Hash32 (Sync.EntityValidationError)
  | TaskEntityDecodingError Hash32 CBOR.DeserialiseFailure
  deriving (Show, Eq)

instance Loggable AmbiguousComponentCheckError where
  toLog = \case
    TaskAmbiguousComponentCheckError hash32 ->
      Logging.textLog ("Ambiguous component found for hash: " <> into @Text hash32)
        & Logging.withSeverity Logging.Error
    TaskEntityValidationError hash32 validationError ->
      Logging.textLog
        ( "Entity validation error for hash: "
            <> into @Text hash32
            <> ", error: "
            <> into @Text (show validationError)
        )
        & Logging.withSeverity Logging.Error
    TaskEntityDecodingError hash32 decodeError ->
      Logging.textLog
        ( "Entity decoding error for hash: "
            <> into @Text hash32
            <> ", error: "
            <> into @Text (show decodeError)
        )
        & Logging.withSeverity Logging.Error

run :: Background ()
run = withWorkerName "ambiguous-component-task" do
  Logging.logInfoText "Starting ambiguous component check task."
  errs <- PG.runTransaction $ do
    cursor <-
      PG.newRowCursor @(CBORBytes TempEntity, Hash32)
        "component_cursor"
        [PG.sql|
        WITH component_hash_ids(component_hash_id) AS (
          SELECT DISTINCT component_hash_id
          FROM terms t
            WHERE t.component_index = 1
        ) SELECT DISTINCT ON (bytes.id) bytes.bytes, ch.base32
            FROM component_hash_ids chi
              JOIN serialized_components sc ON chi.component_hash_id = sc.component_hash_id
              JOIN bytes ON sc.bytes_id = bytes.id
              JOIN component_hashes ch ON chi.component_hash_id = ch.id
      |]
    PG.foldBatched cursor 100 \rows -> do
      rows
        & foldMap
          ( \(bytes, hash32) -> do
              case unpackEntity bytes of
                Left err -> [TaskEntityDecodingError hash32 err]
                Right entity -> do
                  case EV.validateTempEntity hash32 entity of
                    Nothing -> []
                    Just validationError -> [TaskEntityValidationError hash32 validationError]
          )
        & pure
  for_ errs reportError
  Logging.logInfoText "Finished ambiguous component check task."

unpackEntity :: (CBORBytes TempEntity) -> Either CBOR.DeserialiseFailure TempEntity
unpackEntity entityBytes = do
  case CBOR.deserialiseOrFailCBORBytes entityBytes of
    Left err -> Left err
    Right entity -> Right entity
