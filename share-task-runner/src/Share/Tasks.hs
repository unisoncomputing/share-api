module Share.Tasks where

import Share.BackgroundJobs.Monad
import UnliftIO

task :: Background ()
task = do
  liftIO $ putStrLn "Hello from the task runner!"
  cursor <- PG.newRowCursor @(CBORBytes TempEntity, Hash32) "component_cursor" [sql|
    (SELECT DISTINCT ON (t.component_hash_id) bytes.bytes, ch.base32
      FROM terms t
        JOIN serialized_components sc ON t.component_hash_id = sc.component_hash_id
        JOIN bytes ON sc.bytes_id = bytes.id
        JOIN component_hashes ch ON t.component_hash_id = ch.id
    )
    |]
  PG.foldBatched cursor 100 \rows -> do
    for rows \(bytes, hash32) -> do

  pure ()


unpackEntity :: (CBORBytes TempEntity) -> Either CBOR.DeserialiseFailure TempEntity
unpackEntity entityBytes = do
  case CBOR.deserialiseOrFailCBORBytes entityBytes of
    Left err -> Nothing
    Right entity -> pure entity
