-- Multipurpose caching for things which can serialize to JSON.
--
-- Note that this will often involve deserializing, then reserializing the value
-- if it's going to be dumped to a response, which may be slightly less efficient than other
-- methods, but grants a lot of flexibility and simplicity as a general approach.
module Share.Utils.Caching.JSON (CacheKey (..), usingJSONCache) where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Servant.Server qualified as Servant
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors

data CacheKey = CacheKey
  { cacheTopic :: Text,
    -- Ordered key-value pairs to make up a cache key.
    key :: [(Text, Text)],
    -- The causal id which this cache entry is derived from.
    -- Leave as 'Nothing' if the cache entry is not derived from a causal id.
    rootCausalId :: Maybe CausalId,
    -- 'Nothing' is its own global sandbox, and should only be used for
    -- things which are not user-specific.
    sandbox :: Maybe UserId
  }
  deriving (Show)

encodeKey :: CacheKey -> Text
encodeKey (CacheKey {key, rootCausalId}) =
  let keyWithCausal = maybe key (\(CausalId rci) -> ("rootCausalId", tShow @Int32 rci) : key) rootCausalId
   in keyWithCausal
        <&> (\(k, v) -> k <> "=" <> v)
          & T.intercalate ","

usingJSONCache ::
  (ToJSON v, FromJSON v, PG.QueryM m) =>
  CacheKey ->
  -- How to build the value if it's not in the cache.
  m v ->
  m v
usingJSONCache ck action = do
  -- Don't cache on local.
  case Deployment.deployment of
    Deployment.Local -> action
    _ -> do
      getJSONCacheEntry ck >>= \case
        Just v -> pure v
        Nothing -> do
          v <- action
          putJSONCacheEntry ck v
          pure v

data JSONCacheError
  = JSONCacheDecodingError CacheKey Text
  deriving (Show)

instance ToServerError JSONCacheError where
  toServerError (JSONCacheDecodingError ck err) =
    (ErrorID "json-cache:decoding-error", Servant.err500 {Servant.errBody = BL.fromStrict $ Text.encodeUtf8 $ "Error decoding JSON cache entry: " <> tShow ck <> " - " <> err})

instance Logging.Loggable JSONCacheError where
  toLog (JSONCacheDecodingError ck err) =
    Logging.textLog ("Error decoding JSON cache entry: " <> encodeKey ck <> ",  " <> tShow ck <> ", Error: " <> err)
      & Logging.withSeverity Logging.Error
      & Logging.withTag ("cacheTopic", cacheTopic ck)
      & Logging.withTag ("sandbox", tShow $ sandbox ck)
      & Logging.withTag ("rootCausalId", tShow $ rootCausalId ck)

getJSONCacheEntry :: (FromJSON v, PG.QueryM m) => CacheKey -> m (Maybe v)
getJSONCacheEntry ck@(CacheKey {cacheTopic, sandbox}) = do
  let cacheKey = encodeKey ck
  r <-
    PG.query1Col @Text
      [PG.sql|
    SELECT jc.value
    FROM json_cache jc
    WHERE topic = #{cacheTopic}
      AND key = #{cacheKey}
      AND codebase_user_id = #{sandbox}
    LIMIT 1
  |]
  case r of
    Nothing -> pure Nothing
    Just valText ->
      case Aeson.eitherDecode (BL.fromStrict . Text.encodeUtf8 $ valText) of
        Left _err -> do
          -- reportError $ JSONCacheDecodingError ck (T.pack err)
          pure Nothing
        Right v -> pure $ Just v

putJSONCacheEntry :: (ToJSON v, PG.QueryM m) => CacheKey -> v -> m ()
putJSONCacheEntry ck@(CacheKey {cacheTopic, sandbox}) v = do
  let keyText = encodeKey ck
  let valText = Text.decodeUtf8 . BL.toStrict $ Aeson.encode v
  PG.execute_
    [PG.sql|
      INSERT INTO json_cache (topic, key, codebase_user_id, value)
      VALUES (#{cacheTopic}, #{keyText}, #{sandbox}, #{valText}::jsonb)
      ON CONFLICT (topic, key, codebase_user_id)
      DO UPDATE SET value = EXCLUDED.value
    |]
