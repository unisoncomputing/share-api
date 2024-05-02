{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Helpers for working with Redis
module Enlil.Redis where

import Data.Binary
import Data.Binary qualified as Binary
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Time
import Data.Typeable (TypeRep, typeRep)
import Database.Redis qualified as R
import Enlil.Prelude
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.App
import Enlil.Web.Errors
import Enlil.Web.Errors qualified as Errors

data RedisErr
  = RedisErr R.Reply
  | -- We failed to decode the value at a key, likely because the binary format for that
    -- entity was changed in a backwards-incompatible way.
    --          key        errMsg type
    DecodingErr ByteString String TypeRep
  deriving stock (Show)
  deriving anyclass (Exception)

instance Logging.Loggable RedisErr where
  toLog = Logging.withSeverity Logging.Error . Logging.showLog

instance Errors.ToServerError RedisErr where
  toServerError = \case
    RedisErr {} -> (ErrorID "redis:redis-err", Errors.internalServerError)
    DecodingErr {} -> (ErrorID "redis:decoding-err", Errors.internalServerError)

class RedisKey a where
  redisKey :: a -> ByteString

redisPut :: (RedisKey a, Binary a) => Maybe NominalDiffTime -> a -> WebApp ()
redisPut mayExpiry a = do
  let key = redisKey a
  let value = toStrict $ encode a
  void . handleErrReply $ case mayExpiry of
    Just expirySeconds -> do
      R.setex key (floor expirySeconds) value
    Nothing -> R.set key value

handleErrReply :: WebApp (Either R.Reply a) -> WebApp a
handleErrReply m = m >>= either (respondError . RedisErr) pure

redisGet :: forall a. (Typeable a, Binary a) => ByteString -> WebApp (Maybe a)
redisGet key = do
  a <-
    handleErrReply (R.get key) >>= \case
      Nothing -> pure Nothing
      (Just bs) ->
        case Binary.decodeOrFail (fromStrict bs) of
          Left (_, _, errMsg) -> Errors.respondError $ DecodingErr key errMsg (typeRep (Proxy @a))
          Right (_, _, a) -> pure (Just a)
  pure a

redisDelete :: ByteString -> WebApp ()
redisDelete key = do
  void $ handleErrReply (R.del [key])
