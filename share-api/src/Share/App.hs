{-# LANGUAGE InstanceSigs #-}

module Share.App
  ( AppM,
    runAppM,
    shareIssuer,
    shareAud,
  )
where

import Control.Monad.Random.Strict
import Control.Monad.Reader
import Crypto.Random.Types qualified as Cryptonite
import Data.Map qualified as Map
import Data.Set qualified as Set
import Database.Redis qualified as R
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Share.Env.Types (Env (..))
import Share.Env.Types qualified as Env
import Share.JWT.Types (Audience (..), Issuer (..))
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.Tags (HasTags (..), MonadTags (..))

newtype AppM reqCtx a = AppM {_unAppM :: ReaderT (Env reqCtx) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env reqCtx), MonadRandom, MonadIO, MonadUnliftIO)

instance (HasTags ctx) => MonadTags (AppM ctx) where
  askTags = ask >>= getTags
  withTags newTags = local (\env -> env {ctx = addTags newTags $ Env.ctx env})

instance (HasTags ctx) => Logging.MonadLogger (AppM ctx) where
  logMsg msg = do
    log <- asks Env.logger
    currentTags <- askTags
    msg <- pure $ msg {Logging.tags = Logging.tags msg `Map.union` currentTags}
    minSeverity <- asks Env.minLogSeverity
    when (Logging.severity msg >= minSeverity) $ do
      timestamp <- asks timeCache >>= liftIO
      liftIO . log . Logging.logFmtFormatter timestamp $ msg

instance MonadTracer (AppM reqCtx) where
  getTracer = asks Env.tracer

runAppM :: Env reqCtx -> AppM reqCtx a -> IO a
runAppM env (AppM m) = runReaderT m env

instance Cryptonite.MonadRandom (AppM reqCtx) where
  getRandomBytes =
    liftIO . Cryptonite.getRandomBytes

instance R.MonadRedis (AppM reqCtx) where
  liftRedis m = do
    redis <- asks Env.redisConnection
    liftIO $ R.runRedis redis m

instance R.RedisCtx (AppM reqCtx) (Either R.Reply) where
  returnDecode r = do
    R.liftRedis $ R.returnDecode r

-- | JWT Issuer, currently just root URI
shareIssuer :: AppM reqCtx Issuer
shareIssuer = do
  Issuer <$> asks Env.apiOrigin

-- | JWT Audience, currently the same as the issuer.
shareAud :: AppM reqCtx (Set Audience)
shareAud =
  shareIssuer
    <&> (\(Issuer uri) -> Audience uri)
    <&> Set.singleton
