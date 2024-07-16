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
import Data.Set qualified as Set
import Database.Redis qualified as R
import Servant
import Share.Env (Env (..))
import Share.Env qualified as Env
import Share.Prelude
import Share.Utils.Logging qualified as Logging

newtype AppM reqCtx a = AppM {_unAppM :: ReaderT (Env reqCtx) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env reqCtx), MonadRandom, MonadIO, MonadUnliftIO)

runAppM :: Env reqCtx -> AppM reqCtx a -> IO a
runAppM env (AppM m) = runReaderT m env

instance Logging.MonadLogger (AppM ()) where
  logMsg msg = do
    log <- asks Env.logger
    minSeverity <- asks Env.minLogSeverity
    when (Logging.severity msg >= minSeverity) $ do
      timestamp <- asks timeCache >>= liftIO
      liftIO . log . Logging.logFmtFormatter timestamp $ msg

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
shareIssuer :: AppM reqCtx URI
shareIssuer = do
  asks Env.apiOrigin

-- | JWT Audience, currently the same as the issuer.
shareAud :: AppM reqCtx (Set URI)
shareAud = Set.singleton <$> shareIssuer
