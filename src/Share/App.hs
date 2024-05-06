{-# LANGUAGE InstanceSigs #-}

module Share.App where

import Control.Monad.Except
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Crypto.JWT qualified as JWT
import Crypto.Random.Types qualified as Cryptonite
import Data.Set qualified as Set
import Database.Redis qualified as R
import Share.Env (Env (..))
import Share.Env qualified as Env
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Servant

newtype AppM reqCtx a = AppM {unAppM :: ReaderT (Env reqCtx) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env reqCtx), MonadRandom, MonadIO, MonadUnliftIO)

type CloudApp = AppM ()

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

runAppM :: Env reqCtx -> AppM reqCtx a -> IO a
runAppM env (AppM m) = runReaderT m env

instance R.MonadRedis (AppM reqCtx) where
  liftRedis m = do
    redis <- asks Env.redisConnection
    liftIO $ R.runRedis redis m

instance R.RedisCtx (AppM reqCtx) (Either R.Reply) where
  returnDecode r = do
    R.liftRedis $ R.returnDecode r

-- | JWT Issuer, currently just root URI
enlilIssuer :: AppM reqCtx URI
enlilIssuer = do
  asks Env.apiOrigin

-- | JWT Audience, currently the same as the issuer.
enlilAud :: AppM reqCtx (Set URI)
enlilAud = Set.singleton <$> enlilIssuer
