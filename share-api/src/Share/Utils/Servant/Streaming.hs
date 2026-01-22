module Share.Utils.Servant.Streaming
  ( toConduit,
    cborStreamToConduit,
    fromConduit,
    sourceIOWithAsync,
    queueToCBORStream,
    queueToSourceIO,
  )
where

-- Orphan instances for SourceIO

import Codec.Serialise qualified as CBOR
import Conduit
import Control.Concurrent.STM.TBMQueue qualified as STM
import Control.Monad.Except
import Data.ByteString.Builder qualified as Builder
import Ki.Unlifted qualified as Ki
import Servant
import Servant.Conduit (conduitToSourceIO)
import Servant.Types.SourceT
import Share.Prelude
import Unison.Util.Servant.CBOR
import UnliftIO.STM qualified as STM

-- | Run the provided IO action in the background while streaming results.
--
-- Servant doesn't provide any easier way to do bracketing like this, all the IO must be
-- inside the SourceIO somehow.
sourceIOWithAsync :: IO a -> SourceIO r -> SourceIO r
sourceIOWithAsync action (SourceT k) =
  SourceT \k' ->
    Ki.scoped \scope -> do
      _ <- Ki.fork scope action
      k k'

toConduit :: (MonadIO m, MonadIO n) => SourceIO o -> m (ConduitT void o n ())
toConduit sourceIO = fmap (transPipe liftIO) . liftIO $ fromSourceIO $ sourceIO

cborStreamToConduit :: (MonadIO m, MonadIO n, CBOR.Serialise o) => SourceIO (CBORStream o) -> m (ConduitT void o (ExceptT CBORStreamError n) ())
cborStreamToConduit sourceIO = toConduit sourceIO <&> \stream -> (stream .| unpackCBORBytesStream)

fromConduit :: ConduitT void o IO () -> SourceIO o
fromConduit = conduitToSourceIO

queueToCBORStream :: forall a f. (CBOR.Serialise a, Foldable f) => STM.TBMQueue (f a) -> ConduitT () (CBORStream a) IO ()
queueToCBORStream q = do
  let loop :: ConduitT () (CBORStream a) IO ()
      loop = do
        liftIO (STM.atomically (STM.readTBMQueue q)) >>= \case
          -- The queue is closed.
          Nothing -> do
            pure ()
          Just batches -> do
            batches
              & foldMap (CBOR.serialiseIncremental)
              & (CBORStream . Builder.toLazyByteString)
              & Conduit.yield
            loop
  loop

queueToSourceIO :: forall a f. (CBOR.Serialise a, Foldable f) => STM.TBMQueue (f a) -> SourceIO (CBORStream a)
queueToSourceIO q = fromConduit (queueToCBORStream q)
