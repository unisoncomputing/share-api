module Share.Telemetry
  ( withSpan,
    TracerT (..),
    runTracerT,
    AttributeMap,
    Trace.toAttribute,
  )
where

import Control.Monad.Reader (MonadReader (..), MonadTrans, ReaderT (..))
import Data.HashMap.Lazy qualified as HM
import Data.Map qualified as Map
import Data.Text (Text)
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer)
import OpenTelemetry.Trace.Monad qualified as TraceM
import Share.Env (HasTags)
import Share.Env qualified as Env
import UnliftIO

type AttributeMap = HM.HashMap Text Trace.Attribute

withSpan :: (MonadUnliftIO m, TraceM.MonadTracer m, HasTags ctx, MonadReader ctx m) => Text -> AttributeMap -> m a -> m a
withSpan name spanTags action = do
  tags <- ask >>= Env.getTags
  let spanAttributes = spanTags <> HM.fromList (Map.toList (Trace.toAttribute <$> tags))
  let spanArguments =
        Trace.SpanArguments
          { kind = Trace.Server,
            attributes = spanAttributes,
            links = [],
            startTime = Nothing -- This will be set automatically
          }
  TraceM.inSpan name spanArguments $ action

-- | Helper for adding MonadTracer.
newtype TracerT m a = TracerT
  { unTracerT :: ReaderT Trace.Tracer m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadTrans)

runTracerT :: Trace.Tracer -> TracerT m a -> m a
runTracerT tracer (TracerT action) = runReaderT action tracer

instance (Monad m) => MonadTracer (TracerT m) where
  getTracer = TracerT $ ask
