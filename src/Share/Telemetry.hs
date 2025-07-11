module Share.Telemetry (withSpan) where

import Data.Text (Text)
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad qualified as TraceM
import UnliftIO

withSpan :: (MonadUnliftIO m, TraceM.MonadTracer m) => Text -> m a -> m a
withSpan name action = do
  let spanAttributes = mempty
  let spanArguments =
        Trace.SpanArguments
          { kind = Trace.Server,
            attributes = spanAttributes,
            links = [],
            startTime = Nothing -- This will be set automatically
          }
  TraceM.inSpan name spanArguments $ action
