-- | Initial Telemetry setup.
module Share.Telemetry.Setup (withTracer) where

import Data.Text (Text)
import OpenTelemetry.Attributes qualified as Trace
import OpenTelemetry.Trace qualified as Trace
import UnliftIO

withTracer :: Text -> (Trace.Tracer -> IO c) -> IO c
withTracer commitHash f =
  bracket
    -- Install the SDK, pulling configuration from the environment
    Trace.initializeGlobalTracerProvider
    -- Ensure that any spans that haven't been exported yet are flushed
    Trace.shutdownTracerProvider
    -- Get a tracer so you can create spans
    (\tracerProvider -> f $ Trace.makeTracer tracerProvider instrumentationLibrary tracerOptions)
  where
    tracerOptions = Trace.TracerOptions {Trace.tracerSchema = Nothing}
    instrumentationLibrary =
      Trace.InstrumentationLibrary
        { Trace.libraryName = "share-api",
          Trace.libraryVersion = commitHash,
          Trace.librarySchemaUrl = "",
          Trace.libraryAttributes = Trace.emptyAttributes
        }
