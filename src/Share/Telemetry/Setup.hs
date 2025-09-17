-- | Initial Telemetry setup.
module Share.Telemetry.Setup (withTracer) where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HM
import Data.Text qualified as Text
import Network.URI qualified as URI
import OpenTelemetry.Attributes qualified as Trace
import OpenTelemetry.Baggage qualified as Baggage
import OpenTelemetry.Context (Context)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Id (TraceId)
import OpenTelemetry.Trace.Sampler qualified as Sampler
import OpenTelemetry.Trace.TraceState (TraceState)
import OpenTelemetry.Trace.TraceState qualified as TraceState
import Share.Prelude
import Unison.Debug qualified as Debug

initSampler :: IO Sampler.Sampler
initSampler =
  Trace.detectSampler
    <&> \defaultSampler ->
      defaultSampler {Sampler.shouldSample = shouldSample defaultSampler}
  where
    lookupTextAttribute :: Text -> Trace.SpanArguments -> Maybe Text
    lookupTextAttribute key args =
      HM.lookup key (Trace.attributes args) >>= \case
        Trace.AttributeValue (Trace.TextAttribute t) -> Just t
        _ -> Nothing
    dropSample = (Sampler.Drop, HM.empty, TraceState.empty)
    -- Configure some custom sampling logic.
    shouldForceSample :: Context -> Bool
    shouldForceSample ctx = fromMaybe False do
      baggage <- Context.lookupBaggage ctx
      let baggageMap = Baggage.values baggage
      forceTraceToken <- Baggage.mkToken "force-trace"
      forceTrace <- Baggage.value <$> HM.lookup forceTraceToken baggageMap
      Debug.debugLogM Debug.Temp ("Force trace token: " <> show forceTrace)
      case Text.toLower forceTrace of
        "false" -> Just False
        "0" -> Just False
        _ -> Just True
    shouldSample :: Sampler.Sampler -> Context -> TraceId -> Text -> Trace.SpanArguments -> IO (Sampler.SamplingResult, HashMap Text Trace.Attribute, TraceState)
    shouldSample defaultSampler ctx tid name args
      | shouldForceSample ctx = Sampler.shouldSample Sampler.alwaysOn ctx tid name args
      | otherwise = do
          case (lookupTextAttribute "http.target" args >>= URI.parseURIReference . Text.unpack) <&> URI.pathSegments of
            Just ("metrics" : _) -> pure dropSample
            Just ("health" : _) -> pure dropSample
            -- This is currently used in a health check.
            Just ("users" : "zarelit" : _) -> pure dropSample
            _ -> Sampler.shouldSample defaultSampler ctx tid name args

withTracer :: Text -> (Trace.Tracer -> IO c) -> IO c
withTracer commitHash f =
  bracket
    -- Install the SDK, pulling configuration from the environment
    ( do
        (spanProcessors, tp) <- Trace.getTracerProviderInitializationOptions
        sampler <- initSampler
        tp <- Trace.createTracerProvider spanProcessors (tp {Trace.tracerProviderOptionsSampler = sampler})
        Trace.setGlobalTracerProvider tp
        pure tp
    )
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
