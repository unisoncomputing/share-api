{-# LANGUAGE RecordWildCards #-}

-- | Utilities for interacting with our metrics library.
--
-- If possible, try to keep details specific to our current metrics libraries isolated to
-- within this module so we can easily swap to new providers if needed.
module Share.Metrics
  ( serveMetricsMiddleware,
    requestMetricsMiddleware,
    tickUserSignup,
    recordBackgroundImportDuration,
    recordDefinitionSearchIndexDuration,
  )
where

import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Time qualified as Time
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Prometheus qualified as Prom
import Prometheus qualified as Prom
import Prometheus.Metric.GHC qualified as Prom
import Share.Env qualified as Env
import Share.Postgres qualified as PG
import Share.Postgres.Metrics.Queries qualified as Q
import Share.Prelude
import Share.Utils.Deployment qualified as Deployment
import Share.Utils.Servant.PathInfo (HasPathInfo, normalizePath)
import System.Clock (Clock (..), diffTimeSpec, toNanoSecs)
import System.Clock qualified as Clock
import UnliftIO qualified

service :: Text
service = "share-api"

deployment :: Text
deployment = tShow Deployment.deployment

metricUpdateInterval :: Time.NominalDiffTime
metricUpdateInterval = 10 * 60 -- 10 mins

-- | Low resolution metrics that are updated rarely.
data SlowChangingMetrics = SlowChangingMetrics
  { uniqueUserPushesInWeek :: Int64,
    uniqueUserPushesInMonth :: Int64,
    uniqueUserPushesAllTime :: Int64,
    numTotalUsers :: Int64,
    numPublicProjects :: Int64,
    numPrivateProjects :: Int64,
    numTotalProjects :: Int64,
    releaseDownloadsLastDay :: Int64,
    releaseDownloadsLastWeek :: Int64,
    releaseDownloadsLastMonth :: Int64,
    -- Number of users with access to cloud.
    numCloudUsers :: Int64,
    -- Number of total public definitions on /main branch of all Share projects
    numTotalPublicDefinitions :: Int64,
    -- Number of total public or private definitions on /main branch of all Share projects
    numTotalPublicOrPrivateDefinitions :: Int64,
    usersWithContributions :: Int64,
    usersWithTickets :: Int64
  }

-- | Serves the app's prometheus metrics at `/metrics`
serveMetricsMiddleware :: Env.Env x -> IO Wai.Middleware
serveMetricsMiddleware env = do
  Prom.register Prom.ghcMetrics
  getMetrics <- atMostOnceEveryInterval metricUpdateInterval $ do
    now <- Time.getCurrentTime
    runPG (queryMetrics now)
  pure \app req handleResponse -> do
    refreshGauges getMetrics
    Prom.prometheus prometheusSettings app req handleResponse
  where
    runPG = PG.runSessionWithPool (Env.pgConnectionPool env) . PG.readTransaction
    prometheusSettings =
      Prom.def
        { Prom.prometheusEndPoint = ["metrics"],
          Prom.prometheusInstrumentApp = False,
          Prom.prometheusInstrumentPrometheus = False
        }

-- | Record an event to the middleware metric.
requestMetricsMiddleware :: (HasPathInfo api) => Proxy api -> Wai.Middleware
requestMetricsMiddleware api app req handleResponse = do
  if recordRequest req
    then do
      start <- Clock.getTime Monotonic
      app req $ \resp ->
        do
          end <- Clock.getTime Monotonic
          let method = Just $ decodeUtf8 (Wai.requestMethod req)
          -- There's probably some nice way to do this with Servant.
          let path = Text.intercalate "/" <$> normalizePath api (Wai.pathInfo req)
          let status = Just $ Text.pack (show (HTTP.statusCode (Wai.responseStatus resp)))
          result <- handleResponse resp
          let latency :: Double
              latency = fromRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
          Prom.withLabel
            requestLatency
            (tShow Deployment.deployment, service, fromMaybe "" method, fromMaybe "" status, fromMaybe "unknown-path" path)
            (flip Prom.observe latency)
          pure result
    else app req handleResponse
  where
    ignoredPaths = Set.fromList [["health"], ["metrics"]]
    recordRequest req = Set.notMember (Wai.pathInfo req) ignoredPaths

{-# NOINLINE requestLatency #-}
requestLatency :: Prom.Vector Prom.Label5 Prom.Histogram
requestLatency =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service", "method", "status_code", "path") $
      Prom.histogram info Prom.defaultBuckets
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in seconds."

{-# NOINLINE userSignupsCounter #-}
userSignupsCounter :: Prom.Vector Prom.Label2 Prom.Counter
userSignupsCounter =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.counter info
  where
    info =
      Prom.Info
        "user_signups_count"
        "The total number of users registered with share"

{-# NOINLINE totalUsersGauge #-}
totalUsersGauge :: Prom.Vector Prom.Label2 Prom.Gauge
totalUsersGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "total_registered_users"
        "The number of registered users."

{-# NOINLINE publicProjectsGauge #-}
publicProjectsGauge :: Prom.Vector Prom.Label2 Prom.Gauge
publicProjectsGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "total_public_projects"
        "The number of public projects."

{-# NOINLINE privateProjectsGauge #-}
privateProjectsGauge :: Prom.Vector Prom.Label2 Prom.Gauge
privateProjectsGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "total_private_projects"
        "The number of private projects."

{-# NOINLINE totalProjectsGauge #-}
totalProjectsGauge :: Prom.Vector Prom.Label2 Prom.Gauge
totalProjectsGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "total_projects"
        "The total number of projects."

{-# NOINLINE releaseDownloadsLastDayGauge #-}
releaseDownloadsLastDayGauge :: Prom.Vector Prom.Label2 Prom.Gauge
releaseDownloadsLastDayGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "release_downloads_last_day"
        "The total number of release downloads over the last day."

{-# NOINLINE releaseDownloadsLastWeekGauge #-}
releaseDownloadsLastWeekGauge :: Prom.Vector Prom.Label2 Prom.Gauge
releaseDownloadsLastWeekGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "release_downloads_last_week"
        "The total number of release downloads over the last week."

{-# NOINLINE releaseDownloadsLastMonthGauge #-}
releaseDownloadsLastMonthGauge :: Prom.Vector Prom.Label2 Prom.Gauge
releaseDownloadsLastMonthGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "release_downloads_last_month"
        "The total number of release downloads over the last month."

{-# NOINLINE numCloudUsersGauge #-}
numCloudUsersGauge :: Prom.Vector Prom.Label2 Prom.Gauge
numCloudUsersGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "num_cloud_users"
        "The number of users with access to cloud."

{-# NOINLINE numTotalPublicOrPrivateDefinitionsGauge #-}
numTotalPublicOrPrivateDefinitionsGauge :: Prom.Vector Prom.Label2 Prom.Gauge
numTotalPublicOrPrivateDefinitionsGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "num_total_public_or_private_definitions"
        "The number of total definitions on /main branch of all Share projects (including private projects)."

{-# NOINLINE numTotalPublicDefinitionsGauge #-}
numTotalPublicDefinitionsGauge :: Prom.Vector Prom.Label2 Prom.Gauge
numTotalPublicDefinitionsGauge =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "num_total_public_definitions"
        "The number of total definitions on /main branch of all public Share projects"

numUsersWithContributions :: Prom.Vector Prom.Label2 Prom.Gauge
numUsersWithContributions =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info = Prom.Info "num_users_with_contributions" "The number of users who have interacted contributions."

numUsersWithTickets :: Prom.Vector Prom.Label2 Prom.Gauge
numUsersWithTickets =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info = Prom.Info "num_users_with_tickets" "The number of users who have interacted with tickets."

-- | Adds one to the user-signup counter
tickUserSignup :: (MonadIO m) => m ()
tickUserSignup = liftIO do
  Prom.withLabel userSignupsCounter (tShow Deployment.deployment, "share-api") Prom.incCounter

-- | Runs the provided action at most once every interval, will return previous results until
-- it's time to run again.
--
-- This is useful for metrics that are expensive to compute and don't change often.
atMostOnceEveryInterval :: Time.NominalDiffTime -> IO a -> IO (IO a)
atMostOnceEveryInterval interval action = do
  lastRunRef <- UnliftIO.newMVar Nothing
  pure do
    now <- Time.getCurrentTime
    UnliftIO.modifyMVar lastRunRef \cached -> do
      case cached of
        Just (lastRunTime, result) ->
          if now `Time.diffUTCTime` lastRunTime < interval
            then pure (Just (lastRunTime, result), result)
            else refresh
        Nothing -> refresh
  where
    refresh = do
      result <- action
      now <- Time.getCurrentTime
      pure $ (Just (now, result), result)

-- | A collection of metrics that are expensive to compute and don't change often.
queryMetrics :: Time.UTCTime -> PG.Transaction e SlowChangingMetrics
queryMetrics now = do
  uniqueUserPushesInWeek <- Q.numUniqueUsersPushedSince (Time.addUTCTime ((-7) * Time.nominalDay) now)
  uniqueUserPushesInMonth <- Q.numUniqueUsersPushedSince (Time.addUTCTime ((-30) * Time.nominalDay) now)
  uniqueUserPushesAllTime <- Q.numUniqueUsersWithAPush
  numTotalUsers <- Q.allUsersCount
  (numPrivateProjects, numPublicProjects, numTotalProjects) <- Q.allProjectsCount
  (releaseDownloadsLastDay, releaseDownloadsLastWeek, releaseDownloadsLastMonth) <- Q.releaseDownloadsGauges
  numCloudUsers <- Q.numCloudUsers
  numTotalPublicOrPrivateDefinitions <- Q.numTotalPublicOrPrivateDefinitions
  numTotalPublicDefinitions <- Q.numTotalPublicDefinitions
  usersWithContributions <- Q.usersInteractedWithContributions
  usersWithTickets <- Q.usersInteractedWithTickets
  pure SlowChangingMetrics {..}

-- | Since some time-based metrics will change due to the passage of time rather than any
-- specific user action, we just refresh them whenever prometheus queries metrics for them.
-- Ensure that any queries here aren't too expensive since they'll be run often.
refreshGauges :: IO SlowChangingMetrics -> IO ()
refreshGauges getMetrics = do
  SlowChangingMetrics {..} <- getMetrics
  Prom.withLabel numUniqueAccountsWithAPushInLastWeek (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral uniqueUserPushesInWeek)
  Prom.withLabel numUniqueAccountsWithAPushInLastMonth (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral uniqueUserPushesInMonth)
  Prom.withLabel numUniqueAccountsWithAtLeastOnePush (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral uniqueUserPushesAllTime)
  Prom.withLabel totalUsersGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numTotalUsers)

  Prom.withLabel privateProjectsGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numPrivateProjects)
  Prom.withLabel publicProjectsGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numPublicProjects)
  Prom.withLabel totalProjectsGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numTotalProjects)

  Prom.withLabel releaseDownloadsLastDayGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral releaseDownloadsLastDay)
  Prom.withLabel releaseDownloadsLastWeekGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral releaseDownloadsLastWeek)
  Prom.withLabel releaseDownloadsLastMonthGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral releaseDownloadsLastMonth)

  Prom.withLabel numCloudUsersGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numCloudUsers)
  Prom.withLabel numTotalPublicOrPrivateDefinitionsGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numTotalPublicOrPrivateDefinitions)
  Prom.withLabel numTotalPublicDefinitionsGauge (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral numTotalPublicDefinitions)

  Prom.withLabel numUsersWithContributions (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral usersWithContributions)
  Prom.withLabel numUsersWithTickets (deployment, service) \gauge -> Prom.setGauge gauge (fromIntegral usersWithTickets)

{-# NOINLINE numUniqueAccountsWithAPushInLastWeek #-}
numUniqueAccountsWithAPushInLastWeek :: Prom.Vector Prom.Label2 Prom.Gauge
numUniqueAccountsWithAPushInLastWeek =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "unique_users_with_push_in_last_week_count"
        "The number of unique users with at least one push in the last week."

{-# NOINLINE numUniqueAccountsWithAPushInLastMonth #-}
numUniqueAccountsWithAPushInLastMonth :: Prom.Vector Prom.Label2 Prom.Gauge
numUniqueAccountsWithAPushInLastMonth =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "unique_users_with_push_in_last_month_count"
        "The number of unique users with at least one push in the last 30 days."

{-# NOINLINE numUniqueAccountsWithAtLeastOnePush #-}
numUniqueAccountsWithAtLeastOnePush :: Prom.Vector Prom.Label2 Prom.Gauge
numUniqueAccountsWithAtLeastOnePush =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.gauge info
  where
    info =
      Prom.Info
        "unique_users_with_at_least_one_push_count"
        "The number of unique users with at least one push over all time."

{-# NOINLINE backgroundImportDurationSeconds #-}
backgroundImportDurationSeconds :: Prom.Vector Prom.Label2 Prom.Histogram
backgroundImportDurationSeconds =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.histogram info Prom.defaultBuckets
  where
    info =
      Prom.Info
        "background_codebase_import_duration_seconds"
        "The time it took to import a pulled branch into the user's codebase."

{-# NOINLINE definitionSearchIndexDurationSeconds #-}
definitionSearchIndexDurationSeconds :: Prom.Vector Prom.Label2 Prom.Histogram
definitionSearchIndexDurationSeconds =
  Prom.unsafeRegister $
    Prom.vector ("deployment", "service") $
      Prom.histogram info Prom.defaultBuckets
  where
    info =
      Prom.Info
        "definition_search_indexing_duration_seconds"
        "The time it took to index a release for definition search"

timeActionIntoHistogram :: (Prom.Label l, MonadUnliftIO m) => (Prom.Vector l Prom.Histogram) -> l -> m c -> m c
timeActionIntoHistogram histogram l m = do
  UnliftIO.bracket start end \_ -> m
  where
    start = UnliftIO.liftIO $ Clock.getTime Monotonic
    end startTime = UnliftIO.liftIO $ do
      end <- Clock.getTime Monotonic
      let latency :: Double
          latency = fromRational (toNanoSecs (end `diffTimeSpec` startTime) % 1000000000)
      Prom.withLabel histogram l (flip Prom.observe latency)

-- | Record the duration of a background import.
recordBackgroundImportDuration :: (MonadUnliftIO m) => m r -> m r
recordBackgroundImportDuration = timeActionIntoHistogram backgroundImportDurationSeconds (deployment, service)

-- | Record the duration of a background import.
recordDefinitionSearchIndexDuration :: (MonadUnliftIO m) => m r -> m r
recordDefinitionSearchIndexDuration = timeActionIntoHistogram definitionSearchIndexDurationSeconds (deployment, service)
