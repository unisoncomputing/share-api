module Share.BackgroundJobs.Search.DefinitionSync (worker) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Workers (newWorker)
import Share.IDs (ReleaseId)
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.NameLookups.Ops (ensureNameLookupForBranchId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.Queries qualified as PG
import Share.Postgres.Search.DefinitionSync qualified as DefnSyncQ
import Share.Prelude
import Share.Project (Project (..), ProjectVisibility (..))
import Share.Release (Release (..))
import Share.User (User (..), UserVisibility (..))
import Share.Utils.Logging qualified as Logging
import UnliftIO.Concurrent qualified as UnliftIO

-- | How often to poll for new releases to sync in seconds.
pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

worker :: Ki.Scope -> Background ()
worker scope = newWorker scope "search:defn-sync" $ forever do
  liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000
  Logging.logInfoText "Syncing definitions..."
  PG.runTransaction $ do
    mayReleaseId <- DefnSyncQ.claimUnsyncedRelease
    for_ mayReleaseId syncRelease

syncRelease :: ReleaseId -> PG.Transaction e ()
syncRelease releaseId = fmap (fromMaybe ()) . runMaybeT $ do
  Release {projectId, squashedCausal} <- lift $ PG.expectReleaseById releaseId
  Project {slug, ownerUserId, visibility = projectVis} <- lift $ PG.expectProjectById projectId
  User {handle, visibility = userVis} <- PG.expectUserByUserId ownerUserId
  -- Don't sync private projects
  guard $ projectVis == ProjectPublic
  -- Don't sync private users
  guard $ userVis == UserPublic
  bhId <- HashQ.expectNamespaceIdsByCausalIdsOf id squashedCausal
  NLOps.ensureNameLookupForBranchId bhId
  _
