module Share.BackgroundJobs.Search.DefinitionSync (worker) where

import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync.Types (DefinitionDocument (..), TermOrTypeSummary (..))
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase (CodebaseM)
import Share.Codebase qualified as Codebase
import Share.IDs (ProjectShortHand (ProjectShortHand), ReleaseId, ReleaseVersion)
import Share.Postgres (QueryM (transactionUnsafeIO))
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.Queries qualified as PG
import Share.Postgres.Search.DefinitionSync qualified as DefnSyncQ
import Share.Prelude
import Share.Project (Project (..), ProjectVisibility (..))
import Share.Release (Release (..))
import Share.User (User (..), UserVisibility (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Server.Share.DefinitionSummary qualified as Summary
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

-- | How often to poll for new releases to sync in seconds.
pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

-- | How many definitions to hold in memory at a time while syncing
defnBatchSize :: Int32
defnBatchSize = 10

worker :: Ki.Scope -> Background ()
worker scope = newWorker scope "search:defn-sync" $ forever do
  liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000
  Logging.logInfoText "Syncing definitions..."
  authZReceipt <- AuthZ.backgroundJobAuthZ
  toIO <- UnliftIO.askRunInIO
  let syncDD = transactionUnsafeIO . toIO . syncDefinitionToCloud
  PG.runTransaction $ do
    mayReleaseId <- DefnSyncQ.claimUnsyncedRelease
    for_ mayReleaseId (syncRelease authZReceipt syncDD)

syncRelease :: AuthZ.AuthZReceipt -> (DefinitionDocument -> CodebaseM e ()) -> ReleaseId -> PG.Transaction e ()
syncRelease authZReceipt syncDD releaseId = fmap (fromMaybe ()) . runMaybeT $ do
  Release {projectId, squashedCausal, version = releaseVersion} <- lift $ PG.expectReleaseById releaseId
  Project {slug, ownerUserId, visibility = projectVis} <- lift $ PG.expectProjectById projectId
  User {handle, visibility = userVis} <- PG.expectUserByUserId ownerUserId
  -- Don't sync private projects
  guard $ projectVis == ProjectPublic
  -- Don't sync private users
  guard $ userVis == UserPublic
  lift $ do
    bhId <- HashQ.expectNamespaceIdsByCausalIdsOf id squashedCausal
    nlReceipt <- NLOps.ensureNameLookupForBranchId bhId
    let codebaseLoc = Codebase.codebaseLocationForProjectRelease ownerUserId
    let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
    Codebase.codebaseMToTransaction codebase $ do
      termsCursor <- lift $ NLOps.termsWithinNamespace nlReceipt bhId
      let projectShortHand = ProjectShortHand handle slug
      syncTerms syncDD bhId projectShortHand releaseVersion termsCursor
      typesCursor <- lift $ NLOps.typesWithinNamespace nlReceipt bhId
      syncTypes syncDD projectShortHand releaseVersion typesCursor

syncTerms ::
  (DefinitionDocument -> CodebaseM e ()) ->
  BranchHashId ->
  ProjectShortHand ->
  ReleaseVersion ->
  Cursors.PGCursor (Name, Referent) ->
  CodebaseM e ()
syncTerms syncDD bhId projectShortHand releaseVersion termsCursor =
  Cursors.fetchN defnBatchSize termsCursor >>= \case
    Nothing -> pure ()
    Just terms -> do
      for terms \(fqn, ref) -> do
        let sh = Referent.toShortHash ref
        sig <- Codebase.loadTypeOfReferent ref
        termSummary <- Summary.termSummaryForReferent ref sig (Just fqn) bhId Nothing Nothing
        let dd =
              DefinitionDocument
                { projectShortHand,
                  releaseVersion,
                  fqn,
                  hash = sh,
                  tokens,
                  payload = TermSummary termSummary
                }
        syncDD dd
      pure ()

syncTypes = undefined

syncDefinitionToCloud :: DefinitionDocument -> Background ()
syncDefinitionToCloud dd = do
  _
