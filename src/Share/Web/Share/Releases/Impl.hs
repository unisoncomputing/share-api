{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Share.Web.Share.Releases.Impl
  ( releasesServer,
    getProjectReleaseReadmeEndpoint,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Servant
import Share.Codebase qualified as Codebase
import Share.IDs
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Hashes.Queries qualified as CausalQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.Queries qualified as Q
import Share.Prelude
import Share.Project (Project (..))
import Share.Release (Release (..), releaseCausals_, releaseUsers_)
import Share.User (User (..))
import Share.User qualified as User
import Share.Utils.API
import Share.Utils.Caching
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.CodeBrowsing.API qualified as API
import Share.Web.Share.Releases.API
import Share.Web.Share.Releases.API qualified as API
import Share.Web.Share.Releases.Types
import Share.Web.Share.Releases.Types qualified as API
import Share.Web.Share.Types
import Share.Web.UCM.Sync.Impl qualified as SyncQ
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Share.DefinitionSummary (serveTermSummary, serveTypeSummary)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary, TypeSummary)
import Unison.Server.Share.Definitions qualified as ShareBackend
import Unison.Server.Share.FuzzyFind qualified as Fuzzy
import Unison.Server.Share.NamespaceDetails qualified as ND
import Unison.Server.Share.NamespaceListing (NamespaceListing (..))
import Unison.Server.Share.NamespaceListing qualified as NL
import Unison.Server.Share.RenderDoc (findAndRenderDoc)
import Unison.Server.Types (DefinitionDisplayResults, NamespaceDetails (..), Suffixify (..))
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty qualified as Pretty

releasesServer :: Maybe Session -> UserHandle -> ProjectSlug -> ServerT API.ProjectReleasesAPI WebApp
releasesServer session handle projectSlug =
  listReleasesByProjectEndpoint session handle projectSlug
    :<|> createRelease session handle projectSlug
    :<|> ( \releaseVersion ->
             hoistServer (Proxy @API.ProjectReleaseResourceAPI) (addTags releaseVersion) $
               ( getProjectReleaseEndpoint session handle projectSlug releaseVersion
                   :<|> getProjectReleaseReadmeEndpoint session handle projectSlug releaseVersion
                   :<|> getProjectReleaseNotesEndpoint session handle projectSlug releaseVersion
                   :<|> updateReleaseEndpoint session handle projectSlug releaseVersion
                   :<|> releaseCodeBrowsingServer session handle projectSlug releaseVersion
               )
         )
  where
    addTags releaseVersion m = do
      addRequestTag "release-version" (IDs.toText releaseVersion)
      m

releaseCodeBrowsingServer :: Maybe Session -> UserHandle -> ProjectSlug -> ReleaseVersion -> ServerT API.CodeBrowseAPI WebApp
releaseCodeBrowsingServer session handle projectSlug releaseVersion =
  ( projectReleaseBrowseEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseDefinitionsByNameEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseDefinitionsByHashEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseTermSummaryEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseTypeSummaryEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseFindEndpoint session handle projectSlug releaseVersion
      :<|> projectReleaseNamespacesByNameEndpoint session handle projectSlug releaseVersion
  )

getProjectRelease ::
  ProjectReleaseShortHand ->
  WebApp (Project, Release CausalId UserId)
getProjectRelease projectReleaseShortHand = do
  addRequestTag "release" (IDs.toText projectReleaseShortHand)
  onNothingM missingRelease . PG.runTransaction . runMaybeT $ do
    release@Release {projectId} <- MaybeT $ Q.releaseByProjectReleaseShortHand projectReleaseShortHand
    project <- lift $ Q.expectProjectById projectId
    pure (project, release)
  where
    missingRelease = respondError (EntityMissing (ErrorID "missing-project-release") "Release could not be found")

projectReleaseBrowseEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Maybe Path.Path ->
  Maybe Path.Path ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceListing)
projectReleaseBrowseEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion relativeTo namespace rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-browse" cacheParams releaseHead $ do
    Codebase.runCodebaseTransactionOrRespondError codebase $ do
      NL.serve releaseHead relativeTo namespace `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "missing-namespace") "Namespace could not be found")
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

    cacheParams = [IDs.toText projectReleaseShortHand, tShow $ fromMaybe mempty relativeTo, tShow $ fromMaybe mempty namespace]

projectReleaseDefinitionsByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  HQ.HashQualified Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
projectReleaseDefinitionsByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion name relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-definitions-by-name" cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      ShareBackend.definitionForHQName (fromMaybe mempty relativeTo) releaseHead renderWidth (Suffixify False) rt name
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
    cacheParams = [IDs.toText projectReleaseShortHand, HQ.toTextWith Name.toText name, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectReleaseDefinitionsByHashEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Referent ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
projectReleaseDefinitionsByHashEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion referent relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-definitions-by-hash" cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      ShareBackend.definitionForHQName (fromMaybe mempty relativeTo) releaseHead renderWidth (Suffixify False) rt query
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
    cacheParams = [IDs.toText projectReleaseShortHand, toUrlPiece referent, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectReleaseTermSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Referent ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TermSummary)
projectReleaseTermSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion ref mayName relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-term-summary" cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      serveTermSummary ref mayName releaseHead relativeTo renderWidth
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
    cacheParams = [IDs.toText projectReleaseShortHand, toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectReleaseTypeSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Reference ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TypeSummary)
projectReleaseTypeSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion ref mayName relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-type-summary" cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      serveTypeSummary ref mayName renderWidth
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
    cacheParams = [IDs.toText projectReleaseShortHand, toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectReleaseFindEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Maybe Path.Path ->
  Maybe Int ->
  Maybe Pretty.Width ->
  Text ->
  Bool ->
  Maybe CausalHash ->
  WebApp [(Fuzzy.Alignment, Fuzzy.FoundResult)]
projectReleaseFindEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion mayRelativeTo limit renderWidth query searchDependencies rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  let relativeTo = fromMaybe mempty mayRelativeTo
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  Codebase.runCodebaseTransaction codebase $ do
    Fuzzy.serveFuzzyFind isInScratch searchDependencies releaseHead relativeTo limit renderWidth query
  where
    isInScratch = False
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

projectReleaseNamespacesByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceDetails)
projectReleaseNamespacesByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion path renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported for releases")
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-release-namespaces-by-name" cacheParams releaseHead $ do
    Codebase.runCodebaseTransactionOrRespondError codebase $ do
      ND.namespaceDetails rt (fromMaybe mempty path) releaseHead renderWidth `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "missing-namespace") "Namespace could not be found")
  where
    cacheParams = [IDs.toText projectReleaseShortHand, tShow path, foldMap (toUrlPiece . Pretty.widthToInt) renderWidth]
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

getProjectReleaseEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  WebApp APIRelease
getProjectReleaseEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion = do
  addRequestTag "release" (IDs.toText projectReleaseShortHand)
  (project, release) <- PG.runTransactionOrRespondError $ do
    release@Release {projectId} <-
      Q.releaseByProjectReleaseShortHand projectReleaseShortHand
        `whenNothingM` throwError (EntityMissing (ErrorID "missing-project-release") "Release could not be found")
    releaseWithHandle <- forOf releaseUsers_ release \uid -> do
      User.handle <$> Q.userByUserId uid `whenNothingM` throwError (EntityMissing (ErrorID "missing-user") "User could not be found")
    project <- Q.expectProjectById projectId
    releaseWithCausalHashes <- CausalQ.expectCausalHashesByIdsOf releaseCausals_ releaseWithHandle
    pure (project, releaseWithCausalHashes)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReleaseGet callerUserId project release
  pure $ API.releaseToAPIRelease (ProjectShortHand {userHandle, projectSlug}) release
  where
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

-- | Gets the readme for a project release.
getProjectReleaseReadmeEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  WebApp (Cached JSON ReadmeResponse)
getProjectReleaseReadmeEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion = do
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let rootPath = mempty
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "get-project-release-doc" cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      mayNamespaceDetails <- ND.namespaceDetails rt rootPath releaseHead Nothing
      let mayReadme = do
            NamespaceDetails {readme} <- mayNamespaceDetails
            readme
      pure $ ReadmeResponse {readMe = mayReadme}
  where
    cacheParams = [IDs.toText projectReleaseShortHand]
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

getProjectReleaseNotesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  WebApp (Cached JSON DocResponse)
getProjectReleaseNotesEndpoint caller userHandle projectSlug releaseVersion = do
  getProjectReleaseDocEndpoint "get-project-release-notes" releaseNotesNames caller userHandle projectSlug releaseVersion
  where
    releaseNotesNames = Set.fromList $ NameSegment <$> ["release_notes", "ReleaseNotes", "releaseNotes", "RELEASE_NOTES"]

getProjectReleaseDocEndpoint ::
  Text ->
  Set NameSegment ->
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  ReleaseVersion ->
  WebApp (Cached JSON DocResponse)
getProjectReleaseDocEndpoint cacheKey docNames (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug releaseVersion = do
  (project@Project {ownerUserId = projectOwnerUserId}, Release {squashedCausal = releaseHead}) <- getProjectRelease projectReleaseShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectReleaseRead callerUserId project
  let rootPath = mempty
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc cacheKey cacheParams releaseHead $ do
    Codebase.runCodebaseTransaction codebase $ do
      doc <- findAndRenderDoc docNames rt rootPath releaseHead Nothing
      pure $ DocResponse {doc}
  where
    cacheParams = [IDs.toText projectReleaseShortHand]
    projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}

listReleasesByProjectEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  Maybe (Cursor ListReleasesCursor) ->
  Maybe Limit ->
  Maybe Query ->
  Maybe API.ReleaseStatusFilter ->
  WebApp (Paged ListReleasesCursor APIRelease)
listReleasesByProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug mayCursor mayLimit mayVersionPrefix mayStatusFilter = do
  let projectShortHand = ProjectShortHand {userHandle, projectSlug}
  (project@Project {projectId}) <- PG.runTransactionOrRespondError do
    (Q.projectByShortHand projectShortHand) `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @ProjectShortHand projectShortHand))
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkListReleasesForProject callerUserId project
  releases <- PG.runTransaction do
    Q.listReleasesByProject limit mayCursor mayVersionPrefix (fromMaybe defaultStatusFilter mayStatusFilter) projectId
  shareReleases <-
    releases
      & PG.runTransaction . CausalQ.expectCausalHashesByIdsOf (traversed . releaseCausals_)
      <&> fmap
        ( \release ->
            API.releaseToAPIRelease projectShortHand release
        )
  pure $ Paged {items = shareReleases, cursor = nextCursor releases}
  where
    nextCursor :: [Release causal userId] -> Maybe (Cursor ListReleasesCursor)
    nextCursor releases = case releases of
      [] -> Nothing
      releases@(x : xs)
        | length releases < fromIntegral (getLimit limit) -> Nothing
        | otherwise ->
            let Release {version = ReleaseVersion {major, minor, patch}, releaseId} = NonEmpty.last (x :| xs)
             in Just $ Cursor (major, minor, patch, releaseId)

    limit = fromMaybe defaultLimit mayLimit
    defaultLimit = Limit 20
    defaultStatusFilter = AllReleases

-- | Update a release, a release can be updated draft -> published, published -> deprecated or draft -> deprecated.
-- If publishing a release, we also squash the causal for that release, add it to the codebase
-- and add a name-lookup for it.
updateReleaseEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> ReleaseVersion -> UpdateReleaseRequest -> WebApp ()
updateReleaseEndpoint session userHandle projectSlug releaseVersion UpdateReleaseRequest {status = newStatus} = do
  let projectReleaseShortHand = ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion}
  (Project {projectId}, Release {releaseId}) <- getProjectRelease projectReleaseShortHand
  addRequestTag "project-id" (IDs.toText projectId)
  addRequestTag "release-version" (IDs.toText releaseVersion)
  callerUserId <- AuthN.requireAuthenticatedUser session
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReleaseUpdate (Just callerUserId) projectId
  PG.runTransactionOrRespondError do
    Q.updateRelease callerUserId releaseId newStatus >>= \case
      Q.UpdateRelease'Success -> pure ()
      Q.UpdateRelease'NotFound -> throwSomeServerError releaseNotFound
      Q.UpdateRelease'CantPublishDeprecated -> throwSomeServerError cantPublishDeprecated
  where
    releaseNotFound = EntityMissing (ErrorID "missing-release") "Release could not be found"
    cantPublishDeprecated = BadRequest "Cannot publish a release which has already been deprecated"

createRelease :: Maybe Session -> UserHandle -> ProjectSlug -> CreateReleaseRequest -> WebApp APIRelease
createRelease session userHandle projectSlug CreateReleaseRequest {releaseVersion, causalHash = PrefixedHash unsquashedCausalHash} = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  project@Project {projectId, ownerUserId} <-
    (`or404` (EntityMissing (ErrorID "project:missing") "Project not found"))
      . PG.runTransaction
      $ Q.projectByShortHand (ProjectShortHand userHandle projectSlug)
  addRequestTag "release-name" (IDs.toText (ReleaseShortHand releaseVersion))
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease ownerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReleaseCreate callerUserId project
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  -- Mitigation for the sync bug where sometimes causals get stuck in temp.
  SyncQ.ensureCausalIsFlushed codebase unsquashedCausalHash
  (nlReceipt, squashedCausalId, unsquashedCausalId) <- Codebase.runCodebaseTransactionOrRespondError codebase $ do
    unsquashedCausalId <- Codebase.expectCausalIdByHash unsquashedCausalHash
    unsquashedBranchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id unsquashedCausalId
    nlReceipt <- NLOps.ensureNameLookupForBranchId unsquashedBranchHashId
    squashedCausalId <-
      ( Codebase.squashCausalAndAddToCodebase unsquashedCausalId
          `whenNothingM` do
            throwSomeServerError (InternalServerError @Text "squash-failed" "Failed to squash causal on release publish")
        )
    pure (nlReceipt, squashedCausalId, unsquashedCausalId)
  -- Separate transaction to ensure squashing and name lookups make progress even on failure.
  Codebase.runCodebaseTransactionOrRespondError codebase $ do
    release <- Q.createRelease nlReceipt projectId releaseVersion squashedCausalId unsquashedCausalId callerUserId
    releaseWithHandles <- forOf releaseUsers_ release \userId -> (fmap User.handle <$> Q.userByUserId userId) `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "Project owner not found")
    releaseWithCausalHashes <- CausalQ.expectCausalHashesByIdsOf releaseCausals_ releaseWithHandles
    User {handle = projectOwnerHandle} <- (Q.userByUserId ownerUserId) `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "Project owner not found")
    let projectShortHand =
          ProjectShortHand
            { userHandle = projectOwnerHandle,
              projectSlug = projectSlug
            }
    pure (releaseToAPIRelease projectShortHand releaseWithCausalHashes)
