{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Share.Web.Share.Projects.Impl where

import Control.Lens
import Control.Monad.Except
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant
import Share.Branch (defaultBranchShorthand)
import Share.Branch qualified as Branch
import Share.Codebase (CodebaseEnv)
import Share.Codebase qualified as Codebase
import Share.IDs (PrefixedHash (..), ProjectSlug (..), UserHandle, UserId)
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Projects.Queries qualified as ProjectsQ
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Project (Project (..))
import Share.Release qualified as Release
import Share.User (User (..))
import Share.Utils.API ((:++) (..))
import Share.Utils.Caching (Cached)
import Share.Utils.Caching qualified as Caching
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.Branches.Impl (branchesServer, getProjectBranchReadmeEndpoint)
import Share.Web.Share.Contributions.Impl (contributionsByProjectServer)
import Share.Web.Share.Diffs.Impl qualified as Diffs
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse (..))
import Share.Web.Share.Projects.API qualified as API
import Share.Web.Share.Projects.Types
import Share.Web.Share.Releases.Impl (getProjectReleaseReadmeEndpoint, releasesServer)
import Share.Web.Share.Tickets.Impl (ticketsByProjectServer)
import Share.Web.Share.Types
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.NameSearch.Postgres qualified as PGNameSearch
import Unison.Server.Orphans ()
import Unison.Server.Share.Definitions qualified as Definitions
import Unison.Server.Types (TermDefinition (..), TypeDefinition (..))
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)

data ProjectErrors
  = MaintainersAlreadyExist [UserId]
  | MaintainersMissing [UserId]

instance ToServerError ProjectErrors where
  toServerError = \case
    (MaintainersAlreadyExist userIds) ->
      ( ErrorID "projects:maintainers-already-exist",
        Servant.err409
          { errBody = BL.fromStrict . Text.encodeUtf8 $ "Maintainers already exist: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
          }
      )
    (MaintainersMissing userIds) ->
      ( ErrorID "projects:maintainers-missing",
        Servant.err404
          { errBody = BL.fromStrict . Text.encodeUtf8 $ "Maintainers missing: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
          }
      )

instance Logging.Loggable ProjectErrors where
  toLog = \case
    (MaintainersAlreadyExist userIds) ->
      Logging.textLog
        ( "Maintainers already exist: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
        )
        & Logging.withSeverity Logging.UserFault
    (MaintainersMissing userIds) ->
      Logging.textLog
        ( "Maintainers missing: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
        )
        & Logging.withSeverity Logging.UserFault

projectServer :: Maybe Session -> UserHandle -> ServerT API.ProjectsAPI WebApp
projectServer session handle =
  let maintainersResourceServer slug =
        listMaintainersEndpoint session handle slug
          :<|> addMaintainersEndpoint session handle slug
          :<|> updateMaintainersEndpoint session handle slug
   in listProjectsForUserEndpoint session handle
        :<|> ( \slug ->
                 hoistServer (Proxy @API.ProjectResourceAPI) (addTags slug) $
                   projectReadmeEndpoint session handle slug
                     :<|> branchesServer session handle slug
                     :<|> releasesServer session handle slug
                     :<|> contributionsByProjectServer session handle slug
                     :<|> ticketsByProjectServer session handle slug
                     :<|> ( diffNamespacesEndpoint session handle slug
                              :<|> diffTermsEndpoint session handle slug
                              :<|> diffTypesEndpoint session handle slug
                          )
                     :<|> createProjectEndpoint session handle slug
                     :<|> updateProjectEndpoint session handle slug
                     :<|> deleteProjectEndpoint session handle slug
                     :<|> getProjectEndpoint session handle slug
                     :<|> favProjectEndpoint session handle slug
                     :<|> maintainersResourceServer slug
             )
  where
    addTags :: forall x. ProjectSlug -> WebApp x -> WebApp x
    addTags slug m = do
      addRequestTag "project-slug" (IDs.toText slug)
      m

catalogServer :: ServerT API.CatalogAPI WebApp
catalogServer = projectCatalogEndpoint

projectCatalogEndpoint :: Maybe Session -> WebApp [CatalogCategory]
projectCatalogEndpoint (AuthN.MaybeAuthedUserID callerUserId) = do
  projectMap <- PG.runTransaction $ Q.listProjectsFromCatalogWithMetadata callerUserId
  projectMap
    & Map.toList
    & ( fmap \(category, projectsData) ->
          CatalogCategory
            { name = category,
              projects =
                projectsData <&> \(p, favData, projectOwner) ->
                  projectToAPI projectOwner p :++ favData
            }
      )
    & pure

listProjectsForUserEndpoint :: Maybe Session -> UserHandle -> WebApp ListProjectsResponse
listProjectsForUserEndpoint (AuthN.MaybeAuthedUserID callerUserId) handle = do
  User {user_id = targetUserId} <- PGO.expectUserByHandle handle
  -- Note: This query already filters for only things the user has access to.
  projectData <- PG.runTransaction $ Q.listProjectsByUserWithMetadata callerUserId targetUserId
  let projects = projectData <&> \(proj, favData, projectOwner) -> projectToAPI projectOwner proj :++ favData
  pure $ ListProjectsResponse {projects}

diffNamespacesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.BranchOrReleaseShortHand ->
  IDs.BranchOrReleaseShortHand ->
  WebApp (Cached JSON ShareNamespaceDiffResponse)
diffNamespacesEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug oldShortHand newShortHand = do
  project@Project {projectId} <- PG.runTransactionOrRespondError do
    Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @IDs.ProjectShortHand projectShortHand))
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId project
  (_, oldCausalId, _oldBranchId) <- namespaceHashForBranchOrRelease authZReceipt project oldShortHand
  (_, newCausalId, _newBranchId) <- namespaceHashForBranchOrRelease authZReceipt project newShortHand

  let cacheKeys = [IDs.toText projectId, IDs.toText oldShortHand, IDs.toText newShortHand, Caching.causalIdCacheKey oldCausalId, Caching.causalIdCacheKey newCausalId]
  Caching.cachedResponse authZReceipt "project-diff-namespaces" cacheKeys do
    (ancestorCausalId, ancestorCausalHash, newCausalHash) <- PG.runTransaction $ do
      ancestorCausalId <- fromMaybe oldCausalId <$> CausalQ.bestCommonAncestor oldCausalId newCausalId
      (ancestorCausalHash, newCausalHash) <- CausalQ.expectCausalHashesByIdsOf both (ancestorCausalId, newCausalId)
      pure (ancestorCausalId, ancestorCausalHash, newCausalHash)
    namespaceDiff <- Diffs.diffCausals authZReceipt ancestorCausalId newCausalId
    pure $
      ShareNamespaceDiffResponse
        { project = projectShortHand,
          oldRef = oldShortHand,
          oldRefHash = Just $ PrefixedHash ancestorCausalHash,
          newRef = newShortHand,
          newRefHash = Just $ PrefixedHash newCausalHash,
          diff = namespaceDiff
        }
  where
    projectShortHand = IDs.ProjectShortHand {userHandle, projectSlug}

diffTermsEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.BranchOrReleaseShortHand ->
  IDs.BranchOrReleaseShortHand ->
  Name ->
  Name ->
  WebApp ShareTermDiffResponse
diffTermsEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug oldShortHand newShortHand oldTermName newTermName =
  do
    project <- PG.runTransactionOrRespondError do
      Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @IDs.ProjectShortHand projectShortHand))
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId project
    oldTerm@(TermDefinition {termDefinition = oldDisplayObj}) <- getTermDefinition authZReceipt project oldShortHand oldTermName `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("Term not found: " <> IDs.toText oldShortHand <> ":" <> Name.toText oldTermName))
    newTerm@(TermDefinition {termDefinition = newDisplayObj}) <- getTermDefinition authZReceipt project newShortHand newTermName `whenNothingM` respondError (EntityMissing (ErrorID "term-not-found") ("Term not found: " <> IDs.toText newShortHand <> ":" <> Name.toText newTermName))
    let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects oldDisplayObj newDisplayObj
    pure $
      ShareTermDiffResponse
        { project = projectShortHand,
          oldBranch = oldShortHand,
          newBranch = newShortHand,
          oldTerm = oldTerm,
          newTerm = newTerm,
          diff = termDiffDisplayObject
        }
  where
    renderWidth :: Width
    renderWidth = 80
    getTermDefinition :: AuthZ.AuthZReceipt -> Project -> IDs.BranchOrReleaseShortHand -> Name -> WebApp (Maybe TermDefinition)
    getTermDefinition authZReceipt project shorthand name = do
      (codebase, _causalId, bhId) <- namespaceHashForBranchOrRelease authZReceipt project shorthand
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransaction $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransaction codebase do
        Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName

    projectShortHand :: IDs.ProjectShortHand
    projectShortHand = IDs.ProjectShortHand {userHandle, projectSlug}

diffTypesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.BranchOrReleaseShortHand ->
  IDs.BranchOrReleaseShortHand ->
  Name ->
  Name ->
  WebApp ShareTypeDiffResponse
diffTypesEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug oldShortHand newShortHand oldTypeName newTypeName =
  do
    project <- PG.runTransactionOrRespondError do
      Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @IDs.ProjectShortHand projectShortHand))
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId project
    sourceType@(TypeDefinition {typeDefinition = sourceDisplayObj}) <- getTypeDefinition authZReceipt project oldShortHand oldTypeName `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("Type not found: " <> IDs.toText oldShortHand <> ":" <> Name.toText oldTypeName))
    newType@(TypeDefinition {typeDefinition = newDisplayObj}) <- getTypeDefinition authZReceipt project newShortHand newTypeName `whenNothingM` respondError (EntityMissing (ErrorID "type-not-found") ("Type not found: " <> IDs.toText newShortHand <> ":" <> Name.toText newTypeName))
    let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects sourceDisplayObj newDisplayObj
    pure $
      ShareTypeDiffResponse
        { project = projectShortHand,
          oldBranch = oldShortHand,
          newBranch = newShortHand,
          oldType = sourceType,
          newType = newType,
          diff = typeDiffDisplayObject
        }
  where
    renderWidth :: Width
    renderWidth = 80
    getTypeDefinition :: AuthZ.AuthZReceipt -> Project -> IDs.BranchOrReleaseShortHand -> Name -> WebApp (Maybe TypeDefinition)
    getTypeDefinition authZReceipt project shorthand name = do
      (codebase, _causalId, bhId) <- namespaceHashForBranchOrRelease authZReceipt project shorthand
      let perspective = Path.empty
      (namesPerspective, Identity relocatedName) <- PG.runTransaction $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
      let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
      rt <- Codebase.codebaseRuntime codebase
      Codebase.runCodebaseTransaction codebase do
        Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName

    projectShortHand :: IDs.ProjectShortHand
    projectShortHand = IDs.ProjectShortHand {userHandle, projectSlug}

namespaceHashForBranchOrRelease :: AuthZ.AuthZReceipt -> Project -> IDs.BranchOrReleaseShortHand -> WebApp (CodebaseEnv, CausalId, BranchHashId)
namespaceHashForBranchOrRelease authZReceipt Project {projectId, ownerUserId = projectOwnerUserId} = \case
  IDs.IsBranchShortHand branchShortHand -> do
    PG.runTransactionOrRespondError $ do
      branch <- Q.branchByProjectIdAndShortHand projectId branchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch-not-found") ("Branch not found: " <> IDs.toText @IDs.BranchShortHand branchShortHand))
      let causalId = Branch.causal branch
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId (Branch.contributorId branch)
      let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
      Codebase.codebaseMToTransaction codebase do
        branchHashId <- CausalQ.expectNamespaceIdForCausal causalId
        pure (codebase, causalId, branchHashId)
  IDs.IsReleaseShortHand releaseShortHand -> do
    PG.runTransactionOrRespondError $ do
      release <- Q.releaseByProjectIdAndReleaseShortHand projectId releaseShortHand `whenNothingM` throwError (EntityMissing (ErrorID "release-not-found") ("Release not found: " <> IDs.toText @IDs.ReleaseShortHand releaseShortHand))
      let causalId = Release.squashedCausal release
      let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
      let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
      Codebase.codebaseMToTransaction codebase do
        branchHashId <- CausalQ.expectNamespaceIdForCausal causalId
        pure (codebase, causalId, branchHashId)

createProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> CreateProjectRequest -> WebApp CreateProjectResponse
createProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug req = do
  User {user_id = targetUserId} <- PGO.expectUserByHandle userHandle
  AuthZ.permissionGuard $ AuthZ.checkProjectCreate callerUserId targetUserId
  let CreateProjectRequest {summary, tags, visibility} = req
  projectId <- PGO.createProject targetUserId projectSlug summary tags visibility
  addRequestTag "project-id" (IDs.toText projectId)
  pure CreateProjectResponse

updateProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> UpdateProjectRequest -> WebApp ()
updateProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug req = do
  projectId <- PG.runTransaction (Q.projectIDFromHandleAndSlug userHandle projectSlug) `or404` (EntityMissing (ErrorID "project:missing") "Project not found")
  addRequestTag "project-id" (IDs.toText projectId)
  AuthZ.permissionGuard $ AuthZ.checkProjectUpdate callerUserId projectId
  let UpdateProjectRequest {summary, tags, visibility} = req
  success <- PG.runTransaction $ Q.updateProject projectId summary tags visibility
  when (not success) $ respondError (EntityMissing (ErrorID "missing-project") "Project could not be found")
  pure ()

deleteProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp ()
deleteProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug = do
  projectId <- PG.runTransaction (Q.projectIDFromHandleAndSlug userHandle projectSlug) `or404` (EntityMissing (ErrorID "project:missing") "Project not found")
  addRequestTag "project-id" (IDs.toText projectId)
  AuthZ.permissionGuard $ AuthZ.checkProjectDelete callerUserId projectId
  PG.runTransaction $ Q.deleteProject projectId

getProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp GetProjectResponse
getProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug = do
  projectId <- PG.runTransaction (Q.projectIDFromHandleAndSlug userHandle projectSlug) `or404` (EntityMissing (ErrorID "project:missing") "Project not found")
  addRequestTag "project-id" (IDs.toText projectId)
  (releaseDownloads, (project, favData, projectOwner, defaultBranch, latestRelease), contributionStats, ticketStats) <- PG.runTransactionOrRespondError do
    projectWithMeta <- (Q.projectByIdWithMetadata callerUserId projectId) `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    releaseDownloads <- Q.releaseDownloadStatsForProject projectId
    contributionStats <- Q.contributionStatsForProject projectId
    ticketStats <- Q.ticketStatsForProject projectId
    pure (releaseDownloads, projectWithMeta, contributionStats, ticketStats)
  let releaseDownloadStats = ReleaseDownloadStats {releaseDownloads}
  AuthZ.permissionGuard $ AuthZ.checkProjectGet callerUserId project
  pure (projectToAPI projectOwner project :++ favData :++ APIProjectBranchAndReleaseDetails {defaultBranch, latestRelease} :++ releaseDownloadStats :++ contributionStats :++ ticketStats)

favProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> FavProjectRequest -> WebApp NoContent
favProjectEndpoint sess userHandle projectSlug (FavProjectRequest {isFaved}) = do
  caller <- AuthN.requireAuthenticatedUser sess
  projectId <- PGO.projectIdByUserHandleAndSlug userHandle projectSlug
  PG.runTransaction $ Q.setProjectFav caller projectId isFaved
  pure NoContent

projectReadmeEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp (Cached JSON ReadmeResponse)
projectReadmeEndpoint session userHandle projectSlug = do
  latestReleaseVersion <- PG.runTransaction $ Q.latestReleaseVersionByProjectShorthand (IDs.ProjectShortHand {userHandle, projectSlug})
  case latestReleaseVersion of
    Nothing -> getProjectBranchReadmeEndpoint session userHandle projectSlug defaultBranchShorthand Nothing
    Just releaseVersion -> getProjectReleaseReadmeEndpoint session userHandle projectSlug releaseVersion

listMaintainersEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp ListMaintainersResponse
listMaintainersEndpoint session projectUserHandle projectSlug = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadProjectMaintainersList caller projectId
  (isPremiumProject, maintainers) <- PG.runTransaction $ do
    maintainers <- ProjectsQ.listProjectMaintainers projectId
    withUserDisplayInfo <- maintainers & UsersQ.userDisplayInfoOf (traversed . traversed)
    isPremiumProject <- ProjectsQ.isPremiumProject projectId
    pure (isPremiumProject, withUserDisplayInfo)
  pure $ ListMaintainersResponse {active = isPremiumProject, maintainers}

addMaintainersEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> AddMaintainersRequest -> WebApp AddMaintainersResponse
addMaintainersEndpoint session projectUserHandle projectSlug (AddMaintainersRequest {maintainers}) = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkUpdateProjectMaintainersList caller projectId
  PG.runTransactionOrRespondError $ do
    ProjectsQ.addMaintainers projectId maintainers >>= \case
      Left alreadyExistingUserIds -> throwError (MaintainersAlreadyExist alreadyExistingUserIds)
      Right updatedMaintainers -> do
        updatedMaintainers' <- UsersQ.userDisplayInfoOf (traversed . traversed) updatedMaintainers
        pure $ AddMaintainersResponse {maintainers = updatedMaintainers'}

updateMaintainersEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> UpdateMaintainersRequest -> WebApp UpdateMaintainersResponse
updateMaintainersEndpoint session projectUserHandle projectSlug (UpdateMaintainersRequest {maintainers}) = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkUpdateProjectMaintainersList caller projectId
  PG.runTransactionOrRespondError $ do
    ProjectsQ.updateMaintainers projectId maintainers >>= \case
      Left missingUsers -> throwError (MaintainersAlreadyExist missingUsers)
      Right updatedMaintainers -> do
        updatedMaintainers' <- UsersQ.userDisplayInfoOf (traversed . traversed) updatedMaintainers
        pure $ UpdateMaintainersResponse {maintainers = updatedMaintainers'}
