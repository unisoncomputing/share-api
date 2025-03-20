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
import Share.Postgres.Authorization.Queries qualified as AuthZQ
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Projects.Queries qualified as ProjectsQ
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Releases.Queries qualified as RQ
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
import Share.Web.Authorization.Types
import Share.Web.Errors
import Share.Web.Share.Branches.Impl (branchesServer, getProjectBranchReadmeEndpoint)
import Share.Web.Share.Contributions.Impl (contributionsByProjectServer)
import Share.Web.Share.Diffs.Impl qualified as Diffs
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse (..), ShareTermDiffResponse (..), ShareTypeDiffResponse (..))
import Share.Web.Share.Projects.API qualified as API
import Share.Web.Share.Projects.Types
import Share.Web.Share.Releases.Impl (getProjectReleaseReadmeEndpoint, releasesServer)
import Share.Web.Share.Roles (canonicalRoleAssignmentOrdering)
import Share.Web.Share.Roles.Queries (displaySubjectsOf)
import Share.Web.Share.Tickets.Impl (ticketsByProjectServer)
import Share.Web.Share.Types
import Unison.Name (Name)
import Unison.Server.Orphans ()
import Unison.Server.Types
import Unison.Syntax.Name qualified as Name

data ProjectErrors
  = RolesAlreadyExist [UserId]
  | RolesMissing [UserId]

instance ToServerError ProjectErrors where
  toServerError = \case
    (RolesAlreadyExist userIds) ->
      ( ErrorID "projects:roles-already-exist",
        Servant.err409
          { errBody = BL.fromStrict . Text.encodeUtf8 $ "Roles already exist: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
          }
      )
    (RolesMissing userIds) ->
      ( ErrorID "projects:roles-missing",
        Servant.err404
          { errBody = BL.fromStrict . Text.encodeUtf8 $ "Roles missing: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
          }
      )

instance Logging.Loggable ProjectErrors where
  toLog = \case
    (RolesAlreadyExist userIds) ->
      Logging.textLog
        ( "Roles already exist: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
        )
        & Logging.withSeverity Logging.UserFault
    (RolesMissing userIds) ->
      Logging.textLog
        ( "Roles missing: " <> (Text.intercalate ", " (IDs.toText <$> userIds))
        )
        & Logging.withSeverity Logging.UserFault

projectServer :: Maybe Session -> UserHandle -> ServerT API.ProjectsAPI WebApp
projectServer session handle =
  let maintainersResourceServer slug =
        listRolesEndpoint session handle slug
          :<|> addRolesEndpoint session handle slug
          :<|> removeRolesEndpoint session handle slug
   in listProjectsForUserEndpoint session handle
        :<|> ( \slug ->
                 hoistServer (Proxy @API.ProjectResourceAPI) (addTags slug) $
                   projectReadmeEndpoint session handle slug
                     :<|> branchesServer session handle slug
                     :<|> releasesServer session handle slug
                     :<|> contributionsByProjectServer session handle slug
                     :<|> ticketsByProjectServer session handle slug
                     :<|> ( diffNamespacesEndpoint session handle slug
                              :<|> projectDiffTermsEndpoint session handle slug
                              :<|> projectDiffTypesEndpoint session handle slug
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
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId projectId
  (oldCodebase, oldCausalId, _oldBranchId) <- namespaceHashForBranchOrRelease authZReceipt project oldShortHand
  (newCodebase, newCausalId, _newBranchId) <- namespaceHashForBranchOrRelease authZReceipt project newShortHand

  let cacheKeys = [IDs.toText projectId, IDs.toText oldShortHand, IDs.toText newShortHand, Caching.causalIdCacheKey oldCausalId, Caching.causalIdCacheKey newCausalId]
  Caching.cachedResponse authZReceipt "project-diff-namespaces" cacheKeys do
    (ancestorCausalId, ancestorCausalHash, newCausalHash) <- PG.runTransaction $ do
      ancestorCausalId <- fromMaybe oldCausalId <$> CausalQ.bestCommonAncestor oldCausalId newCausalId
      (ancestorCausalHash, newCausalHash) <- CausalQ.expectCausalHashesByIdsOf both (ancestorCausalId, newCausalId)
      pure (ancestorCausalId, ancestorCausalHash, newCausalHash)
    namespaceDiff <- respondExceptT (Diffs.diffCausals authZReceipt (oldCodebase, ancestorCausalId) (newCodebase, newCausalId))
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

projectDiffTermsEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.BranchOrReleaseShortHand ->
  IDs.BranchOrReleaseShortHand ->
  Name ->
  Name ->
  WebApp (Cached JSON ShareTermDiffResponse)
projectDiffTermsEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug oldShortHand newShortHand oldTermName newTermName =
  do
    project@Project {projectId} <- PG.runTransactionOrRespondError do
      Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @IDs.ProjectShortHand projectShortHand))
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId projectId

    (oldCodebase, _causalId, oldBhId) <- namespaceHashForBranchOrRelease authZReceipt project oldShortHand
    (newCodebase, _newCausalId, newBhId) <- namespaceHashForBranchOrRelease authZReceipt project newShortHand

    let cacheKeys = [IDs.toText projectId, IDs.toText oldShortHand, IDs.toText newShortHand, Caching.branchIdCacheKey oldBhId, Caching.branchIdCacheKey newBhId, Name.toText oldTermName, Name.toText newTermName]
    Caching.cachedResponse authZReceipt "project-diff-terms" cacheKeys do
      termDiff <- respondExceptT (Diffs.diffTerms authZReceipt (oldCodebase, oldBhId, oldTermName) (newCodebase, newBhId, newTermName))
      pure $
        ShareTermDiffResponse
          { project = projectShortHand,
            oldBranch = oldShortHand,
            newBranch = newShortHand,
            oldTerm = termDiff.left,
            newTerm = termDiff.right,
            diff = termDiff.diff
          }
  where
    projectShortHand :: IDs.ProjectShortHand
    projectShortHand = IDs.ProjectShortHand {userHandle, projectSlug}

projectDiffTypesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.BranchOrReleaseShortHand ->
  IDs.BranchOrReleaseShortHand ->
  Name ->
  Name ->
  WebApp (Cached JSON ShareTypeDiffResponse)
projectDiffTypesEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug oldShortHand newShortHand oldTypeName newTypeName =
  do
    project@Project {projectId} <- PG.runTransactionOrRespondError do
      Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @IDs.ProjectShortHand projectShortHand))
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchDiff callerUserId projectId

    (oldCodebase, _causalId, oldBhId) <- namespaceHashForBranchOrRelease authZReceipt project oldShortHand
    (newCodebase, _newCausalId, newBhId) <- namespaceHashForBranchOrRelease authZReceipt project newShortHand

    let cacheKeys = [IDs.toText projectId, IDs.toText oldShortHand, IDs.toText newShortHand, Caching.branchIdCacheKey oldBhId, Caching.branchIdCacheKey newBhId, Name.toText oldTypeName, Name.toText newTypeName]
    Caching.cachedResponse authZReceipt "project-diff-types" cacheKeys do
      typeDiff <- respondExceptT (Diffs.diffTypes authZReceipt (oldCodebase, oldBhId, oldTypeName) (newCodebase, newBhId, newTypeName))
      pure $
        ShareTypeDiffResponse
          { project = projectShortHand,
            oldBranch = oldShortHand,
            newBranch = newShortHand,
            oldType = typeDiff.left,
            newType = typeDiff.right,
            diff = typeDiff.diff
          }
  where
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
        branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        pure (codebase, causalId, branchHashId)
  IDs.IsReleaseShortHand releaseShortHand -> do
    PG.runTransactionOrRespondError $ do
      release <- Q.releaseByProjectIdAndReleaseShortHand projectId releaseShortHand `whenNothingM` throwError (EntityMissing (ErrorID "release-not-found") ("Release not found: " <> IDs.toText @IDs.ReleaseShortHand releaseShortHand))
      let causalId = Release.squashedCausal release
      let codebaseLoc = Codebase.codebaseLocationForProjectRelease projectOwnerUserId
      let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
      Codebase.codebaseMToTransaction codebase do
        branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
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
  (releaseDownloads, (project, favData, projectOwner, defaultBranch, latestRelease), contributionStats, ticketStats, permissionsForProject) <- PG.runTransactionOrRespondError do
    projectWithMeta <- (Q.projectByIdWithMetadata callerUserId projectId) `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    releaseDownloads <- Q.releaseDownloadStatsForProject projectId
    contributionStats <- Q.contributionStatsForProject projectId
    ticketStats <- Q.ticketStatsForProject projectId
    permissionsForProject <- PermissionsInfo <$> AuthZQ.permissionsForProject callerUserId projectId
    pure (releaseDownloads, projectWithMeta, contributionStats, ticketStats, permissionsForProject)
  let releaseDownloadStats = ReleaseDownloadStats {releaseDownloads}
  AuthZ.permissionGuard $ AuthZ.checkProjectGet callerUserId projectId
  pure (projectToAPI projectOwner project :++ favData :++ APIProjectBranchAndReleaseDetails {defaultBranch, latestRelease} :++ releaseDownloadStats :++ contributionStats :++ ticketStats :++ permissionsForProject)

favProjectEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> FavProjectRequest -> WebApp NoContent
favProjectEndpoint sess userHandle projectSlug (FavProjectRequest {isFaved}) = do
  caller <- AuthN.requireAuthenticatedUser sess
  projectId <- PGO.projectIdByUserHandleAndSlug userHandle projectSlug
  PG.runTransaction $ Q.setProjectFav caller projectId isFaved
  pure NoContent

projectReadmeEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp (Cached JSON ReadmeResponse)
projectReadmeEndpoint session userHandle projectSlug = do
  latestReleaseVersion <- PG.runTransactionOrRespondError $ do
    projectId <- Q.projectIDFromHandleAndSlug userHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    RQ.latestReleaseVersionByProjectId projectId
  case latestReleaseVersion of
    Nothing -> getProjectBranchReadmeEndpoint session userHandle projectSlug defaultBranchShorthand Nothing
    Just releaseVersion -> getProjectReleaseReadmeEndpoint session userHandle projectSlug releaseVersion

listRolesEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> WebApp ListRolesResponse
listRolesEndpoint session projectUserHandle projectSlug = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadProjectRolesList caller projectId
  (isPremiumProject, roleAssignments) <- PG.runTransaction $ do
    projectRoles <- ProjectsQ.listProjectRoles projectId
    roleAssignments <- canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) projectRoles
    isPremiumProject <- ProjectsQ.isPremiumProject projectId
    pure (isPremiumProject, roleAssignments)
  pure $ ListRolesResponse {active = isPremiumProject, roleAssignments}

addRolesEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> AddRolesRequest -> WebApp AddRolesResponse
addRolesEndpoint session projectUserHandle projectSlug (AddRolesRequest {roleAssignments}) = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkAddProjectRoles caller projectId
  PG.runTransaction $ do
    updatedRoles <- ProjectsQ.addProjectRoles projectId roleAssignments
    roleAssignments <- canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) updatedRoles
    pure $ AddRolesResponse {roleAssignments}

removeRolesEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> RemoveRolesRequest -> WebApp RemoveRolesResponse
removeRolesEndpoint session projectUserHandle projectSlug (RemoveRolesRequest {roleAssignments}) = do
  caller <- AuthN.requireAuthenticatedUser session
  projectId <- PG.runTransactionOrRespondError $ do
    Q.projectIDFromHandleAndSlug projectUserHandle projectSlug `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkRemoveProjectRoles caller projectId
  PG.runTransaction $ do
    updatedRoles <- ProjectsQ.removeProjectRoles projectId roleAssignments
    roleAssignments <- canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) updatedRoles
    pure $ RemoveRolesResponse {roleAssignments}
