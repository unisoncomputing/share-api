{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Enlil.Web.Share.Projects.Impl where

import Control.Lens
import Control.Monad.Except
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Enlil.Branch (Branch (..), defaultBranchShorthand)
import Enlil.IDs (PrefixedHash (..), ProjectSlug (..), UserHandle, UserId)
import Enlil.IDs qualified as IDs
import Enlil.OAuth.Session
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Causal.Queries qualified as CausalQ
import Enlil.Postgres.IDs (CausalId)
import Enlil.Postgres.Ops qualified as PGO
import Enlil.Postgres.Projects.Queries qualified as ProjectsQ
import Enlil.Postgres.Queries qualified as Q
import Enlil.Postgres.Users.Queries qualified as UsersQ
import Enlil.Prelude
import Enlil.Project (Project (..))
import Enlil.Release (Release (..))
import Enlil.User (User (..))
import Enlil.Utils.API ((:++) (..))
import Enlil.Utils.Caching (Cached)
import Enlil.Utils.Caching qualified as Caching
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.App
import Enlil.Web.Authentication qualified as AuthN
import Enlil.Web.Authorization qualified as AuthZ
import Enlil.Web.Errors
import Enlil.Web.Share.Branches.Impl (branchesServer, getProjectBranchReadmeEndpoint)
import Enlil.Web.Share.Contributions.Impl (contributionsByProjectServer)
import Enlil.Web.Share.Diffs.Impl qualified as Diffs
import Enlil.Web.Share.Diffs.Types (ShareNamespaceDiffResponse (..))
import Enlil.Web.Share.Projects.API qualified as API
import Enlil.Web.Share.Projects.Types
import Enlil.Web.Share.Releases.Impl (getProjectReleaseReadmeEndpoint, releasesServer)
import Enlil.Web.Share.Tickets.Impl (ticketsByProjectServer)
import Enlil.Web.Share.Types
import Servant

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
                     :<|> diffNamespacesEndpoint session handle slug
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
  oldCausalId <- resolveBranchOrRelease projectId oldShortHand
  newCausalId <- resolveBranchOrRelease projectId newShortHand

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
    resolveBranchOrRelease :: IDs.ProjectId -> IDs.BranchOrReleaseShortHand -> WebApp CausalId
    resolveBranchOrRelease projectId = \case
      IDs.IsBranchShortHand branchShortHand -> do
        PG.runTransactionOrRespondError $ do
          Branch {causal = branchHead} <- Q.branchByProjectIdAndShortHand projectId branchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch-not-found") ("Branch not found: " <> IDs.toText @IDs.BranchShortHand branchShortHand))
          pure branchHead
      IDs.IsReleaseShortHand releaseShortHand -> do
        PG.runTransactionOrRespondError $ do
          Release {squashedCausal = releaseHead} <- Q.releaseByProjectIdAndReleaseShortHand projectId releaseShortHand `whenNothingM` throwError (EntityMissing (ErrorID "release-not-found") ("Release not found: " <> IDs.toText @IDs.ReleaseShortHand releaseShortHand))
          pure releaseHead

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
