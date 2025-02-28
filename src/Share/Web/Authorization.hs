{-# LANGUAGE DuplicateRecordFields #-}

-- | Contains authorization checks for various resources.
module Share.Web.Authorization
  ( assertCausalHashAccessibleFromRoot,
    checkProjectCreate,
    checkProjectUpdate,
    checkProjectDelete,
    checkProjectGet,
    checkBranchGet,
    checkProjectBranchRead,
    checkProjectBranchDiff,
    checkProjectReleaseRead,
    checkReadProjectMaintainersList,
    checkUpdateProjectMaintainersList,
    checkUserIsAdmin,
    checkAdminSudo,
    checkBranchCreate,
    checkBranchSet,
    checkBranchDelete,
    checkReleaseGet,
    checkReleaseCreate,
    checkReleaseUpdate,
    checkListBranchesForProject,
    checkListReleasesForProject,
    checkContributionListByProject,
    checkContributionCreate,
    checkContributionUpdate,
    checkContributionMerge,
    checkContributionDiffRead,
    checkContributionRead,
    checkContributionTimelineRead,
    checkCommentCreate,
    checkCommentUpdate,
    checkCommentDelete,
    checkTicketListByProject,
    checkTicketCreate,
    checkTicketUpdate,
    checkTicketRead,
    checkTicketTimelineRead,
    checkReadUserCodebase,
    checkWriteUserCodebase,
    checkUploadToUserCodebase,
    checkUploadToProjectBranchCodebase,
    checkUserUpdate,
    checkDownloadFromUserCodebase,
    checkDownloadFromProjectBranchCodebase,
    permissionGuard,
    readPath,
    writePath,
    adminOverride,
    backgroundJobAuthZ,
    migrationOverride,
    userCreationOverride,
    Permission (..),
    CodebasePermission (..),
    ProjectPermission (..),
    CachingToken,
    AuthZFailure,

    -- * Utils
    authZFailureMessage,
    AuthZReceipt,
    getCacheability,
  )
where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Servant
import Share.BackgroundJobs.Monad (Background)
import Share.Branch
import Share.Contribution (Contribution (..), ContributionStatus (..))
import Share.IDs
import Share.OAuth.Session (Session (sessionCreated))
import Share.OAuth.Session qualified as Session
import Share.Postgres qualified as PG
import Share.Postgres.Authorization.Queries qualified as Q
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import Share.Project
import Share.Release
import Share.Ticket
import Share.User (User (..), UserVisibility (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Authorization.Types (RolePermission (..))
import Share.Web.Errors
import Share.Web.Errors qualified as Errors
import Share.Web.Share.Comments
import Share.Web.Share.Contributions.Types (UpdateContributionRequest (..))
import Share.Web.Share.Tickets.Types
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment.Internal (NameSegment (..))

-- | Proof that an auth check has been run at some point.
data AuthZReceipt = AuthZReceipt {getCacheability :: Maybe CachingToken}

data Permission
  = CodebasePermission CodebasePermission
  | ProjectPermission ProjectPermission
  | UserPermission UserPermission
  | AdminPermission
  | SudoPermission
  deriving stock (Show, Eq, Ord)

data CodebasePermission
  = UserCodebaseReadPath [NameSegment]
  | UserCodebaseWritePath [NameSegment]
  | CodebaseUpload
  | CodebaseDownload
  deriving stock (Show, Eq, Ord)

data ProjectPermission
  = ProjectCreate UserId
  | ProjectUpdate ProjectId
  | ProjectDelete ProjectId
  | ProjectGet ProjectId
  | BranchGet BranchId ProjectId
  | ReleaseGet ReleaseId ProjectId
  | BranchCreate ProjectId
  | BranchDelete ProjectId
  | BranchSet ProjectId
  | ReleaseCreate ProjectId
  | ReleaseUpdate ProjectId
  | ReleaseSet ProjectId
  | ReleaseRead ProjectId
  | ProjectBranchBrowse ProjectId
  | ProjectBranchDiff ProjectId
  | ProjectBranchList ProjectId
  | ProjectReleaseList ProjectId
  | ContributionList ProjectId
  | ContributionCreate ProjectId
  | ContributionUpdate ProjectId
  | ContributionRead ProjectId
  | ContributionTimelineGet ProjectId
  | CommentCreate ProjectId
  | CommentUpdate CommentId
  | CommentDelete CommentId
  | TicketList ProjectId
  | TicketCreate ProjectId
  | TicketUpdate ProjectId
  | TicketRead ProjectId
  | TicketTimelineGet ProjectId
  | MaintainersList ProjectId
  | MaintainersEdit ProjectId
  | -- (RootHash, TargetHash)
    AccessCausalHash CausalId CausalId
  deriving stock (Show, Eq, Ord)

data UserPermission
  = UserUpdate UserId
  deriving stock (Show, Eq, Ord)

data AuthZFailure = AuthZFailure Permission
  deriving stock (Show)

instance Logging.Loggable AuthZFailure where
  toLog = Logging.withSeverity Logging.UserFault . Logging.showLog

instance Errors.ToServerError AuthZFailure where
  toServerError (AuthZFailure perm) = case perm of
    CodebasePermission cp -> case cp of
      UserCodebaseReadPath {} ->
        (ErrorID "authz:read-path", err403 {errBody = "Permission Denied: " <> msg})
      UserCodebaseWritePath {} ->
        (ErrorID "authz:write-path", err403 {errBody = "Permission Denied: " <> msg})
      CodebaseUpload ->
        (ErrorID "authz:upload", err403 {errBody = "Permission Denied: " <> msg})
      CodebaseDownload ->
        (ErrorID "authz:download", err403 {errBody = "Permission Denied: " <> msg})
    ProjectPermission pp -> case pp of
      ProjectCreate _uid -> (ErrorID "authz:project:create", err403 {errBody = "Permission Denied: " <> msg})
      ProjectUpdate _pid -> (ErrorID "authz:project:update", err403 {errBody = "Permission Denied: " <> msg})
      ProjectDelete _pid -> (ErrorID "authz:project:delete", err403 {errBody = "Permission Denied: " <> msg})
      -- We mask 403s for gets as 404s to avoid leaking info about which projects might exist.
      ProjectGet _pid -> (ErrorID "authz:project:get", err404 {errBody = "Not Found: " <> msg})
      BranchGet _bid _pid -> (ErrorID "authz:branch:get", err404 {errBody = "Not Found: " <> msg})
      BranchCreate _pid -> (ErrorID "authz:branch:create", err403 {errBody = "Permission Denied: " <> msg})
      BranchDelete _pid -> (ErrorID "authz:branch:delete", err403 {errBody = "Permission Denied: " <> msg})
      BranchSet _pid -> (ErrorID "authz:branch:set", err403 {errBody = "Permission Denied: " <> msg})
      ReleaseGet _rid _pid -> (ErrorID "authz:release:get", err404 {errBody = "Not Found: " <> msg})
      ReleaseCreate _pid -> (ErrorID "authz:release:create", err403 {errBody = "Permission Denied: " <> msg})
      ReleaseUpdate _pid -> (ErrorID "authz:release:update", err403 {errBody = "Permission Denied: " <> msg})
      ReleaseSet _pid -> (ErrorID "authz:release:set", err403 {errBody = "Permission Denied: " <> msg})
      ReleaseRead _pid -> (ErrorID "authz:release:read", err403 {errBody = "Permission Denied: " <> msg})
      ProjectBranchBrowse _pid -> (ErrorID "authz:project:browse", err404 {errBody = "Not Found: " <> msg})
      ProjectBranchDiff _pid -> (ErrorID "authz:project:diff", err404 {errBody = "Not Found: " <> msg})
      ProjectBranchList _pid -> (ErrorID "authz:project:list", err404 {errBody = "Not Found: " <> msg})
      ProjectReleaseList _pid -> (ErrorID "authz:project:list", err404 {errBody = "Not Found: " <> msg})
      ContributionList _pid -> (ErrorID "authz:contribution:list", err404 {errBody = "Not Found: " <> msg})
      ContributionCreate _pid -> (ErrorID "authz:contribution:create", err403 {errBody = "Permission Denied: " <> msg})
      ContributionUpdate _pid -> (ErrorID "authz:contribution:update", err403 {errBody = "Permission Denied: " <> msg})
      ContributionRead _pid -> (ErrorID "authz:contribution:read", err403 {errBody = "Permission Denied: " <> msg})
      ContributionTimelineGet _pid -> (ErrorID "authz:contribution:timeline", err403 {errBody = "Permission Denied: " <> msg})
      CommentCreate _pid -> (ErrorID "authz:comment:create", err403 {errBody = "Permission Denied: " <> msg})
      CommentUpdate _comment -> (ErrorID "authz:comment:update", err403 {errBody = "Permission Denied: " <> msg})
      CommentDelete _comment -> (ErrorID "authz:comment:delete", err403 {errBody = "Permission Denied: " <> msg})
      TicketList _pid -> (ErrorID "authz:ticket:list", err404 {errBody = "Not Found: " <> msg})
      TicketCreate _pid -> (ErrorID "authz:ticket:create", err403 {errBody = "Permission Denied: " <> msg})
      TicketUpdate _pid -> (ErrorID "authz:ticket:update", err403 {errBody = "Permission Denied: " <> msg})
      TicketRead _pid -> (ErrorID "authz:ticket:read", err403 {errBody = "Permission Denied: " <> msg})
      TicketTimelineGet _pid -> (ErrorID "authz:ticket:timeline", err403 {errBody = "Permission Denied: " <> msg})
      MaintainersList _pid -> (ErrorID "authz:maintainers:list", err403 {errBody = "Permission Denied: " <> msg})
      MaintainersEdit _pid -> (ErrorID "authz:maintainers:edit", err403 {errBody = "Permission Denied: " <> msg})
      AccessCausalHash _ _ -> (ErrorID "authz:causal-hash", err403 {errBody = "Permission Denied: " <> msg})
    UserPermission userPermission ->
      case userPermission of
        UserUpdate _uid -> (ErrorID "authz:user:update", err403 {errBody = "Permission Denied: " <> msg})
    AdminPermission ->
      (ErrorID "authz:admin", err403 {errBody = "Permission Denied: " <> msg})
    SudoPermission ->
      (ErrorID "authz:sudo", err403 {errBody = "Permission Denied: " <> msg})
    where
      msg = BL.fromStrict . Text.encodeUtf8 $ authZFailureMessage (AuthZFailure perm)

authZFailureMessage :: AuthZFailure -> Text
authZFailureMessage (AuthZFailure perm) = case perm of
  CodebasePermission cp -> case cp of
    UserCodebaseReadPath path -> "Not permitted to read " <> tShow path
    UserCodebaseWritePath path -> "Not permitted to write " <> tShow path
    CodebaseUpload -> "Not permitted to upload to this codebase"
    CodebaseDownload -> "Not permitted to download from this codebase"
  ProjectPermission pp -> case pp of
    ProjectCreate _uid -> "Not permitted to create this project"
    ProjectUpdate _pid -> "Not permitted to update this project"
    ProjectDelete _pid -> "Not permitted to delete this project"
    ProjectGet _pid -> "No project found"
    BranchGet _bid _pid -> "No branch found"
    BranchCreate _pid -> "Not permitted to create this branch"
    BranchDelete _pid -> "Not permitted to delete this branch"
    BranchSet _pid -> "Not permitted to update this branch"
    ReleaseGet _bid _pid -> "No release found"
    ReleaseCreate _pid -> "Not permitted to create a release in this project"
    ReleaseUpdate _pid -> "Not permitted to update this release"
    ReleaseSet _pid -> "Not permitted to update this release"
    ReleaseRead _pid -> "Not permitted to read this release"
    ProjectBranchBrowse _pid -> "No project found"
    ProjectBranchDiff _pid -> "No project found"
    ProjectBranchList _pid -> "No project found"
    ProjectReleaseList _pid -> "No project found"
    ContributionList _pid -> "No project found"
    ContributionCreate _pid -> "Not permitted to create a contribution in this project"
    ContributionUpdate _pid -> "Not permitted to update this contribution"
    ContributionRead _pid -> "Not permitted to read this contribution"
    ContributionTimelineGet _pid -> "Not permitted to read this contribution"
    CommentCreate _pid -> "Not permitted to create a comment on this ticket or contribution"
    CommentUpdate _comment -> "Not permitted to update this comment"
    CommentDelete _comment -> "Not permitted to delete this comment"
    TicketList _pid -> "No project found"
    TicketCreate _pid -> "Not permitted to create an ticket in this project"
    TicketUpdate _pid -> "Not permitted to update this ticket"
    TicketRead _pid -> "Not permitted to read this ticket"
    TicketTimelineGet _pid -> "Not permitted to read this ticket"
    MaintainersList _pid -> "Not permitted to list maintainers"
    MaintainersEdit _pid -> "Not permitted to edit maintainers"
    AccessCausalHash _ _ -> "Not permitted to access this causal hash"
  UserPermission userPermission ->
    case userPermission of
      UserUpdate _uid -> "Not permitted to update this user"
  AdminPermission -> "Not permitted to access this resource"
  SudoPermission -> "Sudo mode required to perform this action. Please re-authenticate to enable sudo mode."

readPath :: Path -> CodebasePermission
readPath path = UserCodebaseReadPath (Path.toList path)

writePath :: Path -> CodebasePermission
writePath path = UserCodebaseWritePath (Path.toList path)

isPublicPath :: [NameSegment] -> Bool
isPublicPath (NameSegment "public" : _) = True
isPublicPath _ = False

-- | Requests should only be cached if they're for a public endpoint.
-- Obtaining a caching token is proof that the resource was public and can be cached.
data CachingToken = CachingToken

-- | Checks that A given causal is accessible from the other, useful for determining that a
-- rootHash is attached to a branch head that we can validate access to.
--
-- Throws a permission failure if the causal is not accessible.
assertCausalHashAccessibleFromRoot :: CausalId -> CausalId -> WebApp ()
assertCausalHashAccessibleFromRoot rootCausalId targetCausalId = permissionGuard $ maybePermissionFailure (ProjectPermission $ AccessCausalHash rootCausalId targetCausalId) do
  guardM . PG.runTransaction $ Q.causalIsInHistoryOf rootCausalId targetCausalId

checkReadUserCodebase :: Maybe UserId -> User -> Path -> WebApp (Either AuthZFailure AuthZReceipt)
checkReadUserCodebase mayRequestingUser (User {visibility, user_id = targetUserId}) (Path.toList -> path) = maybePermissionFailure (CodebasePermission $ UserCodebaseReadPath path) do
  case visibility of
    UserPublic -> pure $ AuthZReceipt (Just CachingToken)
    UserPrivate -> do
      reqUserId <- guardMaybe mayRequestingUser
      assertUserIsUserMaintainer reqUserId targetUserId
      pure $ AuthZReceipt Nothing

checkWriteUserCodebase :: UserId -> User -> Path -> WebApp (Either AuthZFailure AuthZReceipt)
checkWriteUserCodebase requestingUser (User {user_id, visibility}) (Path.toList -> path) = maybePermissionFailure (CodebasePermission $ UserCodebaseReadPath path) do
  assertUserIsUserMaintainer requestingUser user_id
  case visibility of
    UserPublic -> do
      -- We currently don't allow writes to non-public paths for public users, since
      -- non-public users are currently internal-only
      guard (isPublicPath path)
      pure $ AuthZReceipt Nothing
    UserPrivate -> do
      -- We allow writing on any path for private users. This is a custom tweak for the cloud
      -- user, once projects are rolled out we can remove "private users" entirely.
      pure $ AuthZReceipt Nothing

-- | Check that the caller is allowed to upload to the specified codebase.
checkUploadToProjectBranchCodebase ::
  -- | Requesting user
  UserId ->
  ProjectId ->
  -- | Contributor user Id
  Maybe UserId ->
  WebApp (Either AuthZFailure AuthZReceipt)
checkUploadToProjectBranchCodebase reqUserId projectId mayContributorUserId = maybePermissionFailure (CodebasePermission CodebaseUpload) do
  case mayContributorUserId of
    Nothing -> do
      assertUserHasProjectPermission ProjectContribute reqUserId projectId
      pure $ AuthZReceipt Nothing
    -- Only the contributor themself can upload to their contributor branches.
    -- We may wish to allow project maintainers to upload to contributor branches within their
    -- projects in the future.
    Just contributorId -> do
      guard (contributorId == reqUserId)
      pure $ AuthZReceipt Nothing

checkUserUpdate :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZReceipt)
checkUserUpdate reqUserId targetUserId = maybePermissionFailure (UserPermission (UserUpdate targetUserId)) $ do
  guard (reqUserId == targetUserId) <|> checkUserIsOrgMember reqUserId targetUserId
  pure $ AuthZReceipt Nothing

-- | Check that the caller is allowed to upload to the specified codebase.
checkUploadToUserCodebase ::
  -- | Requesting user
  UserId ->
  -- | Codebase owner user
  UserId ->
  WebApp (Either AuthZFailure AuthZReceipt)
checkUploadToUserCodebase reqUserId codebaseOwnerUserId = maybePermissionFailure (CodebasePermission CodebaseUpload) do
  assertUserIsUserMaintainer reqUserId codebaseOwnerUserId
  pure $ AuthZReceipt Nothing

-- | The download endpoint currently does all of its own auth using HashJWTs,
-- So we don't add any other authz checks here, the HashJWT check is sufficient.
checkDownloadFromUserCodebase :: WebApp (Either AuthZFailure AuthZReceipt)
checkDownloadFromUserCodebase =
  pure . Right $ AuthZReceipt Nothing

-- | The download endpoint currently does all of its own auth using HashJWTs,
-- So we don't add any other authz checks here, the HashJWT check is sufficient.
checkDownloadFromProjectBranchCodebase :: WebApp (Either AuthZFailure AuthZReceipt)
checkDownloadFromProjectBranchCodebase =
  pure . Right $ AuthZReceipt Nothing

checkProjectCreate :: Maybe UserId -> UserId -> WebApp (Either AuthZFailure AuthZReceipt)
checkProjectCreate mayReqUserId targetUserId = maybePermissionFailure (ProjectPermission (ProjectCreate targetUserId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserIsUserMaintainer reqUserId targetUserId
  pure $ AuthZReceipt Nothing

checkProjectUpdate :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure ())
checkProjectUpdate mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ProjectUpdate targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission ProjectManage reqUserId targetProjectId

checkProjectDelete :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure ())
checkProjectDelete mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ProjectDelete targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission ProjectManage reqUserId targetProjectId

checkProjectGet :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkProjectGet reqUserId (Project {projectId, visibility}) = maybePermissionFailure (ProjectPermission (ProjectGet projectId)) $ do
  case visibility of
    ProjectPublic -> pure $ AuthZReceipt (Just CachingToken)
    ProjectPrivate -> do
      uid <- guardMaybe reqUserId
      assertUserHasProjectPermission ProjectView uid projectId
      pure $ AuthZReceipt Nothing

-- | Currently you can access any branch within a project you can access.
checkBranchGet :: Maybe UserId -> Project -> Branch causal -> WebApp (Either AuthZFailure AuthZReceipt)
checkBranchGet mayCallerUserId project (Branch {projectId, branchId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayCallerUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (BranchGet branchId projectId)

checkReleaseGet :: Maybe UserId -> Project -> Release causal v -> WebApp (Either AuthZFailure AuthZReceipt)
checkReleaseGet mayCallerUserId project (Release {projectId, releaseId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayCallerUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ReleaseGet releaseId projectId)

-- | Checks whether a user has access to create a core branch for the provided project, within
-- an optional contributor namespace.
checkBranchCreate :: UserId -> Project -> Maybe UserId -> WebApp (Either AuthZFailure AuthZReceipt)
checkBranchCreate reqUserId project@(Project {projectId = projectId}) mayContributorNamespace = maybePermissionFailure (ProjectPermission (BranchCreate projectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission ProjectContribute reqUserId projectId
    Just contributorUserId -> do
      checkAllowedAsProjectContributor reqUserId project
      -- For now we allow only the contributor to access their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZReceipt Nothing

-- | Checks whether a user has access to delete a branch for the provided project
checkBranchDelete :: UserId -> Project -> Branch causal -> WebApp (Either AuthZFailure AuthZReceipt)
checkBranchDelete reqUserId project@(Project {projectId = projectId}) Branch {contributorId = mayContributorNamespace} = maybePermissionFailure (ProjectPermission (BranchDelete projectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission ProjectContribute reqUserId projectId
    Just contributorUserId -> do
      checkAllowedAsProjectContributor reqUserId project
      -- For now we allow only the contributor to manage their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZReceipt Nothing

-- | Checks whether a given user may contribute to the project.
checkAllowedAsProjectContributor :: UserId -> Project -> MaybeT WebApp ()
checkAllowedAsProjectContributor reqUserId project = do
  -- For now we just check that the caller has access to the project in order to be a contributor.
  _ <- MaybeT (eitherToMaybe <$> checkProjectGet (Just reqUserId) project)
  pure ()

-- | Checks whether a user has access to create a release for the provided project.
checkReleaseCreate :: UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkReleaseCreate reqUserId Project {projectId = targetProjectId} = maybePermissionFailure (ProjectPermission (ReleaseCreate targetProjectId)) $ do
  assertUserHasProjectPermission ProjectContribute reqUserId targetProjectId
  pure $ AuthZReceipt Nothing

checkReleaseUpdate :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZReceipt)
checkReleaseUpdate mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ReleaseUpdate targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission ProjectContribute reqUserId targetProjectId
  pure $ AuthZReceipt Nothing

-- | Checks whether a user has access to set a core branch for the provided project to a
-- new Causal
checkBranchSet :: UserId -> Project -> Branch causal -> WebApp (Either AuthZFailure AuthZReceipt)
checkBranchSet reqUserId project Branch {projectId = targetProjectId, contributorId = mayContributorNamespace} = maybePermissionFailure (ProjectPermission (BranchSet targetProjectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission ProjectContribute reqUserId targetProjectId
    Just contributorUserId -> do
      checkAllowedAsProjectContributor reqUserId project
      -- For now we allow only the contributor to access their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZReceipt Nothing

checkProjectBranchRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkProjectBranchRead reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ (ProjectPermission (ProjectBranchBrowse projectId))

checkProjectBranchDiff :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkProjectBranchDiff reqUserId project@Project {projectId} =
  bimap (const authzError) makeCacheable <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ (ProjectPermission (ProjectBranchDiff projectId))

checkListBranchesForProject :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkListBranchesForProject reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ProjectBranchList projectId)

checkProjectReleaseRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkProjectReleaseRead reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ReleaseRead projectId)

checkReadProjectMaintainersList :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZReceipt)
checkReadProjectMaintainersList reqUserId projectId =
  maybePermissionFailure (ProjectPermission (MaintainersList projectId)) $ do
    assertUserHasProjectPermission ProjectManage reqUserId projectId
    pure $ AuthZReceipt Nothing

checkUpdateProjectMaintainersList :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZReceipt)
checkUpdateProjectMaintainersList reqUserId projectId =
  maybePermissionFailure (ProjectPermission (MaintainersEdit projectId)) $ do
    assertUserHasProjectPermission ProjectManage reqUserId projectId
    pure $ AuthZReceipt Nothing

checkListReleasesForProject :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkListReleasesForProject reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ProjectReleaseList projectId)

checkContributionListByProject :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionListByProject reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionList projectId)

checkContributionCreate :: UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionCreate reqUserId project@(Project {projectId}) =
  maybePermissionFailure (ProjectPermission (ContributionCreate projectId)) $ do
    checkAllowedAsProjectContributor reqUserId project
    pure $ AuthZReceipt Nothing

checkContributionUpdate :: UserId -> Contribution -> UpdateContributionRequest -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionUpdate reqUserId (Contribution {projectId, author}) UpdateContributionRequest {status = newStatus} =
  maybePermissionFailure (ProjectPermission (ContributionUpdate projectId)) $ do
    case newStatus of
      Just Merged -> do
        -- Only project maintainers can mark as merged
        assertUserHasProjectPermission ProjectContribute reqUserId projectId
        pure $ AuthZReceipt Nothing
      _ -> do
        guard (reqUserId == author) <|> assertUserHasProjectPermission ProjectContribute reqUserId projectId
        pure $ AuthZReceipt Nothing

checkContributionMerge :: UserId -> Contribution -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionMerge reqUserId (Contribution {projectId}) =
  maybePermissionFailure (ProjectPermission (ContributionUpdate projectId)) $ do
    assertUserHasProjectPermission ProjectContribute reqUserId projectId
    pure $ AuthZReceipt Nothing

checkContributionRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionRead mayReqUserId project@(Project {projectId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayReqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionRead projectId)

checkContributionDiffRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionDiffRead mayReqUserId project@(Project {projectId}) =
  bimap (const authzError) makeCacheable <$> checkProjectGet mayReqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionRead projectId)

checkContributionTimelineRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkContributionTimelineRead mayReqUserId project@(Project {projectId}) =
  mapLeft (const authzError) <$> do
    checkContributionRead mayReqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionTimelineGet projectId)

checkCommentCreate :: UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkCommentCreate userId project@(Project {projectId}) =
  mapLeft (const authzError) <$> do
    checkContributionRead (Just userId) project
  where
    authzError = AuthZFailure $ ProjectPermission (CommentCreate projectId)

checkCommentUpdate :: UserId -> Comment UserId -> WebApp (Either AuthZFailure AuthZReceipt)
checkCommentUpdate userId (Comment {actor = authorUserId, commentId}) =
  if (userId == authorUserId)
    then pure $ Right $ AuthZReceipt Nothing
    else pure $ Left authzError
  where
    authzError = AuthZFailure $ ProjectPermission (CommentUpdate commentId)

-- TODO: Allow project maintainers to delete comments
checkCommentDelete :: UserId -> Comment UserId -> WebApp (Either AuthZFailure AuthZReceipt)
checkCommentDelete userId (Comment {actor = authorUserId, commentId}) =
  if (userId == authorUserId)
    then pure $ Right $ AuthZReceipt Nothing
    else pure $ Left authzError
  where
    authzError = AuthZFailure $ ProjectPermission (CommentDelete commentId)

checkTicketListByProject :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkTicketListByProject reqUserId project@Project {projectId} =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (TicketList projectId)

checkTicketCreate :: UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkTicketCreate reqUserId project@(Project {projectId}) =
  maybePermissionFailure (ProjectPermission (TicketCreate projectId)) $ do
    checkAllowedAsProjectContributor reqUserId project
    pure $ AuthZReceipt Nothing

checkTicketUpdate :: UserId -> Ticket -> UpdateTicketRequest -> WebApp (Either AuthZFailure AuthZReceipt)
checkTicketUpdate reqUserId (Ticket {projectId, author}) UpdateTicketRequest {} =
  maybePermissionFailure (ProjectPermission (TicketUpdate projectId)) $ do
    guard (reqUserId == author) <|> assertUserHasProjectPermission (ProjectContribute) reqUserId projectId
    pure $ AuthZReceipt Nothing

checkTicketRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkTicketRead mayReqUserId project@(Project {projectId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayReqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (TicketRead projectId)

checkTicketTimelineRead :: Maybe UserId -> Project -> WebApp (Either AuthZFailure AuthZReceipt)
checkTicketTimelineRead mayReqUserId project@(Project {projectId}) =
  mapLeft (const authzError) <$> do
    checkTicketRead mayReqUserId project
  where
    authzError = AuthZFailure $ ProjectPermission (TicketTimelineGet projectId)

assertUserIsUserMaintainer :: UserId -> UserId -> MaybeT WebApp ()
assertUserIsUserMaintainer reqUserId targetUserId =
  guardM . lift . PG.runTransaction $ Q.checkIsUserMaintainer reqUserId targetUserId

assertUserHasProjectPermission :: RolePermission -> UserId -> ProjectId -> MaybeT WebApp ()
assertUserHasProjectPermission rolePermission reqUserId projId = do
  guardM $ PG.runTransaction $ do
    Q.userHasProjectPermission reqUserId projId rolePermission

maybePermissionFailure :: Permission -> MaybeT WebApp a -> WebApp (Either AuthZFailure a)
maybePermissionFailure perm m = do
  runMaybeT m >>= \case
    Nothing -> pure (Left (AuthZFailure perm))
    Just a -> pure (Right a)

-- | Check whether the given user has administrative privileges
-- Currently this means checking whether the user is a member of the Unison org.
checkUserIsAdmin :: UserId -> WebApp (Either AuthZFailure ())
checkUserIsAdmin userId = do
  PG.runTransaction $
    Q.isUnisonEmployee userId <&> \case
      False -> Left (AuthZFailure AdminPermission)
      True -> Right ()

checkUserIsOrgMember :: UserId -> UserId -> MaybeT WebApp ()
checkUserIsOrgMember reqUserId orgUserId = do
  guardM $
    PG.runTransaction $
      Q.isOrgMember reqUserId orgUserId

-- | Check whether the given user has administrative privileges,
-- and has a recently created session. This adds additional protection to
-- sensitive endpoints.
checkAdminSudo :: Session.Session -> WebApp (Either AuthZFailure ())
checkAdminSudo Session.Session {sessionUserId, sessionCreated} = runExceptT $ do
  now <- liftIO Time.getCurrentTime
  ExceptT $ checkUserIsAdmin sessionUserId
  ExceptT $
    if Time.diffUTCTime now sessionCreated < sudoTTL
      then pure (Right ())
      else pure (Left (AuthZFailure SudoPermission))
  where
    sudoTTL = 60 * 60 * 2 -- 2 hours

-- | Get an auth token for admin endpoints.
-- Only use this after already checking administrative auth.
adminOverride :: AuthZReceipt
adminOverride = AuthZReceipt Nothing

-- | Get an auth token for migrations.
migrationOverride :: AuthZReceipt
migrationOverride = AuthZReceipt Nothing

-- | An auth token for a user we just created.
-- Only use this during a user creation flow.
userCreationOverride :: AuthZReceipt
userCreationOverride = AuthZReceipt Nothing

backgroundJobAuthZ :: Background AuthZReceipt
backgroundJobAuthZ = pure $ AuthZReceipt Nothing

permissionGuard :: WebApp (Either AuthZFailure a) -> WebApp a
permissionGuard m =
  m >>= \case
    Right a -> pure a
    Left err -> Errors.respondError err

-- | Make an auth receipt cacheable.
-- useful when we're re-using an existing auth receipt, but know that the current endpoint is
-- cacheable for authed users even if the original isn't.
makeCacheable :: AuthZReceipt -> AuthZReceipt
makeCacheable (AuthZReceipt _) = AuthZReceipt (Just CachingToken)
