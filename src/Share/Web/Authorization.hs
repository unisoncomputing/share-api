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
    checkReadProjectRolesList,
    checkAddProjectRoles,
    checkRemoveProjectRoles,
    checkUserIsSuperadmin,
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
    checkUploadToUserCodebase,
    checkUploadToProjectBranchCodebase,
    checkUserUpdate,
    checkDownloadFromUserCodebase,
    checkDownloadFromProjectBranchCodebase,
    checkCreateOrg,
    checkReadOrgRolesList,
    checkEditOrgRoles,
    checkReadOrgMembers,
    checkEditOrgMembers,
    checkNotificationsGet,
    checkNotificationsUpdate,
    checkDeliveryMethodsView,
    checkDeliveryMethodsManage,
    checkSubscriptionsView,
    checkSubscriptionsManage,
    permissionGuard,
    readPath,
    writePath,
    adminOverride,
    backgroundJobAuthZ,
    migrationOverride,
    userCreationOverride,
    WrapperPermissions (..),
    CodebasePermission (..),
    ProjectPermission (..),
    AuthZ.CachingToken,
    AuthZFailure,

    -- * Utils
    authZFailureMessage,
    AuthZ.AuthZReceipt,
    AuthZ.getCacheability,
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
import Share.Postgres.Projects.Queries qualified as PQ
import Share.Prelude
import Share.Project
import Share.Release
import Share.Ticket
import Share.User (User (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Authorization.Types qualified as AuthZ
import Share.Web.Errors
import Share.Web.Errors qualified as Errors
import Share.Web.Share.Comments
import Share.Web.Share.Contributions.Types (UpdateContributionRequest (..))
import Share.Web.Share.Orgs.Queries qualified as OrgQ
import Share.Web.Share.Orgs.Types (Org (..))
import Share.Web.Share.Tickets.Types
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment.Internal (NameSegment (..))

-- | Represents higher-level permissions, representing the ability to perform an individual
-- task. Distinct from 'RolePermission's which are the actual permissions stored in the
-- database.
data WrapperPermissions
  = CodebasePermission CodebasePermission
  | ProjectPermission ProjectPermission
  | UserPermission UserPermission
  | OrgPermission OrgPermission
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
  | ProjectRolesList ProjectId
  | ProjectRolesEdit ProjectId
  | -- (RootHash, TargetHash)
    AccessCausalHash CausalId CausalId
  deriving stock (Show, Eq, Ord)

data UserPermission
  = UserUpdate UserId
  | UserNotificationHubEntryView UserId
  | UserNotificationHubEntryUpdate UserId
  | UserNotificationSubscriptionView UserId
  | UserNotificationSubscriptionManage UserId
  | UserNotificationDeliveryMethodView UserId
  | UserNotificationDeliveryMethodManage UserId
  deriving stock (Show, Eq, Ord)

data OrgPermission
  = OrgRolesList OrgId
  | OrgRolesEdit OrgId
  | OrgCreate UserId
  | OrgMembersEdit OrgId
  deriving stock (Show, Eq, Ord)

data AuthZFailure = AuthZFailure WrapperPermissions
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
      ProjectRolesList _pid -> (ErrorID "authz:maintainers:list", err403 {errBody = "Permission Denied: " <> msg})
      ProjectRolesEdit _pid -> (ErrorID "authz:maintainers:edit", err403 {errBody = "Permission Denied: " <> msg})
      AccessCausalHash _ _ -> (ErrorID "authz:causal-hash", err403 {errBody = "Permission Denied: " <> msg})
    UserPermission userPermission ->
      case userPermission of
        UserUpdate _uid -> (ErrorID "authz:user:update", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationHubEntryView _uid -> (ErrorID "authz:user:notification-get", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationHubEntryUpdate _uid -> (ErrorID "authz:user:notification-update", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationSubscriptionView _uid -> (ErrorID "authz:user:notification-subscription-get", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationSubscriptionManage _uid -> (ErrorID "authz:user:notification-subscription-manage", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationDeliveryMethodView _uid -> (ErrorID "authz:user:notification-delivery-method-get", err403 {errBody = "Permission Denied: " <> msg})
        UserNotificationDeliveryMethodManage _uid -> (ErrorID "authz:user:notification-delivery-method-manage", err403 {errBody = "Permission Denied: " <> msg})
    OrgPermission orgPermission ->
      case orgPermission of
        OrgRolesEdit _orgId -> (ErrorID "authz:org:roles-edit", err403 {errBody = "Permission Denied: " <> msg})
        OrgRolesList _orgId -> (ErrorID "authz:org:roles-list", err403 {errBody = "Permission Denied: " <> msg})
        OrgCreate _uid -> (ErrorID "authz:org:create", err403 {errBody = "Permission Denied: " <> msg})
        OrgMembersEdit _orgId -> (ErrorID "authz:org:members-edit", err403 {errBody = "Permission Denied: " <> msg})
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
    ProjectRolesList _pid -> "Not permitted to list maintainers"
    ProjectRolesEdit _pid -> "Not permitted to edit maintainers"
    AccessCausalHash _ _ -> "Not permitted to access this causal hash"
  UserPermission userPermission ->
    case userPermission of
      UserUpdate _uid -> "Not permitted to update this user"
      UserNotificationHubEntryView _uid -> "Not permitted to get notifications for this user"
      UserNotificationHubEntryUpdate _uid -> "Not permitted to update notifications for this user"
      UserNotificationSubscriptionView _uid -> "Not permitted to get notifications for this user"
      UserNotificationSubscriptionManage _uid -> "Not permitted to manage notifications for this user"
      UserNotificationDeliveryMethodView _uid -> "Not permitted to get notifications for this user"
      UserNotificationDeliveryMethodManage _uid -> "Not permitted to manage notifications for this user"
  OrgPermission orgPermission ->
    case orgPermission of
      OrgRolesEdit _orgId -> "Not permitted to edit roles in this org"
      OrgRolesList _orgId -> "Not permitted to list roles in this org"
      OrgCreate _uid -> "Not permitted to create an org for this user"
      OrgMembersEdit _orgId -> "Not permitted to edit members in this org"
  AdminPermission -> "Not permitted to access this resource"
  SudoPermission -> "Sudo mode required to perform this action. Please re-authenticate to enable sudo mode."

readPath :: Path -> CodebasePermission
readPath path = UserCodebaseReadPath (Path.toList path)

writePath :: Path -> CodebasePermission
writePath path = UserCodebaseWritePath (Path.toList path)

-- | Checks that A given causal is accessible from the other, useful for determining that a
-- rootHash is attached to a branch head that we can validate access to.
--
-- Throws a permission failure if the causal is not accessible.
assertCausalHashAccessibleFromRoot :: CausalId -> CausalId -> WebApp ()
assertCausalHashAccessibleFromRoot rootCausalId targetCausalId = permissionGuard $ maybePermissionFailure (ProjectPermission $ AccessCausalHash rootCausalId targetCausalId) do
  guardM . PG.runTransaction $ Q.causalIsInHistoryOf rootCausalId targetCausalId

-- | This is deprecated, permissions are all done at the project level now.
-- For back-compat, this simply checks whether the caller is the same as the target user.
checkReadUserCodebase :: Maybe UserId -> User -> Path -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReadUserCodebase mayRequestingUser (User {user_id = targetUserId}) (Path.toList -> path) = maybePermissionFailure (CodebasePermission $ UserCodebaseReadPath path) do
  reqUserId <- guardMaybe mayRequestingUser
  assertUsersEqual reqUserId targetUserId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Check that the caller is allowed to upload to the specified codebase.
checkUploadToProjectBranchCodebase ::
  -- | Requesting user
  UserId ->
  ProjectId ->
  -- | Contributor user Id
  Maybe UserId ->
  WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkUploadToProjectBranchCodebase reqUserId projectId mayContributorUserId = maybePermissionFailure (CodebasePermission CodebaseUpload) do
  case mayContributorUserId of
    -- If we're not namespaced to a contributor branch, we're uploading to a core branch and
    -- need maintain permissions.
    Nothing -> do
      assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
      pure $ AuthZ.UnsafeAuthZReceipt Nothing
    -- Only the contributor themself can upload to their contributor branches.
    -- We may wish to allow project maintainers to upload to contributor branches within their
    -- projects in the future.
    Just contributorId -> do
      assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) projectId
      guard (contributorId == reqUserId)
      pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkUserUpdate :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkUserUpdate reqUserId targetUserId = maybePermissionFailure (UserPermission (UserUpdate targetUserId)) $ do
  guard (reqUserId == targetUserId) <|> checkUserIsOrgMember reqUserId targetUserId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Check that the caller is allowed to upload to the specified codebase.
--
-- Note: This is DEPRECATED, and is only used for the legacy user codebase stuff.
-- It now only checks that the caller is the same as the target user.
checkUploadToUserCodebase ::
  -- | Requesting user
  UserId ->
  -- | Codebase owner user
  UserId ->
  WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkUploadToUserCodebase reqUserId codebaseOwnerUserId = maybePermissionFailure (CodebasePermission CodebaseUpload) do
  assertUsersEqual reqUserId codebaseOwnerUserId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | The download endpoint currently does all of its own auth using HashJWTs,
-- So we don't add any other authz checks here, the HashJWT check is sufficient.
checkDownloadFromUserCodebase :: WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkDownloadFromUserCodebase =
  pure . Right $ AuthZ.UnsafeAuthZReceipt Nothing

-- | The download endpoint currently does all of its own auth using HashJWTs,
-- So we don't add any other authz checks here, the HashJWT check is sufficient.
checkDownloadFromProjectBranchCodebase :: WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkDownloadFromProjectBranchCodebase =
  pure . Right $ AuthZ.UnsafeAuthZReceipt Nothing

checkProjectCreate :: Maybe UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkProjectCreate mayReqUserId targetUserId = maybePermissionFailure (ProjectPermission (ProjectCreate targetUserId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  -- Can create projects in their own user, or in any org they have permission to create projects in.
  guard (reqUserId == targetUserId) <|> checkCreateInOrg reqUserId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing
  where
    checkCreateInOrg userId = do
      Org {orgId} <- guardMaybeM $ PG.runTransaction $ OrgQ.orgByUserId targetUserId
      assertUserHasOrgPermission userId orgId AuthZ.OrgProjectCreate

checkProjectUpdate :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure ())
checkProjectUpdate mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ProjectUpdate targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) targetProjectId

checkProjectDelete :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure ())
checkProjectDelete mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ProjectDelete targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission AuthZ.ProjectDelete (Just reqUserId) targetProjectId

checkProjectGet :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkProjectGet reqUserId projectId = maybePermissionFailure (ProjectPermission (ProjectGet projectId)) $ do
  assertUserHasProjectPermission AuthZ.ProjectView reqUserId projectId
  case reqUserId of
    Nothing -> pure $ AuthZ.UnsafeAuthZReceipt Nothing
    Just _ -> pure $ AuthZ.UnsafeAuthZReceipt $ Just AuthZ.CachingToken

-- | Currently you can access any branch within a project you can access.
checkBranchGet :: Maybe UserId -> Branch causal -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkBranchGet mayCallerUserId (Branch {projectId, branchId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayCallerUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (BranchGet branchId projectId)

checkReleaseGet :: Maybe UserId -> Release causal v -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReleaseGet mayCallerUserId (Release {projectId, releaseId}) =
  mapLeft (const authzError) <$> do
    checkProjectGet mayCallerUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ReleaseGet releaseId projectId)

-- | Checks whether a user has access to create a core branch for the provided project, within
-- an optional contributor namespace.
checkBranchCreate :: UserId -> Project -> Maybe UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkBranchCreate reqUserId (Project {projectId = projectId}) mayContributorNamespace = maybePermissionFailure (ProjectPermission (BranchCreate projectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
    Just contributorUserId -> do
      assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) projectId
      -- For now we allow only the contributor to access their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Checks whether a user has access to delete a branch for the provided project
checkBranchDelete :: UserId -> Project -> Branch causal -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkBranchDelete reqUserId (Project {projectId = projectId}) Branch {contributorId = mayContributorNamespace} = maybePermissionFailure (ProjectPermission (BranchDelete projectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
    Just contributorUserId -> do
      assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) projectId
      -- For now we allow only the contributor to manage their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Checks whether a user has access to create a release for the provided project.
checkReleaseCreate :: UserId -> Project -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReleaseCreate reqUserId Project {projectId = targetProjectId} = maybePermissionFailure (ProjectPermission (ReleaseCreate targetProjectId)) $ do
  assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) targetProjectId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkReleaseUpdate :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReleaseUpdate mayReqUserId targetProjectId = maybePermissionFailure (ProjectPermission (ReleaseUpdate targetProjectId)) $ do
  reqUserId <- guardMaybe mayReqUserId
  assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) targetProjectId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Checks whether a user has access to set a core branch for the provided project to a
-- new Causal
checkBranchSet :: UserId -> Branch causal -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkBranchSet reqUserId Branch {projectId = targetProjectId, contributorId = mayContributorNamespace} = maybePermissionFailure (ProjectPermission (BranchSet targetProjectId)) $ do
  case mayContributorNamespace of
    Nothing -> assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) targetProjectId
    Just contributorUserId -> do
      assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) targetProjectId
      -- For now we allow only the contributor to access their branches. No project
      -- maintainers yet due to concerns with the swear-word problem.
      guard (contributorUserId == reqUserId)
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkProjectBranchRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkProjectBranchRead reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ (ProjectPermission (ProjectBranchBrowse projectId))

checkProjectBranchDiff :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkProjectBranchDiff reqUserId projectId =
  bimap (const authzError) makeCacheable <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ (ProjectPermission (ProjectBranchDiff projectId))

checkListBranchesForProject :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkListBranchesForProject reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ProjectBranchList projectId)

checkProjectReleaseRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkProjectReleaseRead reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ReleaseRead projectId)

checkReadProjectRolesList :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReadProjectRolesList reqUserId projectId =
  maybePermissionFailure (ProjectPermission (ProjectRolesList projectId)) $ do
    assertUserHasProjectPermission AuthZ.ProjectManage (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkAddProjectRoles :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkAddProjectRoles reqUserId projectId =
  maybePermissionFailure (ProjectPermission (ProjectRolesEdit projectId)) $ do
    -- In order to add new collaborators it must be a premium project.
    assertIsPremiumProject projectId
    assertUserHasProjectPermission AuthZ.ProjectManage (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkRemoveProjectRoles :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkRemoveProjectRoles reqUserId projectId =
  maybePermissionFailure (ProjectPermission (ProjectRolesEdit projectId)) $ do
    assertUserHasProjectPermission AuthZ.ProjectManage (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

assertIsPremiumProject :: ProjectId -> MaybeT WebApp ()
assertIsPremiumProject projectId = do
  guardM $ PG.runTransaction $ PQ.isPremiumProject projectId

checkListReleasesForProject :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkListReleasesForProject reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ProjectReleaseList projectId)

checkContributionListByProject :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionListByProject reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionList projectId)

checkContributionCreate :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionCreate reqUserId projectId =
  maybePermissionFailure (ProjectPermission (ContributionCreate projectId)) $ do
    assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkContributionUpdate :: UserId -> Contribution -> UpdateContributionRequest -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionUpdate reqUserId (Contribution {projectId, author}) UpdateContributionRequest {status = newStatus} =
  maybePermissionFailure (ProjectPermission (ContributionUpdate projectId)) $ do
    case newStatus of
      Just Merged -> do
        -- Only project maintainers can mark as merged
        assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
        pure $ AuthZ.UnsafeAuthZReceipt Nothing
      _ -> do
        guard (reqUserId == author) <|> assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
        pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkContributionMerge :: UserId -> Contribution -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionMerge reqUserId (Contribution {projectId}) =
  maybePermissionFailure (ProjectPermission (ContributionUpdate projectId)) $ do
    assertUserHasProjectPermission AuthZ.ProjectMaintain (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkContributionRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionRead mayReqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet mayReqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionRead projectId)

checkContributionDiffRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionDiffRead mayReqUserId projectId =
  bimap (const authzError) makeCacheable <$> checkProjectGet mayReqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionRead projectId)

checkContributionTimelineRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkContributionTimelineRead mayReqUserId projectId =
  mapLeft (const authzError) <$> do
    checkContributionRead mayReqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (ContributionTimelineGet projectId)

checkCommentCreate :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkCommentCreate userId projectId =
  mapLeft (const authzError) <$> do
    checkContributionRead (Just userId) projectId
  where
    authzError = AuthZFailure $ ProjectPermission (CommentCreate projectId)

checkCommentUpdate :: UserId -> Comment UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkCommentUpdate userId (Comment {actor = authorUserId, commentId}) =
  if (userId == authorUserId)
    then pure $ Right $ AuthZ.UnsafeAuthZReceipt Nothing
    else pure $ Left authzError
  where
    authzError = AuthZFailure $ ProjectPermission (CommentUpdate commentId)

-- TODO: Allow project maintainers to delete comments
checkCommentDelete :: UserId -> Comment UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkCommentDelete userId (Comment {actor = authorUserId, commentId}) =
  if (userId == authorUserId)
    then pure $ Right $ AuthZ.UnsafeAuthZReceipt Nothing
    else pure $ Left authzError
  where
    authzError = AuthZFailure $ ProjectPermission (CommentDelete commentId)

checkTicketListByProject :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkTicketListByProject reqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet reqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (TicketList projectId)

checkTicketCreate :: UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkTicketCreate reqUserId projectId =
  maybePermissionFailure (ProjectPermission (TicketCreate projectId)) $ do
    assertUserHasProjectPermission AuthZ.ProjectContribute (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkTicketUpdate :: UserId -> Ticket -> UpdateTicketRequest -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkTicketUpdate reqUserId (Ticket {projectId, author}) UpdateTicketRequest {} =
  maybePermissionFailure (ProjectPermission (TicketUpdate projectId)) $ do
    guard (reqUserId == author) <|> assertUserHasProjectPermission (AuthZ.ProjectMaintain) (Just reqUserId) projectId
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkTicketRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkTicketRead mayReqUserId projectId =
  mapLeft (const authzError) <$> do
    checkProjectGet mayReqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (TicketRead projectId)

checkTicketTimelineRead :: Maybe UserId -> ProjectId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkTicketTimelineRead mayReqUserId projectId =
  mapLeft (const authzError) <$> do
    checkTicketRead mayReqUserId projectId
  where
    authzError = AuthZFailure $ ProjectPermission (TicketTimelineGet projectId)

assertUserHasOrgPermission :: UserId -> OrgId -> AuthZ.RolePermission -> MaybeT WebApp ()
assertUserHasOrgPermission reqUserId orgId rolePermission =
  guardM . lift . PG.runTransaction $ Q.userHasOrgPermission reqUserId orgId rolePermission

assertUserHasOrgPermissionByOrgUser :: UserId -> UserId -> AuthZ.RolePermission -> MaybeT WebApp ()
assertUserHasOrgPermissionByOrgUser reqUserId orgUserId rolePermission = do
  Org {orgId} <- guardMaybeM $ PG.runTransaction $ OrgQ.orgByUserId orgUserId
  assertUserHasOrgPermission reqUserId orgId rolePermission

assertUserHasProjectPermission :: AuthZ.RolePermission -> Maybe UserId -> ProjectId -> MaybeT WebApp ()
assertUserHasProjectPermission rolePermission mayReqUserId projId = do
  guardM $ PG.runTransaction $ do
    Q.userHasProjectPermission mayReqUserId projId rolePermission

maybePermissionFailure :: WrapperPermissions -> MaybeT WebApp a -> WebApp (Either AuthZFailure a)
maybePermissionFailure perm m = do
  runMaybeT m >>= \case
    Nothing -> pure (Left (AuthZFailure perm))
    Just a -> pure (Right a)

-- | Check whether the given user has administrative privileges
checkUserIsSuperadmin :: UserId -> WebApp (Either AuthZFailure ())
checkUserIsSuperadmin userId = do
  PG.runTransaction $
    Q.isSuperadmin userId <&> \case
      False -> Left (AuthZFailure AdminPermission)
      True -> Right ()

checkUserIsOrgMember :: UserId -> UserId -> MaybeT WebApp ()
checkUserIsOrgMember reqUserId orgUserId = do
  guardM $
    PG.runTransaction $
      Q.isOrgMember reqUserId orgUserId

checkCreateOrg :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkCreateOrg reqUserId ownerUserId = maybePermissionFailure (OrgPermission (OrgCreate ownerUserId)) $ do
  guardM . PG.runTransaction $ Q.isSuperadmin reqUserId
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkReadOrgRolesList :: UserId -> OrgId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReadOrgRolesList reqUserId orgId =
  maybePermissionFailure (OrgPermission (OrgRolesList orgId)) $ do
    assertUserHasOrgPermission reqUserId orgId AuthZ.OrgAdmin
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkEditOrgRoles :: UserId -> OrgId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkEditOrgRoles reqUserId orgId =
  maybePermissionFailure (OrgPermission (OrgRolesEdit orgId)) $ do
    assertUserHasOrgPermission reqUserId orgId AuthZ.OrgAdmin
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkReadOrgMembers :: UserId -> OrgId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkReadOrgMembers _reqUserId _orgId = do
  -- Currently all org members lists are public
  pure . Right $ AuthZ.UnsafeAuthZReceipt Nothing

checkEditOrgMembers :: UserId -> OrgId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkEditOrgMembers reqUserId orgId = do
  maybePermissionFailure (OrgPermission (OrgMembersEdit orgId)) $ do
    assertUserHasOrgPermission reqUserId orgId AuthZ.OrgManage
    pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkNotificationsGet :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkNotificationsGet caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationHubEntryView notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.NotificationHubEntryView
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkNotificationsUpdate :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkNotificationsUpdate caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationHubEntryUpdate notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.OrgProjectCreate
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkDeliveryMethodsView :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkDeliveryMethodsView caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationDeliveryMethodView notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.NotificationHubEntryView
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkDeliveryMethodsManage :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkDeliveryMethodsManage caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationDeliveryMethodManage notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.NotificationHubEntryUpdate
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkSubscriptionsView :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkSubscriptionsView caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationSubscriptionView notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.NotificationSubscriptionView
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

checkSubscriptionsManage :: UserId -> UserId -> WebApp (Either AuthZFailure AuthZ.AuthZReceipt)
checkSubscriptionsManage caller notificationUser = maybePermissionFailure (UserPermission $ UserNotificationSubscriptionManage notificationUser) do
  assertUsersEqual caller notificationUser <|> assertUserHasOrgPermissionByOrgUser caller notificationUser AuthZ.NotificationSubscriptionManage
  pure $ AuthZ.UnsafeAuthZReceipt Nothing

-- | Check whether the given user has administrative privileges,
-- and has a recently created session. This adds additional protection to
-- sensitive endpoints.
checkAdminSudo :: Session.Session -> WebApp (Either AuthZFailure ())
checkAdminSudo Session.Session {sessionUserId, sessionCreated} = runExceptT $ do
  now <- liftIO Time.getCurrentTime
  ExceptT $ checkUserIsSuperadmin sessionUserId
  ExceptT $
    if Time.diffUTCTime now sessionCreated < sudoTTL
      then pure (Right ())
      else pure (Left (AuthZFailure SudoPermission))
  where
    sudoTTL = 60 * 60 * 2 -- 2 hours

-- | Get an auth token for admin endpoints.
-- Only use this after already checking administrative auth.
adminOverride :: AuthZ.AuthZReceipt
adminOverride = AuthZ.UnsafeAuthZReceipt Nothing

-- | Get an auth token for migrations.
migrationOverride :: AuthZ.AuthZReceipt
migrationOverride = AuthZ.UnsafeAuthZReceipt Nothing

-- | An auth token for a user we just created.
-- Only use this during a user creation flow.
userCreationOverride :: AuthZ.AuthZReceipt
userCreationOverride = AuthZ.UnsafeAuthZReceipt Nothing

backgroundJobAuthZ :: Background AuthZ.AuthZReceipt
backgroundJobAuthZ = pure $ AuthZ.UnsafeAuthZReceipt Nothing

permissionGuard :: WebApp (Either AuthZFailure a) -> WebApp a
permissionGuard m =
  m >>= \case
    Right a -> pure a
    Left err -> Errors.respondError err

-- | Make an auth receipt cacheable.
-- useful when we're re-using an existing auth receipt, but know that the current endpoint is
-- cacheable for authed users even if the original isn't.
makeCacheable :: AuthZ.AuthZReceipt -> AuthZ.AuthZReceipt
makeCacheable (AuthZ.UnsafeAuthZReceipt _) = AuthZ.UnsafeAuthZReceipt (Just AuthZ.CachingToken)

-- | Helper for checking deprecated User Codebase permissions.
-- It mostly serves as a marker of places in code that can be cleaned up once user codebase
-- stuff is sunset.
assertUsersEqual :: UserId -> UserId -> MaybeT WebApp ()
assertUsersEqual reqUserId targetUserId = do
  guard $ reqUserId == targetUserId
