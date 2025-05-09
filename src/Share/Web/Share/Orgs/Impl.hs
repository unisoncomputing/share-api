{-# LANGUAGE DataKinds #-}

module Share.Web.Share.Orgs.Impl (server) where

import Control.Lens
import Data.Either (isRight)
import Data.Set qualified as Set
import Servant
import Servant.Server.Generic
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.User (User (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Authorization.Types
import Share.Web.Errors
import Share.Web.Share.DisplayInfo (OrgDisplayInfo)
import Share.Web.Share.Orgs.API as API
import Share.Web.Share.Orgs.Operations qualified as OrgOps
import Share.Web.Share.Orgs.Queries qualified as OrgQ
import Share.Web.Share.Orgs.Types (CreateOrgRequest (..), Org (..), OrgMembersAddRequest (..), OrgMembersListResponse (..), OrgMembersRemoveRequest (..))
import Share.Web.Share.Roles (canonicalRoleAssignmentOrdering)
import Share.Web.Share.Roles.Queries (displaySubjectsOf)
import Unison.Util.Set qualified as Set

data OrgError
  = OrgMemberOfOrgError
  | OrgMustHaveOwnerError
  deriving stock (Show, Eq)
  deriving (Logging.Loggable) via (Logging.ShowLoggable Logging.UserFault OrgError)

instance ToServerError OrgError where
  toServerError = \case
    OrgMemberOfOrgError ->
      ( ErrorID "org:org-member-of-org",
        err400
          { errBody = "Cannot add an org as a member of another org.",
            errReasonPhrase = "Invalid Org Member"
          }
      )
    OrgMustHaveOwnerError ->
      ( ErrorID "org:must-have-owner",
        err400
          { errBody = "Cannot remove the only owner of an org.",
            errReasonPhrase = "Invalid Org Member"
          }
      )

server :: ServerT API.API WebApp
server =
  let orgResourceServer orgHandle =
        API.ResourceRoutes
          { API.roles = rolesServer orgHandle,
            API.members = membersServer orgHandle
          }
   in orgCreateEndpoint :<|> orgResourceServer

orgCreateEndpoint :: UserId -> CreateOrgRequest -> WebApp OrgDisplayInfo
orgCreateEndpoint callerUserId (CreateOrgRequest {name, handle, avatarUrl, email, owner = ownerHandle, isCommercial}) = do
  User {user_id = ownerUserId} <- PG.runTransaction (UserQ.userByHandle ownerHandle) `whenNothingM` respondError (EntityMissing (ErrorID "missing-user") "Owner not found")
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkCreateOrg callerUserId ownerUserId
  orgId <- PG.runTransactionOrRespondError $ OrgOps.createOrg authZReceipt name handle email avatarUrl ownerUserId callerUserId isCommercial
  PG.runTransaction $ OrgQ.orgDisplayInfoOf id orgId

rolesServer :: UserHandle -> API.OrgRolesRoutes (AsServerT WebApp)
rolesServer orgHandle =
  API.OrgRolesRoutes
    { API.list = listRolesEndpoint orgHandle,
      API.add = addRolesEndpoint orgHandle,
      API.remove = removeRolesEndpoint orgHandle
    }

membersServer :: UserHandle -> API.OrgMembersRoutes (AsServerT WebApp)
membersServer orgHandle =
  API.OrgMembersRoutes
    { API.list = listMembersEndpoint orgHandle,
      API.add = addMembersEndpoint orgHandle,
      API.remove = removeMembersEndpoint orgHandle
    }

orgIdByHandle :: UserHandle -> WebApp OrgId
orgIdByHandle orgHandle = do
  Org {orgId} <- do
    whenNothingM (PG.runTransaction $ OrgQ.orgByUserHandle orgHandle) $
      respondError (EntityMissing (ErrorID "missing-org") "Organization not found")
  pure orgId

listRolesEndpoint :: UserHandle -> UserId -> WebApp ListRolesResponse
listRolesEndpoint orgHandle caller = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadOrgRolesList caller orgId
  callerCanEdit <- isRight <$> AuthZ.checkEditOrgRoles caller orgId
  PG.runTransaction do
    orgRoles <- OrgQ.listOrgRoles orgId
    ListRolesResponse callerCanEdit . canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) orgRoles

addRolesEndpoint :: UserHandle -> UserId -> AddRolesRequest -> WebApp ListRolesResponse
addRolesEndpoint orgHandle caller (AddRolesRequest {roleAssignments}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgRoles caller orgId
  assertNoOrgSubjects roleAssignments
  PG.runTransaction do
    orgRoles <- OrgQ.addOrgRoles orgId roleAssignments
    ListRolesResponse True . canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) orgRoles

removeRolesEndpoint :: UserHandle -> UserId -> RemoveRolesRequest -> WebApp ListRolesResponse
removeRolesEndpoint orgHandle caller (RemoveRolesRequest {roleAssignments}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgRoles caller orgId
  PG.runTransactionOrRespondError do
    orgRoles <- OrgQ.removeOrgRoles orgId roleAssignments
    OrgQ.doesOrgHaveOwner orgId >>= \case
      False -> throwError OrgMustHaveOwnerError
      True -> pure ()

    ListRolesResponse True . canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) orgRoles

listMembersEndpoint :: UserHandle -> UserId -> WebApp OrgMembersListResponse
listMembersEndpoint orgHandle caller = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadOrgMembers caller orgId
  PG.runTransaction do
    OrgMembersListResponse <$> OrgQ.listOrgMembers orgId

addMembersEndpoint :: UserHandle -> UserId -> OrgMembersAddRequest -> WebApp OrgMembersListResponse
addMembersEndpoint orgHandle caller (OrgMembersAddRequest {members}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgMembers caller orgId
  PG.runTransactionOrRespondError do
    userIds <- UserQ.userIdsByHandlesOf Set.traverse (Set.fromList members)
    hasOrgMember <- runMaybeT $ for_ userIds \userId -> do
      MaybeT $ OrgQ.orgByUserId userId
    when (isJust hasOrgMember) do
      throwError OrgMemberOfOrgError
    OrgQ.addOrgMembers orgId userIds
    OrgMembersListResponse <$> OrgQ.listOrgMembers orgId

removeMembersEndpoint :: UserHandle -> UserId -> OrgMembersRemoveRequest -> WebApp OrgMembersListResponse
removeMembersEndpoint orgHandle caller (OrgMembersRemoveRequest {members}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgMembers caller orgId
  PG.runTransaction do
    userIds <- UserQ.userIdsByHandlesOf Set.traverse (Set.fromList members)
    OrgQ.removeOrgMembers orgId userIds
    OrgMembersListResponse <$> OrgQ.listOrgMembers orgId

assertNoOrgSubjects :: [RoleAssignment ResolvedAuthSubject] -> WebApp ()
assertNoOrgSubjects roleAssignments = do
  let hasOrgSubject =
        roleAssignments
          & any
            ( \RoleAssignment {subject} -> case subject of
                OrgSubject {} -> True
                _ -> False
            )
  when hasOrgSubject do
    respondError OrgMemberOfOrgError
