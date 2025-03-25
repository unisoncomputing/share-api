module Share.Web.Share.Orgs.Impl (server) where

import Control.Lens
import Data.Either (isRight)
import Servant
import Servant.Server.Generic
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.User (User (..))
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Authorization.Types
import Share.Web.Errors
import Share.Web.Share.DisplayInfo (OrgDisplayInfo)
import Share.Web.Share.Orgs.API as API
import Share.Web.Share.Orgs.Operations qualified as OrgOps
import Share.Web.Share.Orgs.Queries qualified as OrgQ
import Share.Web.Share.Orgs.Types (CreateOrgRequest (..), Org (..))
import Share.Web.Share.Roles (canonicalRoleAssignmentOrdering)
import Share.Web.Share.Roles.Queries (displaySubjectsOf)

server :: ServerT API.API WebApp
server =
  let orgResourceServer orgHandle = API.ResourceRoutes {API.roles = rolesServer orgHandle}
   in orgCreateEndpoint :<|> orgResourceServer

orgCreateEndpoint :: UserId -> CreateOrgRequest -> WebApp OrgDisplayInfo
orgCreateEndpoint callerUserId (CreateOrgRequest {name, handle, avatarUrl, email, owner = ownerHandle}) = do
  User {user_id = ownerUserId} <- PG.runTransaction (UserQ.userByHandle ownerHandle) `whenNothingM` respondError (EntityMissing (ErrorID "missing-user") "Owner not found")
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkCreateOrg callerUserId ownerUserId
  orgId <- PG.runTransactionOrRespondError $ OrgOps.createOrg authZReceipt name handle email avatarUrl ownerUserId
  PG.runTransaction $ OrgQ.orgDisplayInfoOf id orgId

rolesServer :: UserHandle -> API.OrgRolesRoutes (AsServerT WebApp)
rolesServer orgHandle =
  API.OrgRolesRoutes
    { API.list = listRolesEndpoint orgHandle,
      API.add = addRolesEndpoint orgHandle,
      API.remove = removeRolesEndpoint orgHandle
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
  PG.runTransaction do
    orgRoles <- OrgQ.addOrgRoles orgId roleAssignments
    ListRolesResponse True . canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) orgRoles

removeRolesEndpoint :: UserHandle -> UserId -> RemoveRolesRequest -> WebApp ListRolesResponse
removeRolesEndpoint orgHandle caller (RemoveRolesRequest {roleAssignments}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgRoles caller orgId
  PG.runTransaction do
    orgRoles <- OrgQ.removeOrgRoles orgId roleAssignments
    ListRolesResponse True . canonicalRoleAssignmentOrdering <$> displaySubjectsOf (traversed . traversed) orgRoles
