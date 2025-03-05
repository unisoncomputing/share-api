module Share.Web.Share.Orgs.Impl (server) where

import Servant
import Servant.Server.Generic
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Authorization.Types
import Share.Web.Errors
import Share.Web.Share.Orgs.API as API
import Share.Web.Share.Orgs.Queries qualified as OrgQ
import Share.Web.Share.Orgs.Types (Org (..))

server :: ServerT API.API WebApp
server orgHandle =
  API.Routes
    { API.roles = rolesServer orgHandle
    }

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
  ListRolesResponse True <$> PG.runTransaction (OrgQ.listOrgRoles orgId)

addRolesEndpoint :: UserHandle -> UserId -> AddRolesRequest -> WebApp ListRolesResponse
addRolesEndpoint orgHandle caller (AddRolesRequest {roles}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgRoles caller orgId
  ListRolesResponse True <$> PG.runTransaction (OrgQ.addOrgRoles orgId roles)

removeRolesEndpoint :: UserHandle -> UserId -> RemoveRolesRequest -> WebApp ListRolesResponse
removeRolesEndpoint orgHandle caller (RemoveRolesRequest {roles}) = do
  orgId <- orgIdByHandle orgHandle
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkEditOrgRoles caller orgId
  ListRolesResponse True <$> PG.runTransaction (OrgQ.removeOrgRoles orgId roles)
