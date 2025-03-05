module Share.Web.Share.Orgs.Impl (server) where

import Servant
import Servant.Server.Generic
import Share.IDs
import Share.Web.App
import Share.Web.Authorization.Types
import Share.Web.Share.Orgs.API as API

server :: ServerT API.API WebApp
server orgId =
  API.Routes
    { API.roles = rolesServer orgId
    }

rolesServer :: OrgId -> API.OrgRolesRoutes (AsServerT WebApp)
rolesServer orgId =
  API.OrgRolesRoutes
    { API.list = listRolesEndpoint orgId,
      API.add = addRolesEndpoint orgId,
      API.remove = removeRolesEndpoint orgId
    }

listRolesEndpoint :: OrgId -> WebApp [RoleAssignment ResolvedAuthSubject]
listRolesEndpoint orgId = do
  -- Implementation here
  pure []

addRolesEndpoint :: OrgId -> [RoleAssignment ResolvedAuthSubject] -> WebApp [RoleAssignment ResolvedAuthSubject]
addRolesEndpoint orgId roles = do
  -- Implementation here
  pure roles

removeRolesEndpoint :: OrgId -> [RoleAssignment ResolvedAuthSubject] -> WebApp [RoleAssignment ResolvedAuthSubject]
removeRolesEndpoint orgId roles = do
  -- Implementation here
  pure roles
