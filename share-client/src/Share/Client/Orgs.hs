{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Share.Client.Orgs
  ( createOrg,
    addOrgRoles,
    listOrgRoles,
    removeOrgRoles,
    listOrgMembers,
    addOrgMembers,
    removeOrgMembers,

    -- * Types
    module OrgsAPI,
    module Share.Web.Authorization.Types,
    module Share.IDs,
  )
where

import Crypto.JWT qualified as JWT
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client
import Servant.Client.Core
import Share.Client.Utils (jwtToAuthenticatedRequest)
import Share.IDs (UserHandle)
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Web.API (OrgsAPI)
import Share.Web.Authorization.Types
import Share.Web.Authorization.Types qualified as OrgsAPI
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Orgs.API qualified as OrgsAPI
import Share.Web.Share.Orgs.Types

orgsClient :: Client ClientM OrgsAPI
orgsClient = client (Proxy :: Proxy OrgsAPI)

resourceRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.ResourceRoutes)
orgRolesRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.OrgRolesRoutes)
orgRolesRoutes = OrgsAPI.orgRoles <$> resourceRoutes

orgMembersRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.OrgMembersRoutes)
orgMembersRoutes = OrgsAPI.orgMembers <$> resourceRoutes

listOrgRoles :: JWT.SignedJWT -> UserHandle -> ClientM ListRolesResponse
listOrgRoles jwt userHandle = OrgsAPI.listOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt)

removeOrgRoles :: JWT.SignedJWT -> UserHandle -> OrgsAPI.RemoveRolesRequest -> ClientM ListRolesResponse
removeOrgRoles jwt userHandle removeRolesReq =
  OrgsAPI.removeOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt) removeRolesReq

listOrgMembers :: JWT.SignedJWT -> UserHandle -> ClientM OrgMembersListResponse
listOrgMembers jwt userHandle = OrgsAPI.listOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt)

addOrgMembers :: JWT.SignedJWT -> UserHandle -> OrgMembersAddRequest -> ClientM OrgMembersListResponse
addOrgMembers jwt userHandle addMembersReq =
  OrgsAPI.addOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) addMembersReq

removeOrgMembers :: JWT.SignedJWT -> UserHandle -> OrgMembersRemoveRequest -> ClientM OrgMembersListResponse
removeOrgMembers jwt userHandle removeMembersReq =
  OrgsAPI.removeOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) removeMembersReq

addOrgRoles :: JWT.SignedJWT -> UserHandle -> OrgsAPI.AddRolesRequest -> ClientM ListRolesResponse
addOrgRoles jwt userHandle addRolesReq =
  OrgsAPI.addOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt) addRolesReq

createOrg :: JWT.SignedJWT -> CreateOrgRequest -> ClientM OrgDisplayInfo
createOrg jwt createOrgReq =
  createOrg' (jwtToAuthenticatedRequest jwt) createOrgReq

createOrg' :: AuthenticatedRequest AuthenticatedUserId -> CreateOrgRequest -> ClientM OrgDisplayInfo
(createOrg' :<|> resourceRoutes) = orgsClient
