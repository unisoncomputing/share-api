{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Share.Client
  ( createOrg,
    addOrgRoles,
    listOrgRoles,
    removeOrgRoles,
    listOrgMembers,
    addOrgMembers,
    removeOrgMembers,
  )
where

import Crypto.JWT qualified as JWT
import Data.Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Client.Core qualified as Client
import Share.IDs (UserHandle)
import Share.JWT qualified as ShareJWT
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Web.API (OrgsAPI)
import Share.Web.Authorization.Types
import Share.Web.Authorization.Types qualified as OrgsAPI
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Orgs.API qualified as OrgsAPI
import Share.Web.Share.Orgs.Types

jwtToAuthenticatedRequest :: (AuthClientData a ~ JWT.SignedJWT) => JWT.SignedJWT -> AuthenticatedRequest a
jwtToAuthenticatedRequest jwt =
  mkAuthenticatedRequest jwt addJWTHeader
  where
    addJWTHeader :: JWT.SignedJWT -> Request -> Request
    addJWTHeader jwt req =
      case List.lookup "Authorization" (toList $ Client.requestHeaders req) of
        Nothing ->
          req
            & Client.addHeader "Authorization" ("Bearer " <> ShareJWT.signedJWTToText jwt)
        Just _ -> req

orgsClient :: Client ClientM OrgsAPI
orgsClient = client (Proxy :: Proxy OrgsAPI)

resourceRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.ResourceRoutes)
orgRolesRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.OrgRolesRoutes)
orgRolesRoutes = OrgsAPI.orgRoles <$> resourceRoutes

orgMembersRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.OrgMembersRoutes)
orgMembersRoutes = OrgsAPI.orgMembers <$> resourceRoutes

listOrgRoles :: UserHandle -> JWT.SignedJWT -> ClientM ListRolesResponse
listOrgRoles userHandle jwt = OrgsAPI.listOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt)

removeOrgRoles :: UserHandle -> JWT.SignedJWT -> OrgsAPI.RemoveRolesRequest -> ClientM ListRolesResponse
removeOrgRoles userHandle jwt removeRolesReq =
  OrgsAPI.removeOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt) removeRolesReq

listOrgMembers :: UserHandle -> JWT.SignedJWT -> ClientM OrgMembersListResponse
listOrgMembers userHandle jwt = OrgsAPI.listOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt)

addOrgMembers :: UserHandle -> JWT.SignedJWT -> OrgMembersAddRequest -> ClientM OrgMembersListResponse
addOrgMembers userHandle jwt addMembersReq =
  OrgsAPI.addOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) addMembersReq

removeOrgMembers :: UserHandle -> JWT.SignedJWT -> OrgMembersRemoveRequest -> ClientM OrgMembersListResponse
removeOrgMembers userHandle jwt removeMembersReq =
  OrgsAPI.removeOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) removeMembersReq

addOrgRoles :: UserHandle -> JWT.SignedJWT -> OrgsAPI.AddRolesRequest -> ClientM ListRolesResponse
addOrgRoles userHandle jwt addRolesReq =
  OrgsAPI.addOrgRoles (orgRolesRoutes userHandle) (jwtToAuthenticatedRequest jwt) addRolesReq

createOrg :: JWT.SignedJWT -> CreateOrgRequest -> ClientM OrgDisplayInfo
createOrg jwt createOrgReq =
  createOrg' (jwtToAuthenticatedRequest jwt) createOrgReq

createOrg' :: AuthenticatedRequest AuthenticatedUserId -> CreateOrgRequest -> ClientM OrgDisplayInfo
(createOrg' :<|> resourceRoutes) = orgsClient
