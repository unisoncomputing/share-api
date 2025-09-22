{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Share.Client.Orgs
  ( createOrg,
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

orgMembersRoutes :: UserHandle -> Client ClientM (NamedRoutes OrgsAPI.OrgMembersRoutes)
orgMembersRoutes = OrgsAPI.orgMembers <$> resourceRoutes

listOrgMembers :: JWT.SignedJWT -> UserHandle -> ClientM OrgMembersListResponse
listOrgMembers jwt userHandle = OrgsAPI.listOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt)

addOrgMembers :: JWT.SignedJWT -> UserHandle -> OrgMembersAddRequest -> ClientM OrgMembersListResponse
addOrgMembers jwt userHandle addMembersReq =
  OrgsAPI.addOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) addMembersReq

removeOrgMembers :: JWT.SignedJWT -> UserHandle -> OrgMembersRemoveRequest -> ClientM OrgMembersListResponse
removeOrgMembers jwt userHandle removeMembersReq =
  OrgsAPI.removeOrgMembers (orgMembersRoutes userHandle) (jwtToAuthenticatedRequest jwt) removeMembersReq

createOrg :: JWT.SignedJWT -> CreateOrgRequest -> ClientM OrgDisplayInfo
createOrg jwt createOrgReq =
  createOrg' (jwtToAuthenticatedRequest jwt) createOrgReq

createOrg' :: AuthenticatedRequest AuthenticatedUserId -> CreateOrgRequest -> ClientM OrgDisplayInfo
resourceRoutes :: UserHandle -> OrgsAPI.ResourceRoutes (AsClientT ClientM)
(createOrg' :<|> resourceRoutes) = orgsClient
