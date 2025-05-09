{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.API
  ( API,
    ResourceRoutes (..),
    OrgRolesRoutes (..),
    OrgMembersRoutes (..),
  )
where

import GHC.Generics (Generic)
import Servant
import Share.IDs
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Web.Authorization.Types (AddRolesRequest, ListRolesResponse, RemoveRolesRequest)
import Share.Web.Share.DisplayInfo (OrgDisplayInfo)
import Share.Web.Share.Orgs.Types

type API =
  CreateOrgEndpoint
    :<|> ( Capture "orgHandle" UserHandle :> NamedRoutes ResourceRoutes
         )

data ResourceRoutes mode
  = ResourceRoutes
  { roles :: mode :- "roles" :> NamedRoutes OrgRolesRoutes,
    members :: mode :- "members" :> NamedRoutes OrgMembersRoutes
  }
  deriving stock (Generic)

data OrgRolesRoutes mode
  = OrgRolesRoutes
  { list :: mode :- OrgRolesListEndpoint,
    add :: mode :- OrgRolesAddEndpoint,
    remove :: mode :- OrgRolesRemoveEndpoint
  }
  deriving stock (Generic)

data OrgMembersRoutes mode
  = OrgMembersRoutes
  { list :: mode :- OrgMembersListEndpoint,
    add :: mode :- OrgMembersAddEndpoint,
    remove :: mode :- OrgMembersRemoveEndpoint
  }
  deriving stock (Generic)

type OrgRolesAddEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] AddRolesRequest
    :> Post '[JSON] ListRolesResponse

type OrgRolesRemoveEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] RemoveRolesRequest
    :> Delete '[JSON] ListRolesResponse

type OrgRolesListEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] ListRolesResponse

type CreateOrgEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] CreateOrgRequest
    :> Post '[JSON] OrgDisplayInfo

type OrgMembersAddEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] OrgMembersAddRequest
    :> Post '[JSON] OrgMembersListResponse

type OrgMembersRemoveEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] OrgMembersRemoveRequest
    :> Delete '[JSON] OrgMembersListResponse

type OrgMembersListEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] OrgMembersListResponse
