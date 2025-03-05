{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.API (API, Routes (..), OrgRolesRoutes (..)) where

import GHC.Generics (Generic)
import Servant
import Share.IDs
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Web.Authorization.Types (AddRolesRequest, ListRolesResponse, RemoveRolesRequest)

type API = Capture "orgHandle" UserHandle :> NamedRoutes Routes

data Routes mode
  = Routes
  { roles :: mode :- "roles" :> NamedRoutes OrgRolesRoutes
  }
  deriving stock (Generic)

data OrgRolesRoutes mode
  = OrgRolesRoutes
  { list :: mode :- OrgRolesListEndpoint,
    add :: mode :- "add" :> OrgRolesAddEndpoint,
    remove :: mode :- "remove" :> OrgRolesRemoveEndpoint
  }
  deriving stock (Generic)

type OrgRolesAddEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] AddRolesRequest
    :> Post '[JSON] ListRolesResponse

type OrgRolesRemoveEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] RemoveRolesRequest
    :> Post '[JSON] ListRolesResponse

type OrgRolesListEndpoint =
  AuthenticatedUserId
    :> Get '[JSON] ListRolesResponse
