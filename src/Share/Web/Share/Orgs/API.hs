{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.API (API, Routes (..), OrgRolesRoutes (..)) where

import GHC.Generics (Generic)
import Servant
import Share.IDs
import Share.Web.Authorization.Types (ResolvedAuthSubject, RoleAssignment)

type API = Capture "orgId" OrgId :> NamedRoutes Routes

data Routes mode
  = Routes
  { roles :: mode :- "roles" :> NamedRoutes OrgRolesRoutes
  }
  deriving stock (Generic)

data OrgRolesRoutes mode
  = OrgRolesRoutes
  { list :: mode :- OrgRolesListEndpoint,
    add :: mode :- OrgRolesAddEndpoint,
    remove :: mode :- OrgRolesRemoveEndpoint
  }
  deriving stock (Generic)

type OrgRolesAddEndpoint =
  ReqBody '[JSON] [RoleAssignment ResolvedAuthSubject]
    :> Post '[JSON] [RoleAssignment ResolvedAuthSubject]

type OrgRolesRemoveEndpoint =
  ReqBody '[JSON] [RoleAssignment ResolvedAuthSubject]
    :> Post '[JSON] [RoleAssignment ResolvedAuthSubject]

type OrgRolesListEndpoint =
  Get '[JSON] [RoleAssignment ResolvedAuthSubject]
