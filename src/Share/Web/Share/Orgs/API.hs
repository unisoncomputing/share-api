{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.API
  ( API,
    ResourceRoutes (..),
    OrgMembersRoutes (..),
  )
where

import GHC.Generics (Generic)
import Servant
import Share.IDs
import Share.OAuth.Session (AuthenticatedUserId)
import Share.Web.Share.DisplayInfo.Types (OrgDisplayInfo)
import Share.Web.Share.Orgs.Types

type API =
  CreateOrgEndpoint
    :<|> ( Capture "orgHandle" UserHandle :> NamedRoutes ResourceRoutes
         )

data ResourceRoutes mode
  = ResourceRoutes
  { orgMembers :: mode :- "members" :> NamedRoutes OrgMembersRoutes
  }
  deriving stock (Generic)

data OrgMembersRoutes mode
  = OrgMembersRoutes
  { listOrgMembers :: mode :- OrgMembersListEndpoint,
    addOrgMembers :: mode :- OrgMembersAddEndpoint,
    removeOrgMembers :: mode :- OrgMembersRemoveEndpoint
  }
  deriving stock (Generic)

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
