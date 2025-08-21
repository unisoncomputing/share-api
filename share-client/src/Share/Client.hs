module Share.Client (createOrg) where

import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client
import Share.Web.API (API, OrgsAPI, api)
import Share.Web.Share.DisplayInfo.Types qualified as Orgs
import Share.Web.Share.Orgs.API qualified as OrgsAPI
import Share.Web.Share.Orgs.Types qualified as Orgs

orgsClient :: Client ClientM OrgsAPI
orgsClient = client (Proxy :: Proxy OrgsAPI)

createOrg :: _ -> Orgs.CreateOrgRequest -> ClientM Orgs.OrgDisplayInfo
(createOrg :<|> OrgsAPI.ResourceRoutes {OrgsAPI.roles = OrgsAPI.OrgRolesRoutes {}, OrgsAPI.members = OrgsAPI.OrgMembersRoutes {}}) = orgsClient
