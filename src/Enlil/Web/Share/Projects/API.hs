{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.Projects.API where

import Enlil.IDs
import Enlil.OAuth.Session (MaybeAuthenticatedSession)
import Enlil.Utils.Caching (Cached)
import Enlil.Utils.Servant (RequiredQueryParam)
import Enlil.Web.Share.Branches.API (ProjectBranchesAPI)
import Enlil.Web.Share.Contributions.API (ContributionsByProjectAPI)
import Enlil.Web.Share.Diffs.Types (ShareNamespaceDiffResponse)
import Enlil.Web.Share.Projects.Types
import Enlil.Web.Share.Releases.API
import Enlil.Web.Share.Tickets.API (TicketsByProjectAPI)
import Enlil.Web.Share.Types
import Servant

type ProjectsAPI =
  ( ListProjectsForUserEndpoint
      :<|> (Capture "project_slug" ProjectSlug :> ProjectResourceAPI)
  )

type ProjectResourceAPI =
  ( ("readme" :> ProjectReadmeEndpoint)
      :<|> ("branches" :> ProjectBranchesAPI)
      :<|> ("releases" :> ProjectReleasesAPI)
      :<|> ("contributions" :> ContributionsByProjectAPI)
      :<|> ("tickets" :> TicketsByProjectAPI)
      :<|> ("diff" :> "namespaces" :> ProjectDiffNamespaceEndpoint)
      :<|> CreateProjectEndpoint
      :<|> UpdateProjectEndpoint
      :<|> DeleteProjectEndpoint
      :<|> GetProjectEndpoint
      :<|> ( "fav" :> FavProjectEndpoint
           )
      :<|> "maintainers" :> MaintainersResourceAPI
  )

type ProjectDiffNamespaceEndpoint =
  RequiredQueryParam "old" BranchOrReleaseShortHand
    :> RequiredQueryParam "new" BranchOrReleaseShortHand
    :> Get '[JSON] (Cached JSON ShareNamespaceDiffResponse)

type CreateProjectEndpoint =
  ReqBody '[JSON] CreateProjectRequest
    :> Post '[JSON] CreateProjectResponse

type UpdateProjectEndpoint =
  ReqBody '[JSON] UpdateProjectRequest
    :> Patch '[JSON] ()

type DeleteProjectEndpoint =
  Delete '[JSON] ()

type GetProjectEndpoint = Get '[JSON] GetProjectResponse

type ListProjectsForUserEndpoint = Get '[JSON] ListProjectsResponse

type FavProjectEndpoint =
  ReqBody '[JSON] FavProjectRequest
    :> Put '[JSON] NoContent

type CatalogAPI =
  MaybeAuthenticatedSession :> ProjectCatalogEndpoint

type ProjectCatalogEndpoint = Get '[JSON] [CatalogCategory]

type ProjectReadmeEndpoint = Get '[JSON] (Cached JSON ReadmeResponse)

type MaintainersResourceAPI =
  ( ListMaintainersEndpoint
      :<|> AddMaintainersEndpoint
      :<|> UpdateMaintainersEndpoint
  )

-- | List all maintainers of the project.
type ListMaintainersEndpoint = Get '[JSON] ListMaintainersResponse

-- | Add new maintainers to the project.
type AddMaintainersEndpoint =
  ReqBody '[JSON] AddMaintainersRequest
    :>
    -- Return the updated list of maintainers
    Post '[JSON] AddMaintainersResponse

-- | For each listed maintainer, update their permissions.
-- Note: This does NOT affect any maintainers which are not specified and does NOT
-- remove any maintainers which are not specified.
type UpdateMaintainersEndpoint =
  ReqBody '[JSON] UpdateMaintainersRequest
    :>
    -- Return the updated list of maintainers
    Patch '[JSON] UpdateMaintainersResponse
