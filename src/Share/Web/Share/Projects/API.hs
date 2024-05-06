{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Projects.API where

import Share.IDs
import Share.OAuth.Session (MaybeAuthenticatedSession)
import Share.Utils.Caching (Cached)
import Share.Utils.Servant (RequiredQueryParam)
import Share.Web.Share.Branches.API (ProjectBranchesAPI)
import Share.Web.Share.Contributions.API (ContributionsByProjectAPI)
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse)
import Share.Web.Share.Projects.Types
import Share.Web.Share.Releases.API
import Share.Web.Share.Tickets.API (TicketsByProjectAPI)
import Share.Web.Share.Types
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
