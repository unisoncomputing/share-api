{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.API where

import Servant
import Share.IDs
import Share.OAuth.Session (AuthenticatedSession, AuthenticatedUserId, MaybeAuthenticatedSession)
import Share.Prelude (NonEmpty)
import Share.Utils.API
import Share.Utils.Caching
import Share.Utils.Servant
import Share.Web.Share.Branches.API (UserBranchesAPI)
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Contributions.API (ContributionsByUserAPI)
import Share.Web.Share.Projects.API (ProjectsAPI)
import Share.Web.Share.Types

type UserAPI =
  MaybeAuthenticatedSession
    :> Capture "user_handle" UserHandle
    :> UserResourceAPI

type UserResourceAPI =
  ("readme" :> UserReadmeEndpoint)
    :<|> UserProfileEndpoint
    :<|> UpdateUserEndpoint
    :<|> ("projects" :> ProjectsAPI)
    :<|> ("branches" :> UserBranchesAPI)
    :<|> ("contributions" :> ContributionsByUserAPI)

-- | PATCH /users/:user_handle
-- Update the user's profile
type UpdateUserEndpoint =
  AuthenticatedUserId
    :> ReqBody '[JSON] UpdateUserRequest
    :> Patch '[JSON] DescribeUserProfile

-- | GET /users/:user_handle
type UserProfileEndpoint = Get '[JSON] DescribeUserProfile

-- | GET /users/:user_handle/readme
type UserReadmeEndpoint = Get '[JSON] (Cached JSON ReadmeResponse)

-- | GET /search?query=hoj&limit=9
--
-- Search users by a prefix of their name or handle.
type SearchEndpoint =
  MaybeAuthenticatedSession
    :> RequiredQueryParam "query" Query
    :> QueryParam "limit" Limit
    :> Get '[JSON] [SearchResult]

-- | Search for names to use in a definition search.
type SearchDefinitionNamesEndpoint =
  MaybeAuthenticatedSession
    :> RequiredQueryParam "query" Query
    :> QueryParam "limit" Limit
    :> QueryParam "user-filter" UserHandle
    :> QueryParam "project-filter" ProjectShortHand
    :> QueryParam "release-filter" ReleaseVersion
    :> Get '[JSON] [DefinitionNameSearchResult]

-- | Submit a definition search
type SearchDefinitionsEndpoint =
  MaybeAuthenticatedSession
    :> RequiredQueryParam "query" Query
    :> QueryParam "limit" Limit
    :> QueryParam "user-filter" UserHandle
    :> QueryParam "project-filter" ProjectShortHand
    :> Get '[JSON] [DefinitionSearchResult]

type AccountAPI =
  AuthenticatedSession
    :> ( AccountEndpoint
           :<|> ("tours" :> CompleteToursEndpoint)
       )

-- | GET /account
type AccountEndpoint = Get '[JSON] UserAccountInfo

-- | POST /account/tours
type CompleteToursEndpoint =
  ReqBody '[JSON] (NonEmpty TourId)
    :> Post '[JSON] NoContent

type UserPublicCodebaseAPI =
  MaybeAuthenticatedSession
    :> Capture "user_handle" UserHandle
    :> CodeBrowseAPI
