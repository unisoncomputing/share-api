{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.API where

import Enlil.IDs
import Enlil.OAuth.Session (AuthenticatedSession, AuthenticatedUserId, MaybeAuthenticatedSession)
import Enlil.Prelude (NonEmpty)
import Enlil.Utils.API
import Enlil.Utils.Caching
import Enlil.Utils.Servant
import Enlil.Web.Share.Branches.API (UserBranchesAPI)
import Enlil.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Enlil.Web.Share.Contributions.API (ContributionsByUserAPI)
import Enlil.Web.Share.Projects.API (ProjectsAPI)
import Enlil.Web.Share.Types
import Servant

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
