{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Users.API
  ( API,
    UserResourceAPI,
  )
where

import Servant
import Share.IDs
import Share.Notifications.API qualified as Notifications
import Share.OAuth.Session (AuthenticatedUserId, MaybeAuthenticatedSession)
import Share.Utils.Caching
import Share.Web.Share.Branches.API (UserBranchesAPI)
import Share.Web.Share.Contributions.API (ContributionsByUserAPI)
import Share.Web.Share.Projects.API (ProjectsAPI)
import Share.Web.Share.Types

type API =
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
    :<|> ("notifications" :> Notifications.API)

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
