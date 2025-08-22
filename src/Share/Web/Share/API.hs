{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.API where

import Servant
import Share.IDs
import Share.Notifications.API qualified as Notifications
import Share.OAuth.Session (AuthenticatedSession, AuthenticatedUserId, MaybeAuthenticatedSession, MaybeAuthenticatedUserId)
import Share.Prelude
import Share.Utils.API
import Share.Utils.Caching
import Share.Utils.IDs qualified as IDs
import Share.Utils.Servant
import Share.Web.Share.Branches.API (UserBranchesAPI)
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Contributions.API (ContributionsByUserAPI)
import Share.Web.Share.Projects.API (ProjectsAPI)
import Share.Web.Share.Types


-- | GET /search?query=hoj&limit=9
--
-- Search users by a prefix of their name or handle.
type OmniSearchEndpoint =
  MaybeAuthenticatedSession
    :> RequiredQueryParam "query" Query
    :> QueryParam "kinds" SearchKinds
    :> QueryParam "project-search-kind" ProjectSearchKind
    :> QueryParam "user-search-kind" UserSearchKind
    :> QueryParam "limit" Limit
    :> Get '[JSON] [SearchResult]

-- | Search for names to use in a definition search.
type SearchDefinitionNamesEndpoint =
  MaybeAuthenticatedUserId
    :> RequiredQueryParam "query" Query
    :> QueryParam "limit" Limit
    :> QueryParam "user-filter" (IDs.PrefixedID "@" UserHandle)
    :> QueryParam "project-filter" ProjectShortHand
    :> QueryParam "branch-filter" BranchOrReleaseShortHand
    :> Get '[JSON] [DefinitionNameSearchResult]

-- | Submit a definition search
type SearchDefinitionsEndpoint =
  MaybeAuthenticatedUserId
    :> RequiredQueryParam "query" Query
    :> QueryParam "limit" Limit
    :> QueryParam "user-filter" (IDs.PrefixedID "@" UserHandle)
    :> QueryParam "project-filter" ProjectShortHand
    :> QueryParam "branch-filter" BranchOrReleaseShortHand
    :> Get '[JSON] DefinitionSearchResults

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
