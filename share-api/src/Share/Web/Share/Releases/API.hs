{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Releases.API where

import Data.Int (Int64)
import Share.IDs
import Share.Utils.API
import Share.Utils.Caching
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Releases.Types
import Share.Web.Share.Types
import Servant

type ProjectReleasesAPI =
  ListReleasesByProjectEndpoint
    :<|> CreateReleaseEndpoint
    :<|> (Capture "release_version" ReleaseVersion :> ProjectReleaseResourceAPI)

type ProjectReleaseResourceAPI =
  ProjectReleaseEndpoint
    :<|> ("readme" :> ProjectReleaseReadmeEndpoint)
    :<|> ("releaseNotes" :> ProjectReleaseNotesEndpoint)
    :<|> UpdateReleaseEndpoint
    :<|> CodeBrowseAPI

type ProjectReleaseEndpoint = Get '[JSON] APIRelease

type ProjectReleaseReadmeEndpoint = Get '[JSON] (Cached JSON ReadmeResponse)

type ProjectReleaseNotesEndpoint = Get '[JSON] (Cached JSON DocResponse)

-- | (major version, minor version, patch version, release id)
type ListReleasesCursor = (Int64, Int64, Int64, ReleaseId)

type ListReleasesByProjectEndpoint =
  QueryParam "cursor" (Cursor ListReleasesCursor)
    :> QueryParam "limit" Limit
    -- Search by a prefix of the branch name
    :> QueryParam "version-prefix" Query
    :> QueryParam "status" ReleaseStatusFilter
    :> Get '[JSON] (Paged ListReleasesCursor APIRelease)

type CreateReleaseEndpoint =
  ReqBody '[JSON] CreateReleaseRequest
    :> PostCreated '[JSON] APIRelease

type UpdateReleaseEndpoint =
  ReqBody '[JSON] UpdateReleaseRequest
    :> Patch '[JSON] ()
