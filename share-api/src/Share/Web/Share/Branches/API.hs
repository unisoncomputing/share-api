{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Branches.API where

import Data.Time (UTCTime)
import Servant
import Share.IDs
import Share.Utils.API
import Share.Utils.Caching
import Share.Web.Share.Branches.Types (BranchHistoryResponse, BranchKindFilter, ShareBranch)
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Types
import U.Codebase.HashTags (CausalHash)

type ProjectBranchesAPI =
  ListBranchesByProjectEndpoint
    :<|> (Capture "branch_short_hand" BranchShortHand :> ProjectBranchResourceAPI)

type ProjectBranchResourceAPI =
  ( ("readme" :> ProjectBranchReadmeEndpoint)
      :<|> ("releaseNotes" :> ProjectBranchReleaseNotesEndpoint)
      :<|> ProjectBranchDetailsEndpoint
      :<|> ProjectBranchDeleteEndpoint
      :<|> ProjectBranchHistoryEndpoint
      :<|> CodeBrowseAPI
  )

type ProjectBranchDetailsEndpoint = Get '[JSON] ShareBranch

type ProjectBranchDeleteEndpoint = Delete '[JSON] ()

type ProjectBranchHistoryEndpoint =
  QueryParam "cursor" (Cursor CausalHash)
    :> QueryParam "limit" Limit
    :> Get '[JSON] BranchHistoryResponse

type ProjectBranchReadmeEndpoint =
  QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON ReadmeResponse)

type ProjectBranchReleaseNotesEndpoint =
  QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON DocResponse)

type ListBranchesCursor = (UTCTime, BranchId)

type ListBranchesByProjectEndpoint =
  QueryParam "cursor" (Cursor ListBranchesCursor)
    :> QueryParam "limit" Limit
    -- Limit the branches to contributor branches by this user
    :> QueryParam "contributor-handle" (PrefixedID "@" UserHandle)
    -- Filter the branches by kind
    :> QueryParam "kind" BranchKindFilter
    -- Search by a prefix of the branch name
    :> QueryParam "name-prefix" Query
    :> Get '[JSON] (Paged ListBranchesCursor ShareBranch)

type UserBranchesAPI = ListBranchesByUserEndpoint

type ListBranchesByUserEndpoint =
  QueryParam "cursor" (Cursor ListBranchesCursor)
    :> QueryParam "project-ref" ProjectShortHand
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    -- Search by a prefix of the branch name
    :> QueryParam "name-prefix" Query
    :> Get '[JSON] (Paged ListBranchesCursor ShareBranch)
