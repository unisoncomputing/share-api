{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.Branches.API where

import Data.Time (UTCTime)
import Enlil.IDs
import Enlil.Utils.API
import Enlil.Utils.Caching
import Enlil.Web.Share.Branches.Types (BranchKindFilter, ShareBranch)
import Enlil.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Enlil.Web.Share.Types
import Servant
import U.Codebase.HashTags (CausalHash)

type ProjectBranchesAPI =
  ListBranchesByProjectEndpoint
    :<|> (Capture "branch_short_hand" BranchShortHand :> ProjectBranchResourceAPI)

type ProjectBranchResourceAPI =
  ( ("readme" :> ProjectBranchReadmeEndpoint)
      :<|> ("releaseNotes" :> ProjectBranchReleaseNotesEndpoint)
      :<|> ProjectBranchDetailsEndpoint
      :<|> ProjectBranchDeleteEndpoint
      :<|> CodeBrowseAPI
  )

type ProjectBranchDetailsEndpoint = Get '[JSON] ShareBranch

type ProjectBranchDeleteEndpoint = Delete '[JSON] ()

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
