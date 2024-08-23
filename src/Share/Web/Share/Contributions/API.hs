{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Contributions.API where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Utils.API
import Share.Utils.Caching (Cached)
import Share.Utils.Servant (RequiredQueryParam)
import Share.Web.Share.Comments.API qualified as Comments
import Share.Web.Share.Contributions.Types
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse, ShareTermDiffResponse, ShareTypeDiffResponse)
import Share.Web.Share.Types (UserDisplayInfo)
import Unison.Name (Name)

type ContributionsByUserAPI = ListContributionsByUserEndpoint

type ContributionsByProjectAPI = NamedRoutes ContributionsByProjectRoutes

data ContributionsByProjectRoutes mode = ContributionsByProjectRoutes
  { listContributions :: mode :- ListContributionsByProjectEndpoint,
    createContribution :: mode :- CreateContribution,
    contributionResource :: mode :- Capture "contribution_number" ContributionNumber :> NamedRoutes ContributionResourceRoutes
  }
  deriving stock (Generic)

data DiffRoutes mode = DiffRoutes
  { diffTerms :: mode :- "terms" :> ContributionDiffTermsEndpoint,
    diffTypes :: mode :- "types" :> ContributionDiffTypesEndpoint,
    diffContribution :: mode :- ContributionDiffEndpoint
  }
  deriving stock (Generic)

data TimelineRoutes mode = TimelineRoutes
  { getTimeline :: mode :- GetContributionTimeline,
    comments :: mode :- "comments" :> Comments.CommentsServer
  }
  deriving stock (Generic)

data MergeRoutes mode = MergeRoutes
  { mergeContribution :: mode :- MergeContributionEndpoint,
    checkMergeContribution :: mode :- "check" :> CheckMergeContributionEndpoint
  }
  deriving stock (Generic)

data ContributionResourceRoutes mode
  = ContributionResourceRoutes
  { getContributionByNumber :: mode :- GetContributionByNumber,
    updateContributionByNumber :: mode :- UpdateContributionByNumber,
    diff :: mode :- "diff" :> NamedRoutes DiffRoutes,
    merge :: mode :- "merge" :> NamedRoutes MergeRoutes,
    timeline :: mode :- "timeline" :> NamedRoutes TimelineRoutes
  }
  deriving stock (Generic)

type ContributionDiffEndpoint =
  Get '[JSON] (Cached JSON ShareNamespaceDiffResponse)

type ContributionDiffTermsEndpoint =
  RequiredQueryParam "oldTerm" Name
    :> RequiredQueryParam "newTerm" Name
    :> Get '[JSON] (Cached JSON ShareTermDiffResponse)

type ContributionDiffTypesEndpoint =
  RequiredQueryParam "oldType" Name
    :> RequiredQueryParam "newType" Name
    :> Get '[JSON] (Cached JSON ShareTypeDiffResponse)

type ListContributionsCursor = (UTCTime, ContributionId)

type ListContributionsByProjectEndpoint =
  QueryParam "cursor" (Cursor ListContributionsCursor)
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    -- Only return contributions by this author
    :> QueryParam "author" (PrefixedID "@" UserHandle)
    :> QueryParam "status" ContributionStatus
    -- Filter the contributions by the kind of their source branch
    :> QueryParam "kind" ContributionKindFilter
    :> Get '[JSON] (Paged ListContributionsCursor (ShareContribution UserDisplayInfo))

type ListContributionsByUserEndpoint =
  QueryParam "cursor" (Cursor ListContributionsCursor)
    -- Return a maximimum of this many branches
    :> QueryParam "limit" Limit
    :> QueryParam "status" ContributionStatus
    -- Filter the contributions by the kind of their source branch
    :> QueryParam "kind" ContributionKindFilter
    :> Get '[JSON] (Paged ListContributionsCursor (ShareContribution UserDisplayInfo))

type CreateContribution =
  ReqBody '[JSON] CreateContributionRequest
    :> Post '[JSON] (ShareContribution UserDisplayInfo)

type GetContributionByNumber = Get '[JSON] (ShareContribution UserDisplayInfo)

type UpdateContributionByNumber =
  ReqBody '[JSON] UpdateContributionRequest
    :> Patch '[JSON] (ShareContribution UserDisplayInfo)

-- | Merged a contribution
type MergeContributionEndpoint =
  Post '[JSON] MergeContributionResponse

-- | Check if a contribution can be merged
type CheckMergeContributionEndpoint =
  Get '[JSON] CheckMergeContributionResponse

type ContributionTimelineCursor = UTCTime

type GetContributionTimeline =
  QueryParam "cursor" (Cursor ContributionTimelineCursor)
    :> QueryParam "limit" Limit
    :> Get '[JSON] (Paged ContributionTimelineCursor (ContributionTimelineEvent UserDisplayInfo))
