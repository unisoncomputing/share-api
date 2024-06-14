{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Contributions.API where

import Data.Time (UTCTime)
import Servant
import Share.Contribution (ContributionStatus)
import Share.IDs
import Share.Utils.API
import Share.Utils.Caching (Cached)
import Share.Web.Share.Comments.API qualified as Comments
import Share.Web.Share.Contributions.Types
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse)
import Share.Web.Share.Types (UserDisplayInfo)

type ContributionsByUserAPI = ListContributionsByUserEndpoint

type ContributionsByProjectAPI =
  ListContributionsByProjectEndpoint
    :<|> CreateContribution
    :<|> (Capture "contribution_number" ContributionNumber :> ContributionResourceServer)

type ContributionResourceServer =
  ( GetContributionByNumber
      :<|> UpdateContributionByNumber
      :<|> ("diff" :> ContributionDiffEndpoint)
      :<|> ("merge" :> MergeContribution)
      :<|> ( "timeline"
               :> ( GetContributionTimeline
                      :<|> ("comments" :> Comments.CommentsServer)
                  )
           )
  )

type ContributionDiffEndpoint =
  Get '[JSON] (Cached JSON ShareNamespaceDiffResponse)

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

type MergeContribution =
  Post '[JSON] ()

type ContributionTimelineCursor = UTCTime

type GetContributionTimeline =
  QueryParam "cursor" (Cursor ContributionTimelineCursor)
    :> QueryParam "limit" Limit
    :> Get '[JSON] (Paged ContributionTimelineCursor (ContributionTimelineEvent UserDisplayInfo))
