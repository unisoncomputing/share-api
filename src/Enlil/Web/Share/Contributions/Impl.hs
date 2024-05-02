{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Enlil.Web.Share.Contributions.Impl
  ( contributionsByProjectServer,
    contributionsByUserServer,
    listContributionsByProjectEndpoint,
    listContributionsByUserEndpoint,
  )
where

import Enlil.Branch (Branch (..))
import Enlil.Codebase qualified as Codebase
import Enlil.Contribution
import Enlil.IDs (PrefixedHash (..), ProjectSlug (..), UserHandle)
import Enlil.IDs qualified as IDs
import Enlil.OAuth.Session
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Causal.Queries qualified as CausalQ
import Enlil.Postgres.Contributions.Queries qualified as ContributionsQ
import Enlil.Postgres.Queries qualified as Q
import Enlil.Postgres.Users.Queries (userDisplayInfoOf)
import Enlil.Prelude
import Enlil.Project
import Enlil.User qualified as User
import Enlil.Utils.API
import Enlil.Utils.Caching (Cached)
import Enlil.Utils.Caching qualified as Caching
import Enlil.Web.App
import Enlil.Web.Authentication qualified as AuthN
import Enlil.Web.Authorization qualified as AuthZ
import Enlil.Web.Errors
import Enlil.Web.Share.Comments
import Enlil.Web.Share.Comments.Impl qualified as Comments
import Enlil.Web.Share.Comments.Types
import Enlil.Web.Share.Contributions.API
import Enlil.Web.Share.Contributions.API qualified as API
import Enlil.Web.Share.Contributions.Types
import Enlil.Web.Share.Diffs.Impl qualified as Diffs
import Enlil.Web.Share.Diffs.Types (ShareNamespaceDiffResponse (..))
import Enlil.Web.Share.Types (UserDisplayInfo)
import Servant

contributionsByProjectServer :: Maybe Session -> UserHandle -> ProjectSlug -> ServerT API.ContributionsByProjectAPI WebApp
contributionsByProjectServer session handle projectSlug =
  let commentResourceServer session commentId =
        Comments.updateCommentEndpoint session handle projectSlug commentId
          :<|> Comments.deleteCommentEndpoint session handle projectSlug commentId
      commentsServer contributionNumber =
        createCommentOnContributionEndpoint session handle projectSlug contributionNumber
          :<|> commentResourceServer session

      timelineServer contributionNumber =
        ( getContributionTimelineEndpoint session handle projectSlug contributionNumber
            :<|> commentsServer contributionNumber
        )
      contributionResourceServer contributionNumber =
        addServerTag (Proxy @API.ContributionResourceServer) "contribution-number" (IDs.toText contributionNumber) $
          getContributionByNumberEndpoint session handle projectSlug contributionNumber
            :<|> updateContributionByNumberEndpoint session handle projectSlug contributionNumber
            :<|> contributionDiffEndpoint session handle projectSlug contributionNumber
            :<|> mergeContributionEndpoint session handle projectSlug contributionNumber
            :<|> timelineServer contributionNumber
   in listContributionsByProjectEndpoint session handle projectSlug
        :<|> createContributionEndpoint session handle projectSlug
        :<|> contributionResourceServer

contributionsByUserServer :: Maybe Session -> UserHandle -> ServerT API.ContributionsByUserAPI WebApp
contributionsByUserServer session handle =
  listContributionsByUserEndpoint session handle

createCommentOnContributionEndpoint :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.ContributionNumber -> CreateCommentRequest -> WebApp (CommentEvent UserDisplayInfo)
createCommentOnContributionEndpoint session handle slug contributionNumber createCommentReq = do
  Contribution {contributionId} <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    ContributionsQ.contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
  Comments.createCommentEndpoint session handle slug (Left contributionId) createCommentReq
  where
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug = slug}

listContributionsByProjectEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  Maybe (Cursor ListContributionsCursor) ->
  Maybe Limit ->
  Maybe (IDs.PrefixedID "@" UserHandle) ->
  Maybe ContributionStatus ->
  Maybe ContributionKindFilter ->
  WebApp (Paged ListContributionsCursor ShareContribution)
listContributionsByProjectEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) handle projectSlug cursor mayLimit authorFilter statusFilter kindFilter = do
  (project@Project {projectId}, authorUserId) <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    authorFilterID <- for authorFilter \(IDs.PrefixedID authorHandle) -> do
      User.user_id <$> Q.userByHandle authorHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    pure (project, authorFilterID)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionListByProject mayCallerUserId project
  (nextCursor, contributions) <- PG.runTransaction $ ContributionsQ.listContributionsByProjectId projectId limit cursor authorUserId statusFilter kindFilter
  pure $ Paged {items = contributions, cursor = nextCursor}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

createContributionEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  CreateContributionRequest ->
  WebApp ShareContribution
createContributionEndpoint session userHandle projectSlug (CreateContributionRequest {title, description, status, sourceBranchShortHand, targetBranchShortHand}) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  (project@Project {projectId}, Branch {branchId = sourceBranchId}, Branch {branchId = targetBranchId}) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    sourceBranch <- Q.branchByProjectIdAndShortHand projectId sourceBranchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    targetBranch <- Q.branchByProjectIdAndShortHand projectId targetBranchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    pure (project, sourceBranch, targetBranch)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionCreate callerUserId project
  PG.runTransactionOrRespondError $ do
    (_, contributionNumber) <- ContributionsQ.createContribution callerUserId projectId title description status sourceBranchId targetBranchId
    ContributionsQ.shareContributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (InternalServerError "create-contribution-error" internalServerError)
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

getContributionByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  WebApp ShareContribution
getContributionByNumberEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber = do
  (project, shareContribution) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    shareContribution <- ContributionsQ.shareContributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    pure (project, shareContribution)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionRead mayCallerUserId project
  pure shareContribution
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

updateContributionByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  UpdateContributionRequest ->
  WebApp ShareContribution
updateContributionByNumberEndpoint session handle projectSlug contributionNumber updateRequest@UpdateContributionRequest {title, description, status, sourceBranchSH, targetBranchSH} = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  (contribution@Contribution {contributionId, projectId, number = contributionNumber}, maySourceBranch, mayTargetBranch) <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    contribution <- ContributionsQ.contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    maySourceBranch <- for sourceBranchSH \sb -> Q.branchByProjectIdAndShortHand projectId sb `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    mayTargetBranch <- for targetBranchSH \tb -> Q.branchByProjectIdAndShortHand projectId tb `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    pure (contribution, maySourceBranch, mayTargetBranch)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionUpdate callerUserId contribution updateRequest
  PG.runTransactionOrRespondError $ do
    _ <- ContributionsQ.updateContribution callerUserId contributionId title description status (branchId <$> maySourceBranch) (branchId <$> mayTargetBranch)
    ContributionsQ.shareContributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
  where
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

getContributionTimelineEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  Maybe (Cursor ContributionTimelineCursor) ->
  Maybe Limit ->
  WebApp (Paged ContributionTimelineCursor (ContributionTimelineEvent UserDisplayInfo))
getContributionTimelineEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber mayCursor mayLimit = do
  (project, shareContributionTimeline, nextCursor) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "project:missing") "Project not found")
    (nextCursor, shareContributionTimeline) <- ContributionsQ.getPagedShareContributionTimelineByProjectIdAndNumber projectId contributionNumber (unCursor <$> mayCursor) limit
    shareContributionsTimelineWithUserInfo <-
      shareContributionTimeline
        & userDisplayInfoOf (traverse . traverse)
    pure (project, shareContributionsTimelineWithUserInfo, nextCursor)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionRead mayCallerUserId project
  pure $ Paged {items = shareContributionTimeline, cursor = Cursor <$> nextCursor}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

listContributionsByUserEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe (Cursor ListContributionsCursor) ->
  Maybe Limit ->
  Maybe ContributionStatus ->
  Maybe ContributionKindFilter ->
  WebApp (Paged ListContributionsCursor ShareContribution)
listContributionsByUserEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle mayCursor mayLimit statusFilter kindFilter = do
  (contributions, nextCursor) <- PG.runTransactionOrRespondError $ do
    user <- Q.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    (nextCursor, contributions) <- ContributionsQ.listContributionsByUserId mayCallerUserId (User.user_id user) limit mayCursor statusFilter kindFilter
    pure (contributions, nextCursor)
  pure $ Paged {items = contributions, cursor = nextCursor}
  where
    limit = fromMaybe 20 mayLimit

contributionDiffEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  WebApp (Cached JSON ShareNamespaceDiffResponse)
contributionDiffEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber = do
  ( project,
    Contribution {contributionId, bestCommonAncestorCausalId},
    oldBranch@Branch {causal = oldBranchCausalId, branchId = oldBranchId},
    newBranch@Branch {causal = newBranchCausalId, branchId = newBranchId}
    ) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    contribution@Contribution {sourceBranchId = newBranchId, targetBranchId = oldBranchId} <- ContributionsQ.contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    newBranch <- Q.branchById newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    oldBranch <- Q.branchById oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    pure (project, contribution, oldBranch, newBranch)
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionRead mayCallerUserId project
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  oldPBSH <- Codebase.runCodebaseTransactionOrRespondError oldCodebase $ do
    lift $ Q.projectBranchShortHandByBranchId oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
  newPBSH <- Codebase.runCodebaseTransactionOrRespondError newCodebase $ do
    lift $ Q.projectBranchShortHandByBranchId newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")

  let cacheKeys = [IDs.toText contributionId, IDs.toText newPBSH, IDs.toText oldPBSH, Caching.causalIdCacheKey newBranchCausalId, Caching.causalIdCacheKey oldBranchCausalId]
  Caching.cachedResponse authZReceipt "contribution-diff" cacheKeys do
    let oldCausalId = fromMaybe oldBranchCausalId bestCommonAncestorCausalId
    namespaceDiff <- Diffs.diffCausals authZReceipt oldCausalId newBranchCausalId
    (newBranchCausalHash, oldCausalHash) <- PG.runTransaction $ do
      newBranchCausalHash <- CausalQ.expectCausalHashesByIdsOf id newBranchCausalId
      oldCausalHash <- CausalQ.expectCausalHashesByIdsOf id oldCausalId
      pure (newBranchCausalHash, oldCausalHash)
    pure $
      ShareNamespaceDiffResponse
        { project = projectShorthand,
          newRef = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand newPBSH,
          newRefHash = Just $ PrefixedHash newBranchCausalHash,
          oldRef = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand oldPBSH,
          oldRefHash = Just $ PrefixedHash oldCausalHash,
          diff = namespaceDiff
        }
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

mergeContributionEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  WebApp ()
mergeContributionEndpoint session userHandle projectSlug contributionNumber = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  contribution <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    contribution <- ContributionsQ.contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    pure (contribution)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionMerge callerUserId contribution
  respondError Unimplemented
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}
