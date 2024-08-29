{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Contributions.Impl
  ( contributionsByProjectServer,
    contributionsByUserServer,
    listContributionsByProjectEndpoint,
    listContributionsByUserEndpoint,
  )
where

import Control.Lens
import Servant
import Servant.Server.Generic (AsServerT)
import Share.Branch (Branch (..))
import Share.Branch qualified as Branch
import Share.Codebase qualified as Codebase
import Share.Codebase qualified as PG
import Share.Contribution
import Share.IDs (PrefixedHash (..), ProjectSlug (..), UserHandle)
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.Queries qualified as BranchQ
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Project
import Share.User qualified as User
import Share.Utils.API
import Share.Utils.Caching (Cached)
import Share.Utils.Caching qualified as Caching
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.Comments
import Share.Web.Share.Comments.Impl qualified as Comments
import Share.Web.Share.Comments.Types
import Share.Web.Share.Contributions.API
import Share.Web.Share.Contributions.API qualified as API
import Share.Web.Share.Contributions.MergeDetection qualified as MergeDetection
import Share.Web.Share.Contributions.Types
import Share.Web.Share.Diffs.Impl qualified as Diffs
import Share.Web.Share.Diffs.Types (ShareNamespaceDiffResponse (..), ShareTermDiffResponse (..), ShareTypeDiffResponse (..))
import Share.Web.Share.Types (UserDisplayInfo)
import Unison.Name (Name)
import Unison.Syntax.Name qualified as Name

contributionsByProjectServer :: Maybe Session -> UserHandle -> ProjectSlug -> API.ContributionsByProjectRoutes (AsServerT WebApp)
contributionsByProjectServer session handle projectSlug =
  API.ContributionsByProjectRoutes
    { listContributions = listContributionsByProjectEndpoint session handle projectSlug,
      createContribution = createContributionEndpoint session handle projectSlug,
      contributionResource = \contributionNumber ->
        addServerTag (Proxy @(NamedRoutes API.ContributionResourceRoutes)) "contribution-number" (IDs.toText contributionNumber) $
          contributionsResourceServer session handle projectSlug contributionNumber
    }

diffResourceServer :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.ContributionNumber -> API.DiffRoutes (AsServerT WebApp)
diffResourceServer session handle projectSlug contributionNumber =
  API.DiffRoutes
    { diffTerms = contributionDiffTermsEndpoint session handle projectSlug contributionNumber,
      diffTypes = contributionDiffTypesEndpoint session handle projectSlug contributionNumber,
      diffContribution = contributionDiffEndpoint session handle projectSlug contributionNumber
    }

timelineServer :: (Maybe Session) -> UserHandle -> ProjectSlug -> IDs.ContributionNumber -> API.TimelineRoutes (AsServerT WebApp)
timelineServer session handle projectSlug contributionNumber =
  API.TimelineRoutes
    { getTimeline = getContributionTimelineEndpoint session handle projectSlug contributionNumber,
      comments = commentsServer contributionNumber
    }
  where
    commentsServer contributionNumber =
      createCommentOnContributionEndpoint session handle projectSlug contributionNumber
        :<|> commentResourceServer
    commentResourceServer commentId =
      Comments.updateCommentEndpoint session handle projectSlug commentId
        :<|> Comments.deleteCommentEndpoint session handle projectSlug commentId

mergeServer :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.ContributionNumber -> API.MergeRoutes (AsServerT WebApp)
mergeServer session handle projectSlug contributionNumber =
  API.MergeRoutes
    { mergeContribution = mergeContributionEndpoint session handle projectSlug contributionNumber,
      checkMergeContribution = checkMergeContributionEndpoint session handle projectSlug contributionNumber
    }

contributionsResourceServer :: Maybe Session -> UserHandle -> ProjectSlug -> IDs.ContributionNumber -> API.ContributionResourceRoutes (AsServerT WebApp)
contributionsResourceServer session handle projectSlug contributionNumber =
  API.ContributionResourceRoutes
    { getContributionByNumber = getContributionByNumberEndpoint session handle projectSlug contributionNumber,
      updateContributionByNumber = updateContributionByNumberEndpoint session handle projectSlug contributionNumber,
      diff = diffResourceServer session handle projectSlug contributionNumber,
      merge = mergeServer session handle projectSlug contributionNumber,
      timeline = timelineServer session handle projectSlug contributionNumber
    }

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
  WebApp (Paged ListContributionsCursor (ShareContribution UserDisplayInfo))
listContributionsByProjectEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) handle projectSlug cursor mayLimit authorFilter statusFilter kindFilter = do
  (project@Project {projectId}, authorUserId) <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    authorFilterID <- for authorFilter \(IDs.PrefixedID authorHandle) -> do
      User.user_id <$> Q.userByHandle authorHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    pure (project, authorFilterID)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionListByProject mayCallerUserId project
  (nextCursor, contributions) <- PG.runTransaction $ do
    ContributionsQ.listContributionsByProjectId projectId limit cursor authorUserId statusFilter kindFilter
      >>= UsersQ.userDisplayInfoOf (_2 . traversed . traversed)
  pure $ Paged {items = contributions, cursor = nextCursor}
  where
    limit = fromMaybe 20 mayLimit
    projectShorthand = IDs.ProjectShortHand {userHandle = handle, projectSlug}

createContributionEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  CreateContributionRequest ->
  WebApp (ShareContribution UserDisplayInfo)
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
      >>= UsersQ.userDisplayInfoOf traversed
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

getContributionByNumberEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  WebApp (ShareContribution UserDisplayInfo :++ AtKey "contributionStateToken" ContributionStateToken)
getContributionByNumberEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber = do
  (project, shareContribution) <- PG.runTransactionOrRespondError $ do
    project@Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    shareContribution <-
      ContributionsQ.shareContributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
        >>= UsersQ.userDisplayInfoOf traversed
        >>= \(shareContribution@ShareContribution {contributionId}) -> do
          contributionStateToken <- ContributionsQ.contributionStateTokenById contributionId
          pure $ shareContribution :++ AtKey contributionStateToken
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
  WebApp (ShareContribution UserDisplayInfo)
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
      >>= UsersQ.userDisplayInfoOf traversed
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
        & UsersQ.userDisplayInfoOf (traverse . traverse)
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
  WebApp (Paged ListContributionsCursor (ShareContribution UserDisplayInfo))
listContributionsByUserEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle mayCursor mayLimit statusFilter kindFilter = do
  (contributions, nextCursor) <- PG.runTransactionOrRespondError $ do
    user <- Q.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "user:missing") "User not found")
    (nextCursor, contributions) <-
      ContributionsQ.listContributionsByUserId mayCallerUserId (User.user_id user) limit mayCursor statusFilter kindFilter
        >>= UsersQ.userDisplayInfoOf (_2 . traversed . traversed)
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
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionDiffRead mayCallerUserId project
  let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
  let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
  oldPBSH <- Codebase.runCodebaseTransactionOrRespondError oldCodebase $ do
    lift $ Q.projectBranchShortHandByBranchId oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
  newPBSH <- Codebase.runCodebaseTransactionOrRespondError newCodebase $ do
    lift $ Q.projectBranchShortHandByBranchId newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")

  let oldCausalId = fromMaybe oldBranchCausalId bestCommonAncestorCausalId
  let cacheKeys = [IDs.toText contributionId, IDs.toText newPBSH, IDs.toText oldPBSH, Caching.causalIdCacheKey newBranchCausalId, Caching.causalIdCacheKey oldCausalId]
  Caching.cachedResponse authZReceipt "contribution-diff" cacheKeys do
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

contributionDiffTermsEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  Name ->
  Name ->
  WebApp (Cached JSON ShareTermDiffResponse)
contributionDiffTermsEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber oldTermName newTermName =
  do
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
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionDiffRead mayCallerUserId project
    let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
    let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
    oldPBSH <- Codebase.runCodebaseTransactionOrRespondError oldCodebase $ do
      lift $ Q.projectBranchShortHandByBranchId oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    newPBSH <- Codebase.runCodebaseTransactionOrRespondError newCodebase $ do
      lift $ Q.projectBranchShortHandByBranchId newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    let oldCausalId = fromMaybe oldBranchCausalId bestCommonAncestorCausalId
    let cacheKeys = [IDs.toText contributionId, IDs.toText newPBSH, IDs.toText oldPBSH, Caching.causalIdCacheKey newBranchCausalId, Caching.causalIdCacheKey oldCausalId, Name.toText oldTermName, Name.toText newTermName]
    Caching.cachedResponse authZReceipt "contribution-diff-terms" cacheKeys do
      (oldBranchHashId, newBranchHashId) <- PG.runTransaction $ CausalQ.expectNamespaceIdsByCausalIdsOf both (oldCausalId, newBranchCausalId)
      (oldTerm, newTerm, displayObjDiff) <- Diffs.diffTerms authZReceipt (oldCodebase, oldBranchHashId, oldTermName) (newCodebase, newBranchHashId, newTermName)
      pure $
        ShareTermDiffResponse
          { project = projectShorthand,
            oldBranch = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand oldPBSH,
            newBranch = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand newPBSH,
            oldTerm = oldTerm,
            newTerm = newTerm,
            diff = displayObjDiff
          }
  where
    projectShorthand :: IDs.ProjectShortHand
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

contributionDiffTypesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  Name ->
  Name ->
  WebApp (Cached JSON ShareTypeDiffResponse)
contributionDiffTypesEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) userHandle projectSlug contributionNumber oldTypeName newTypeName =
  do
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
    authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionDiffRead mayCallerUserId project
    let oldCodebase = Codebase.codebaseForProjectBranch authZReceipt project oldBranch
    let newCodebase = Codebase.codebaseForProjectBranch authZReceipt project newBranch
    oldPBSH <- Codebase.runCodebaseTransactionOrRespondError oldCodebase $ do
      lift $ Q.projectBranchShortHandByBranchId oldBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    newPBSH <- Codebase.runCodebaseTransactionOrRespondError newCodebase $ do
      lift $ Q.projectBranchShortHandByBranchId newBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    let oldCausalId = fromMaybe oldBranchCausalId bestCommonAncestorCausalId
    let cacheKeys = [IDs.toText contributionId, IDs.toText newPBSH, IDs.toText oldPBSH, Caching.causalIdCacheKey newBranchCausalId, Caching.causalIdCacheKey oldCausalId, Name.toText oldTypeName, Name.toText newTypeName]
    Caching.cachedResponse authZReceipt "contribution-diff-types" cacheKeys do
      (oldBranchHashId, newBranchHashId) <- PG.runTransaction $ CausalQ.expectNamespaceIdsByCausalIdsOf both (oldCausalId, newBranchCausalId)
      (oldType, newType, displayObjDiff) <- Diffs.diffTypes authZReceipt (oldCodebase, oldBranchHashId, oldTypeName) (newCodebase, newBranchHashId, newTypeName)
      pure $
        ShareTypeDiffResponse
          { project = projectShorthand,
            oldBranch = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand oldPBSH,
            newBranch = IDs.IsBranchShortHand $ IDs.projectBranchShortHandToBranchShortHand newPBSH,
            oldType = oldType,
            newType = newType,
            diff = displayObjDiff
          }
  where
    projectShorthand :: IDs.ProjectShortHand
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

mergeContributionEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  (AtKey key ContributionStateToken) ->
  WebApp MergeContributionResponse
mergeContributionEndpoint session userHandle projectSlug contributionNumber (AtKey contributionStateToken) = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  (contribution, project) <- PG.runTransactionOrRespondError $ do
    project <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    contribution <- ContributionsQ.contributionByProjectIdAndNumber project.projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    pure (contribution, project)
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionMerge callerUserId contribution
  when (contribution.status == Merged) do
    respondError $ SimpleServerError @417 @"contribution:already-merged" @"Contribution already merged." contribution
  PG.runTransactionOrRespondError do
    -- Refetch the contribution within the transaction
    contribution <- ContributionsQ.contributionByProjectIdAndNumber project.projectId contributionNumber `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    currentContributionStateToken <- ContributionsQ.contributionStateTokenById contribution.contributionId
    when (currentContributionStateToken /= contributionStateToken) do
      throwSomeServerError (ContributionStateChangedError contributionStateToken currentContributionStateToken)
    sourceBranch <- Q.branchById contribution.sourceBranchId `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
    targetBranch <- Q.branchById contribution.targetBranchId `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
    isFastForward <- CausalQ.isFastForward targetBranch.causal sourceBranch.causal
    if isFastForward
      then do
        let description = "Merged Contribution #" <> (IDs.toText contributionNumber) <> "\n" <> contribution.title
        newNamespaceId <- CausalQ.expectNamespaceIdsByCausalIdsOf id sourceBranch.causal
        nlReceipt <- NL.ensureNameLookupForBranchId newNamespaceId
        let projectCodebase = Codebase.codebaseEnv authZReceipt $ Codebase.codebaseLocationForProjectBranchCodebase project.ownerUserId targetBranch.contributorId
        PG.codebaseMToTransaction projectCodebase $ Codebase.importCausalIntoCodebase (Branch.branchCodebaseUser sourceBranch) sourceBranch.causal
        BranchQ.setBranchCausalHash nlReceipt description callerUserId targetBranch.branchId sourceBranch.causal
        -- Update any affected contributions to reflect the result of updating this branch.
        MergeDetection.updateContributionsFromBranchUpdate callerUserId targetBranch.branchId
        pure $ MergeContributionResponse MergeSuccess
      else pure $ MergeContributionResponse $ MergeFailed "Share only supports fast forward merges for now."
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}

checkMergeContributionEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  IDs.ContributionNumber ->
  WebApp CheckMergeContributionResponse
checkMergeContributionEndpoint session userHandle projectSlug contributionNumber = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  contribution <- PG.runTransactionOrRespondError $ do
    Project {projectId} <- Q.projectByShortHand projectShorthand `whenNothingM` throwError (EntityMissing (ErrorID "project:missing") "Project not found")
    contribution <- ContributionsQ.contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
    pure contribution
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkContributionMerge callerUserId contribution
  case contribution.status of
    Merged -> pure $ CheckMergeContributionResponse {mergeability = AlreadyMerged}
    _ -> do
      isFastForward <- PG.runTransactionOrRespondError do
        sourceBranch <- Q.branchById contribution.sourceBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Source branch not found")
        targetBranch <- Q.branchById contribution.targetBranchId `whenNothingM` throwError (EntityMissing (ErrorID "branch:missing") "Target branch not found")
        CausalQ.isFastForward targetBranch.causal sourceBranch.causal
      if isFastForward
        then pure CheckMergeContributionResponse {mergeability = CanFastForward}
        else pure CheckMergeContributionResponse {mergeability = CantMerge "Share only supports fast forward merges for now."}
  where
    projectShorthand = IDs.ProjectShortHand {userHandle, projectSlug}
