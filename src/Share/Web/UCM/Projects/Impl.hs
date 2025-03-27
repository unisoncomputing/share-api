{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Share.Web.UCM.Projects.Impl (server, createProjectRelease, getBestNameLookupBase) where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.List.Extra qualified as List
import Servant
import Share.Branch
import Share.Branch qualified as Branch
import Share.Codebase qualified as Codebase
import Share.IDs
import Share.IDs qualified as IDs
import Share.Metrics qualified as Metrics
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (CausalHash, CausalId)
import Share.Postgres.NameLookups.Ops qualified as Codebase
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project
import Share.Release (Release (..))
import Share.User
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant
import Share.Utils.Unison (causalHashToHash32, hash32ToCausalHash)
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (InternalServerError (..), respondError)
import Share.Web.Errors qualified as Errors
import Share.Web.Share.Contributions.MergeDetection qualified as MergeDetection
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.Sync.Impl qualified as SyncQ
import Unison.Server.Orphans ()
import Unison.Share.API.Projects qualified as UCMProjects
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

server :: Maybe Session -> ServerT UCMProjects.ProjectsAPI WebApp
server maySession =
  getProjectEndpoint maySession
    :<|> createProjectEndpoint maySession
    :<|> getProjectBranchEndpoint maySession
    :<|> createProjectBranchOrReleaseEndpoint maySession
    :<|> setProjectBranchOrReleaseHeadEndpoint maySession

type ProjectShortHandParam = Text

type ProjectIdParam = Text

type BranchIdParam = Text

type BranchNameParam = Text

getProjectEndpoint :: Maybe Session -> Maybe ProjectIdParam -> Maybe ProjectShortHandParam -> WebApp UCMProjects.GetProjectResponse
getProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) mayUcmProjectId mayUcmProjectSH = toResponse $ do
  projectId <- lift (runMaybeT (projectIdByName <|> projectIdQueryParam)) `whenNothingM` throwError (UCMProjects.GetProjectResponseNotFound (UCMProjects.NotFound "Project not found"))
  (Project {slug = projectSlug}, User {handle}, defaultBranch, latestRelease) <- pgT $ do
    (project@Project {ownerUserId}, _favData, _projectOwner, defaultBranch, latestRelease) <- Q.projectByIdWithMetadata callerUserId projectId `orThrow` UCMProjects.GetProjectResponseNotFound (UCMProjects.NotFound "Project not found")
    user <- UserQ.userByUserId ownerUserId `orThrow` UCMProjects.GetProjectResponseNotFound (UCMProjects.NotFound "User not found")
    pure (project, user, fmap IDs.toText defaultBranch, fmap IDs.toText latestRelease)
  _authZReceipt <- AuthZ.checkProjectGet callerUserId projectId `ifUnauthorized` UCMProjects.GetProjectResponseUnauthorized
  let apiProject = UCMProjects.Project {projectId = IDs.toText projectId, projectName = IDs.toText (ProjectShortHand {userHandle = handle, projectSlug}), latestRelease, defaultBranch}
  pure $ UCMProjects.GetProjectResponseSuccess apiProject
  where
    hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
    hoistMaybe = MaybeT . pure
    projectIdByName :: MaybeT WebApp ProjectId
    projectIdByName = do
      ucmProjectSH <- hoistMaybe mayUcmProjectSH
      ProjectShortHand {userHandle, projectSlug} <- lift $ parseParam @ProjectShortHand "name" ucmProjectSH
      MaybeT . PG.runTransaction $ Q.projectIDFromHandleAndSlug userHandle projectSlug
    projectIdQueryParam :: MaybeT WebApp ProjectId
    projectIdQueryParam = do
      ucmProjectId <- hoistMaybe mayUcmProjectId
      lift $ parseParam @ProjectId "id" ucmProjectId

createProjectEndpoint :: Maybe Session -> UCMProjects.CreateProjectRequest -> WebApp UCMProjects.CreateProjectResponse
createProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) (UCMProjects.CreateProjectRequest {projectName}) = toResponse do
  ProjectShortHand {userHandle, projectSlug} <- lift $ parseParam @ProjectShortHand "projectName" projectName
  User {user_id = targetUserId} <- pgT do
    UserQ.userByHandle userHandle `orThrow` UCMProjects.CreateProjectResponseNotFound (UCMProjects.NotFound "User not found")
  AuthZ.checkProjectCreate callerUserId targetUserId `ifUnauthorized` UCMProjects.CreateProjectResponseUnauthorized
  let visibility = ProjectPrivate
  let summary = Nothing
  let tags = mempty
  projectId <- lift $ PGO.createProject targetUserId projectSlug summary tags visibility
  let apiProject = UCMProjects.Project {projectId = IDs.toText projectId, projectName, latestRelease = Nothing, defaultBranch = Nothing}
  pure $ UCMProjects.CreateProjectResponseSuccess apiProject

getProjectBranchEndpoint :: Maybe Session -> ProjectIdParam -> Maybe BranchIdParam -> Maybe BranchNameParam -> Bool -> WebApp UCMProjects.GetProjectBranchResponse
getProjectBranchEndpoint (AuthN.MaybeAuthedUserID mayCallerUserId) projectIdParam mayBranchId mayBranchName includeSquashedHead = toResponse do
  projectId <- lift $ parseParam "projectId" projectIdParam
  branchOrRelease <- lift (runMaybeT (branchByName projectId <|> Left <$> branchById)) `orThrow` UCMProjects.GetProjectBranchResponseBranchNotFound (UCMProjects.NotFound "Branch not found")
  (project, User {handle = projectOwnerUserHandle, user_id = projectOwnerUserId}, mayContributor) <- pgT $ do
    project@Project {ownerUserId} <- Q.projectById projectId `orThrow` UCMProjects.GetProjectBranchResponseProjectNotFound (UCMProjects.NotFound "Project not found")
    -- We lump the project not found and user not found errors together here because if the
    -- project exists, the user MUST exist because of the foreign key constraint,
    -- so this error should never happen.
    projectOwnerUser <- UserQ.userByUserId ownerUserId `orThrow` UCMProjects.GetProjectBranchResponseProjectNotFound (UCMProjects.NotFound "User not found")
    let contributorId = case branchOrRelease of
          Left Branch {contributorId} -> contributorId
          Right _ -> Nothing
    mayContributorUser <- for contributorId \cid -> UserQ.userByUserId cid `orThrow` UCMProjects.GetProjectBranchResponseProjectNotFound (UCMProjects.NotFound "User not found")
    pure (project, projectOwnerUser, mayContributorUser)

  authZReceipt <- case branchOrRelease of
    Left branch -> AuthZ.checkBranchGet mayCallerUserId branch `ifUnauthorized` UCMProjects.GetProjectBranchResponseUnauthorized
    Right release -> AuthZ.checkReleaseGet mayCallerUserId release `ifUnauthorized` UCMProjects.GetProjectBranchResponseUnauthorized

  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId (user_id <$> mayContributor)
  let codebaseEnv = Codebase.codebaseEnv authZReceipt codebaseLoc

  (branchHead, maySquashedBranchHead) <- case branchOrRelease of
    Left Branch {causal = causalId} -> do
      maySquashedBranchHead <-
        if includeSquashedHead
          then do
            Codebase.runCodebaseTransaction codebaseEnv $ do
              maySquashedCausalId <- Codebase.squashCausalAndAddToCodebase causalId
              -- Join in the hash
              for maySquashedCausalId \cid -> (cid,) <$> HashQ.expectCausalHashesByIdsOf id cid
          else pure Nothing
      pure (causalId, maySquashedBranchHead)
    Right Release {squashedCausal = squashedCausalId} -> do
      Codebase.runCodebaseTransaction codebaseEnv $ do
        squashedCausalHash <- HashQ.expectCausalHashesByIdsOf id squashedCausalId
        pure (squashedCausalId, Just (squashedCausalId, squashedCausalHash))

  let causalIdsToImport = List.nubOrd $ [branchHead] ++ maybeToList (fst <$> maySquashedBranchHead)
  let codebaseOwnerUserId = Codebase.codebaseOwner codebaseEnv
  case mayCallerUserId of
    Nothing -> pure ()
    Just callerUserId -> lift $ for_ causalIdsToImport (importCausalToCallerCodebase authZReceipt codebaseOwnerUserId callerUserId)

  branchHeadHash <- Codebase.runCodebaseTransaction codebaseEnv $ do
    branchHeadHash <- HashQ.expectCausalHashesByIdsOf id branchHead
    pure branchHeadHash
  signedBranchHead <- lift $ HashJWT.signHashForUser mayCallerUserId (causalHashToHash32 branchHeadHash)
  maySignedSquashedBranchHead <- for maySquashedBranchHead $ \(_id, squashedBranchHead) -> lift $ HashJWT.signHashForUser mayCallerUserId (causalHashToHash32 squashedBranchHead)
  let contributorHandle = handle <$> mayContributor
  let projectShortHand = ProjectShortHand {userHandle = projectOwnerUserHandle, projectSlug = slug project}
  let branchShortHand :: Text = case branchOrRelease of
        Left Branch {branchName} -> IDs.toText BranchShortHand {contributorHandle, branchName}
        Right Release {version} -> IDs.toText ReleaseShortHand {releaseVersion = version}
  let branchId :: Text
      branchId = case branchOrRelease of
        Left Branch {branchId} -> IDs.toText branchId
        Right Release {releaseId} -> IDs.toText releaseId
  let apiBranch =
        UCMProjects.ProjectBranch
          { branchId = IDs.toText branchId,
            branchName = branchShortHand,
            projectId = IDs.toText projectId,
            projectName = IDs.toText projectShortHand,
            branchHead = signedBranchHead,
            squashedBranchHead = maySignedSquashedBranchHead
          }
  pure $ UCMProjects.GetProjectBranchResponseSuccess apiBranch
  where
    hoistMaybe = MaybeT . pure
    branchByName :: ProjectId -> MaybeT WebApp (Either (Branch CausalId) (Release CausalId UserId))
    branchByName projectId = do
      branchNameTxt <- hoistMaybe mayBranchName
      shortHand :: Either BranchShortHand ReleaseShortHand <-
        lift $
          case parseQueryParam @ReleaseShortHand branchNameTxt of
            Right r -> pure (Right r)
            Left _ -> Left <$> parseParam @BranchShortHand "branchName" branchNameTxt
      case shortHand of
        Left branchShorthand -> fmap Left . mapMaybeT PG.runTransaction $ do
          branch@Branch {branchId} <- MaybeT $ Q.branchByProjectIdAndShortHand projectId branchShorthand
          lift $ Q.incrementBranchDownloads branchId
          pure branch
        Right releaseShorthand -> do
          fmap Right . mapMaybeT PG.runTransaction $ do
            release@Release {releaseId} <- MaybeT $ Q.releaseByProjectIdAndReleaseShortHand projectId releaseShorthand
            lift $ Q.incrementReleaseDownloads releaseId
            pure release
    branchById :: MaybeT WebApp (Branch CausalId)
    branchById = do
      ucmBranchId <- hoistMaybe mayBranchId
      branchId <- lift $ parseParam @BranchId "branchId" ucmBranchId
      MaybeT $ PG.runTransaction $ Q.branchById branchId
    importCausalToCallerCodebase :: AuthZ.AuthZReceipt -> UserId -> UserId -> CausalId -> WebApp ()
    importCausalToCallerCodebase authZReceipt fromUserId destinationUserId causalId = do
      void . UnliftIO.forkIO . Metrics.recordBackgroundImportDuration . UnliftIO.handleAny errHandler . void . UnliftIO.timeout backgroundImportTimeout $ do
        let destCodebaseLoc = Codebase.codebaseLocationForUserCodebase destinationUserId
        let codebaseEnv = Codebase.codebaseEnv authZReceipt destCodebaseLoc
        Logging.logInfoText $ "Importing causal " <> tShow causalId <> " from user " <> tShow fromUserId <> "to user " <> tShow destinationUserId
        Codebase.runCodebaseTransaction codebaseEnv $ do
          Codebase.importCausalIntoCodebase fromUserId causalId
      where
        errHandler :: UnliftIO.SomeException -> WebApp ()
        errHandler e = Errors.reportError $ Errors.InternalServerError "background-import-error" e
        backgroundImportTimeout :: Int
        backgroundImportTimeout = 1000000 * 60 * 5 -- 5 minutes, most imports should only be a few seconds

createProjectBranchOrReleaseEndpoint :: Maybe Session -> UCMProjects.CreateProjectBranchRequest -> WebApp UCMProjects.CreateProjectBranchResponse
createProjectBranchOrReleaseEndpoint
  session
  (UCMProjects.CreateProjectBranchRequest {projectId, branchName = branchShorthandTxt, branchCausalHash, branchMergeTarget = mayBranchMergeInfo}) = toResponse do
    callerUserId <- lift $ AuthN.requireAuthenticatedUser session
    projectId <- lift $ parseParam @ProjectId "projectId" projectId
    lift $ addRequestTag "project-id" (IDs.toText projectId)
    case parseBranchOrReleaseShorthand branchShorthandTxt of
      Nothing -> lift . respondError $ InvalidParam {paramName = "branchName", param = branchShorthandTxt, parseError = "Invalid branch shorthand. Must be of the form 'name', '@contributor/name', or 'releases/1.2.3'"}
      Just (Left branchShorthand) -> lift $ createProjectBranch callerUserId projectId (hash32ToCausalHash branchCausalHash) branchShorthand mayBranchMergeInfo
      Just (Right (ReleaseShortHand releaseName)) -> lift $ createProjectRelease callerUserId projectId (hash32ToCausalHash branchCausalHash) releaseName
    where
      parseBranchOrReleaseShorthand :: Text -> Maybe (Either BranchShortHand ReleaseShortHand)
      parseBranchOrReleaseShorthand txt =
        fmap Right (eitherToMaybe (IDs.fromText @ReleaseShortHand txt))
          <|> fmap Left (eitherToMaybe (IDs.fromText @BranchShortHand txt))

createProjectBranch :: UserId -> ProjectId -> CausalHash -> BranchShortHand -> Maybe UCMProjects.ProjectBranchIds -> WebApp UCMProjects.CreateProjectBranchResponse
createProjectBranch
  callerUserId
  projectId
  branchCausalHash
  branchShortHand@BranchShortHand {contributorHandle, branchName}
  mayBranchMergeInfo =
    toResponse do
      lift $ addRequestTag "branch-shorthand" (IDs.toText branchShortHand)
      (project@Project {projectId, ownerUserId, slug}, mayContributorUserId) <- pgT $ do
        project <- Q.projectById projectId `orThrow` UCMProjects.CreateProjectBranchResponseNotFound (UCMProjects.NotFound "Project not found")
        mayContributorUserId <- for contributorHandle \ch -> do
          User {user_id = contributorUserId} <- UserQ.userByHandle ch `orThrow` UCMProjects.CreateProjectBranchResponseNotFound (UCMProjects.NotFound "Contributor not found")
          pure contributorUserId
        pure (project, mayContributorUserId)
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase ownerUserId mayContributorUserId
      authzReceipt <- AuthZ.checkBranchCreate callerUserId project mayContributorUserId `ifUnauthorized` UCMProjects.CreateProjectBranchResponseUnauthorized
      let codebase = Codebase.codebaseEnv authzReceipt codebaseLoc
      mayMergeMergeTargetBranchId <- case mayBranchMergeInfo of
        Nothing -> pure Nothing
        Just (UCMProjects.ProjectBranchIds {branchId = mergeTargetBranch}) ->
          -- If the branch was forked from a release, we can't merge it back into the release,
          -- so we just treat it like an empty merge target.
          case parseQueryParam @ReleaseId mergeTargetBranch of
            Right {} -> pure Nothing
            Left {} -> do
              Just <$> lift (parseParam @BranchId "merge-target-branch-id" mergeTargetBranch)

      -- Mitigation for the sync bug where sometimes causals get stuck in temp.
      branchCausalId <- (lift $ SyncQ.ensureCausalIsFlushed codebase branchCausalHash) `whenNothingM` throwError (UCMProjects.CreateProjectBranchResponseMissingCausalHash (causalHashToHash32 branchCausalHash))
      nameLookupReceipt <- lift . Codebase.runCodebaseTransaction codebase $ do
        bhId <- HashQ.expectNamespaceIdsByCausalIdsOf id branchCausalId
        NLOps.ensureNameLookupForBranchId bhId
      signedBranchHead <- lift $ HashJWT.signHashForUser (Just callerUserId) (causalHashToHash32 branchCausalHash)
      apiBranch <- pgT $ do
        branchId <- Q.createBranch nameLookupReceipt projectId branchName mayContributorUserId branchCausalId mayMergeMergeTargetBranchId callerUserId
        User {handle = projectOwnerHandle} <- UserQ.userByUserId ownerUserId `orThrow` UCMProjects.CreateProjectBranchResponseNotFound (UCMProjects.NotFound "Project owner not found")
        let projectShortHand =
              ProjectShortHand
                { userHandle = projectOwnerHandle,
                  projectSlug = slug
                }
        pure $
          UCMProjects.ProjectBranch
            { branchId = IDs.toText branchId,
              branchName = IDs.toText @BranchShortHand branchShortHand,
              branchHead = signedBranchHead,
              squashedBranchHead = Nothing,
              projectId = IDs.toText projectId,
              projectName = IDs.toText projectShortHand
            }
      pure $ UCMProjects.CreateProjectBranchResponseSuccess apiBranch

createProjectRelease :: UserId -> ProjectId -> CausalHash -> ReleaseVersion -> WebApp UCMProjects.CreateProjectBranchResponse
createProjectRelease callerUserId projectId unsquashedCausalHash releaseName = toResponse do
  lift $ addRequestTag "release-name" (IDs.toText releaseName)
  project@Project {projectId, ownerUserId, slug} <- pgT $ do
    Q.projectById projectId `orThrow` UCMProjects.CreateProjectBranchResponseNotFound (UCMProjects.NotFound "Project not found")
  let codebaseLoc = Codebase.codebaseLocationForProjectRelease ownerUserId
  authzReceipt <- AuthZ.checkReleaseCreate callerUserId project `ifUnauthorized` UCMProjects.CreateProjectBranchResponseUnauthorized
  let codebase = Codebase.codebaseEnv authzReceipt codebaseLoc

  -- Mitigation for the sync bug where sometimes causals get stuck in temp.
  unsquashedCausalId <- lift (SyncQ.ensureCausalIsFlushed codebase unsquashedCausalHash) `whenNothingM` throwError (UCMProjects.CreateProjectBranchResponseMissingCausalHash (causalHashToHash32 unsquashedCausalHash))
  squashResult :: Either (InternalServerError Text) (NameLookupReceipt, CausalId, CausalHash) <-
    lift . Codebase.tryRunCodebaseTransaction codebase $ do
      squashedCausalId <-
        Codebase.squashCausalAndAddToCodebase unsquashedCausalId
          `whenNothingM` do
            throwError (InternalServerError @Text "squash-failed" "Failed to squash causal on release publish")
      squashedNamespaceHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id squashedCausalId
      nlReceipt <- Codebase.ensureNameLookupForBranchId squashedNamespaceHashId
      squashedCausalHash <- HashQ.expectCausalHashesByIdsOf id squashedCausalId
      pure (nlReceipt, squashedCausalId, squashedCausalHash)
  (nlReceipt, squashedCausalId, squashedCausalHash) <- either (lift . respondError) pure squashResult
  signedSquashedCausalHash <- lift $ HashJWT.signHashForUser (Just callerUserId) (causalHashToHash32 squashedCausalHash)
  apiRelease <- pgT $ do
    Release {releaseId} <- Q.createRelease nlReceipt projectId releaseName squashedCausalId unsquashedCausalId callerUserId
    User {handle = projectOwnerHandle} <- UserQ.userByUserId ownerUserId `orThrow` UCMProjects.CreateProjectBranchResponseNotFound (UCMProjects.NotFound "Project owner not found")
    let projectShortHand =
          ProjectShortHand
            { userHandle = projectOwnerHandle,
              projectSlug = slug
            }
    pure $
      UCMProjects.ProjectBranch
        { branchId = IDs.toText releaseId,
          branchName = IDs.toText (ReleaseShortHand releaseName),
          branchHead = signedSquashedCausalHash,
          -- Release heads are always squashed
          squashedBranchHead = Just signedSquashedCausalHash,
          projectId = IDs.toText projectId,
          projectName = IDs.toText projectShortHand
        }
  pure $ UCMProjects.CreateProjectBranchResponseSuccess apiRelease

setProjectBranchOrReleaseHeadEndpoint :: Maybe Session -> UCMProjects.SetProjectBranchHeadRequest -> WebApp UCMProjects.SetProjectBranchHeadResponse
setProjectBranchOrReleaseHeadEndpoint session (UCMProjects.SetProjectBranchHeadRequest {projectId, branchId = branchLikeId, branchOldCausalHash, branchNewCausalHash}) = toResponse $ do
  callerUserId <- lift $ AuthN.requireAuthenticatedUser session
  projectId <- lift $ parseParam @ProjectId "projectId" projectId
  lift $ addRequestTag "project-id" (IDs.toText projectId)
  case parseBranchOrReleaseId branchLikeId of
    Nothing -> lift . respondError $ InvalidParam {paramName = "branchId", param = branchLikeId, parseError = "Invalid branch or release ID"}
    Just (Left branchId) -> lift $ setProjectBranchHead callerUserId projectId branchId (hash32ToCausalHash <$> branchOldCausalHash) (hash32ToCausalHash branchNewCausalHash)
    Just (Right _releaseId) -> throwError UCMProjects.SetProjectBranchHeadResponsePublishedReleaseIsImmutable
  where
    -- UCM treats releases like branches, but we can discriminate between them by the UUID prefix.
    parseBranchOrReleaseId :: Text -> Maybe (Either BranchId ReleaseId)
    parseBranchOrReleaseId txt =
      (fmap Left . eitherToMaybe $ IDs.fromText @BranchId txt)
        <|> (fmap Right . eitherToMaybe $ IDs.fromText @ReleaseId txt)

-- | Set the head of a branch to a new hash.
setProjectBranchHead :: UserId -> ProjectId -> BranchId -> (Maybe CausalHash) -> CausalHash -> WebApp UCMProjects.SetProjectBranchHeadResponse
setProjectBranchHead callerUserId projectId branchId mayOldCausalHash newCausalHash = toResponse do
  lift $ addRequestTag "branch-id" (IDs.toText branchId)
  (Project {ownerUserId}, branch@Branch {contributorId}) <- pgT do
    branch <- (Q.branchById branchId) `orThrow` UCMProjects.SetProjectBranchHeadResponseNotFound (UCMProjects.NotFound "Branch not found")
    project <- (Q.projectById projectId) `orThrow` UCMProjects.SetProjectBranchHeadResponseNotFound (UCMProjects.NotFound "Project not found")
    pure (project, branch)
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase ownerUserId contributorId
  authZReceipt <- AuthZ.checkBranchSet callerUserId branch `ifUnauthorized` UCMProjects.SetProjectBranchHeadResponseUnauthorized
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  -- Mitigation for the sync bug where sometimes causals get stuck in temp.
  newCausalId <- lift (SyncQ.ensureCausalIsFlushed codebase newCausalHash) `whenNothingM` throwError (UCMProjects.SetProjectBranchHeadResponseMissingCausalHash (causalHashToHash32 newCausalHash))
  pgT $ do
    -- Refetch the branch now that we're ready to update so we know its in the same transaction.
    Branch {causal = currentCausalId} <- Q.branchById branchId `orThrow` UCMProjects.SetProjectBranchHeadResponseNotFound (UCMProjects.NotFound "Branch not found")
    currentCausalHash <- CausalQ.expectCausalHashesByIdsOf id currentCausalId
    mayOldCausal <- Codebase.codebaseMToTransaction codebase $ do
      for mayOldCausalHash \oldCausalHash -> (oldCausalHash,) <$> Codebase.expectCausalIdByHash oldCausalHash
    case mayOldCausal of
      Just (expectedCausalHash, expectedCausalId)
        | expectedCausalId /= currentCausalId -> throwError $ UCMProjects.SetProjectBranchHeadResponseExpectedCausalHashMismatch (causalHashToHash32 expectedCausalHash) (causalHashToHash32 currentCausalHash)
      _
        | currentCausalId == newCausalId ->
            -- No-op
            pure ()
        | otherwise -> do
            User {handle = callingUserHandle} <- UserQ.userByUserId callerUserId `orThrow` UCMProjects.SetProjectBranchHeadResponseNotFound (UCMProjects.NotFound "User not found")
            let description = "Pushed by " <> (IDs.toText $ PrefixedID @"@" callingUserHandle)
            newNamespaceId <- HashQ.expectNamespaceIdsByCausalIdsOf id newCausalId
            nlReceipt <- NLOps.ensureNameLookupForBranchId newNamespaceId
            Q.setBranchCausalHash nlReceipt description callerUserId branchId newCausalId
            -- Update any affected contributions to reflect the result of updating this branch.
            MergeDetection.updateContributionsFromBranchUpdate callerUserId branchId
  pure $ UCMProjects.SetProjectBranchHeadResponseSuccess

-- Helpers for UCM endpoints

-- | Helper for safely running a PG transaction while propagating any errors.
-- This is handy for all the UCM endpoints which return custom types on errors.
pgT :: PG.Transaction e a -> ExceptT e WebApp a
pgT = ExceptT . PG.tryRunTransaction

-- | Helper for easily converting a failed lookup into the appropriate return type.
orThrow :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrow m e = m `whenNothingM` throwError e

-- | Helper for easily converting a failed auth check into the appropriate return type.
ifUnauthorized :: (Monad m) => m (Either AuthZ.AuthZFailure a) -> (UCMProjects.Unauthorized -> e) -> ExceptT e m a
ifUnauthorized m toErr =
  lift m >>= \case
    Left authzFailure -> throwError . toErr . UCMProjects.Unauthorized $ AuthZ.authZFailureMessage authzFailure
    Right a -> pure a

-- | UCM handlers use the same type for success and failure, this unifies them
toResponse :: (Functor m) => ExceptT a m a -> m a
toResponse = fmap (either id id) . runExceptT

-- | Given the project and optional branch merge target,
--   find the causal hash we have an index for already which is likely to be closest
--   to the new branch.
--
--   If we have a merge target, we can assume the new branch is likely forked off of it and
--   thus it should be a good place to start from, if there's no merge target, we can simply use the
--   project's main branch.
getBestNameLookupBase :: ProjectId -> Maybe BranchId -> WebApp (Maybe CausalId)
getBestNameLookupBase projectId mayMergeTargetBranchId = runMaybeT do
  mergeTargetCausalHash <|> defaultBranchCausalHash
  where
    hoistMaybe = MaybeT . pure
    mergeTargetCausalHash = do
      mergeTargetBranchId <- hoistMaybe mayMergeTargetBranchId
      mergeTargetBranch <- MaybeT . PG.runTransaction $ Q.branchById mergeTargetBranchId
      pure $ Branch.causal mergeTargetBranch
    defaultBranchCausalHash = do
      mainBranch <- MaybeT $ PG.runTransaction $ Q.branchByProjectIdAndShortHand projectId defaultBranchShorthand
      pure $ Branch.causal mainBranch
