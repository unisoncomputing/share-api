{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.UCM.Sync.Impl
  ( server,
    -- Only exported for migration purposes.
    -- This export can be removed once we've migrated away from sqlite.
    insertEntitiesToCodebase,
    ensureCausalIsFlushed,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NEL
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Servant
import Share.App
import Share.Codebase (CodebaseM)
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseEnv)
import Share.Env qualified as Env
import Share.IDs (ProjectBranchShortHand (..), ProjectReleaseShortHand (..), ProjectShortHand (..), UserHandle, UserId)
import Share.IDs qualified as IDs
import Share.OAuth.Session (Session (..))
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.Queries qualified as PGQ
import Share.Postgres.Sync.Queries (entityLocations)
import Share.Postgres.Sync.Queries qualified as SyncQ
import Share.Prelude
import Share.Project (Project (..))
import Share.User (User (..))
import Share.Utils.Logging
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant (parseParam)
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.UCM.Sync.HashJWT qualified as HashJWT
import Share.Web.UCM.Sync.Types (EntityBunch (..), EntityKind (..), entityKind)
import U.Codebase.Causal qualified as Causal
import U.Codebase.Sqlite.Orphans ()
import Unison.Codebase.Path qualified as Path
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.NameSegment.Internal qualified as UNameSegment
import Unison.Share.API.Hash (HashJWTClaims (..))
import Unison.Share.API.Hash qualified as Hash
import Unison.Sync.API qualified as Sync
import Unison.Sync.Common (causalHashToHash32)
import Unison.Sync.EntityValidation qualified as Sync
import Unison.Sync.Types (DownloadEntitiesError (..), DownloadEntitiesRequest (..), DownloadEntitiesResponse (..), GetCausalHashByPathRequest (..), GetCausalHashByPathResponse (..), NeedDependencies (..), RepoInfo (..), UploadEntitiesError (..), UploadEntitiesRequest (..), UploadEntitiesResponse (..))
import Unison.Sync.Types qualified as Share
import Unison.Sync.Types qualified as Sync
import UnliftIO qualified

data RepoInfoKind
  = RepoInfoUser UserHandle
  | RepoInfoProjectBranch ProjectBranchShortHand
  | RepoInfoProjectRelease ProjectReleaseShortHand
  deriving stock (Show)

-- | Parse a `RepoInfo` into the correct codebase view, e.g.
--
-- >>> repoInfoKind (RepoInfo "@unison")
-- Right (RepoInfoUser (UserHandle "unison"))
--
-- We don't currently support 'just' a project, since you can't upload code without a branch.
-- >>> repoInfoKind (RepoInfo "@unison/base")
-- Left "Invalid repo info: @unison/base"
--
-- >>> repoInfoKind (RepoInfo "@unison/base/main")
-- Right (RepoInfoProjectBranch (ProjectBranchShortHand {userHandle = UserHandle "unison", projectSlug = base, contributorHandle = Nothing, branchName = BranchName "main"}))
--
-- >>> repoInfoKind (RepoInfo "@unison/base/@runarorama/main")
-- Right (RepoInfoProjectBranch (ProjectBranchShortHand {userHandle = UserHandle "unison", projectSlug = base, contributorHandle = Just (UserHandle "runarorama"), branchName = BranchName "main"}))
--
-- >>> repoInfoKind (RepoInfo "@unison/base/releases/1.0.0")
-- Right (RepoInfoProjectRelease (ProjectReleaseShortHand {userHandle = UserHandle "unison", projectSlug = base, releaseName = ReleaseName "1.0.0"}))
repoInfoKind :: RepoInfo -> Either Text RepoInfoKind
repoInfoKind (RepoInfo repoInfo) =
  case parseRelease <|> parseBranch <|> parseUser of
    Just a -> Right a
    Nothing -> Left $ "Invalid repo info: " <> repoInfo
  where
    parseBranch :: Maybe RepoInfoKind
    parseBranch = fmap RepoInfoProjectBranch . eitherToMaybe $ IDs.fromText @ProjectBranchShortHand repoInfo
    parseRelease :: Maybe RepoInfoKind
    parseRelease = fmap RepoInfoProjectRelease . eitherToMaybe $ IDs.fromText @ProjectReleaseShortHand repoInfo
    parseUser :: Maybe RepoInfoKind
    parseUser = fmap RepoInfoUser . eitherToMaybe $ do
      IDs.PrefixedID uh <- IDs.fromText @(IDs.PrefixedID "@" UserHandle) repoInfo
      pure uh

-- swimlanes: https://swimlanes.io/u/cWc0DbQq9

server :: Maybe Session -> ServerT Sync.API WebApp
server (Just Session {sessionUserId}) =
  getCausalHashByPathEndpoint (Just sessionUserId)
    :<|> downloadEntitiesEndpoint (Just sessionUserId)
    :<|> uploadEntitiesEndpoint sessionUserId
server _ =
  getCausalHashByPathEndpoint Nothing
    :<|> downloadEntitiesEndpoint Nothing
    :<|> err
  where
    err :: a -> WebApp b
    err _ = respondError AuthN.UnauthenticatedError

getCausalHashByPathEndpoint :: Maybe UserId -> GetCausalHashByPathRequest -> WebApp GetCausalHashByPathResponse
getCausalHashByPathEndpoint callerUserId (GetCausalHashByPathRequest sharePath) =
  either id id <$> runExceptT do
    let repoInfo = Share.pathRepoInfo sharePath
        localPath = looseCodeCodebasePath sharePath
    addRequestTag "repo-info" (unRepoInfo repoInfo)
    IDs.PrefixedID userHandle <- lift . parseParam @(IDs.PrefixedID "@" UserHandle) "path" $ unRepoInfo repoInfo
    codebaseOwner@User {user_id = codebaseOwnerUserId} <- ExceptT . PG.tryRunTransaction $ do
      PGQ.userByHandle userHandle `whenNothingM` throwError Share.GetCausalHashByPathUserNotFound
    let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
    mayCausalAtPath <-
      lift (AuthZ.checkReadUserCodebase callerUserId codebaseOwner localPath) >>= \case
        Left {} -> throwError (GetCausalHashByPathNoReadPermission sharePath)
        Right authZReceipt -> do
          let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
          lift . Codebase.runCodebaseTransaction codebase $ do
            (codebaseLooseCodeRootCausalId, _) <- Codebase.expectLooseCodeRoot
            Codebase.loadCausalNamespaceAtPath codebaseLooseCodeRootCausalId localPath
    case mayCausalAtPath of
      Nothing -> pure (GetCausalHashByPathSuccess Nothing)
      Just causalAtPath -> do
        hashJwt <- lift $ HashJWT.signHashForUser callerUserId (causalHashToHash32 (Causal.causalHash causalAtPath))
        pure (GetCausalHashByPathSuccess $ Just hashJwt)

downloadEntitiesEndpoint :: Maybe UserId -> DownloadEntitiesRequest -> WebApp DownloadEntitiesResponse
downloadEntitiesEndpoint mayUserId DownloadEntitiesRequest {repoInfo, hashes = hashJWTs} =
  either id id <$> runExceptT do
    addRequestTag "repo-info" (unRepoInfo repoInfo)
    codebase <-
      case repoInfoKind repoInfo of
        Left err -> throwError (DownloadEntitiesFailure $ DownloadEntitiesInvalidRepoInfo err repoInfo)
        Right (RepoInfoUser userHandle) -> do
          User {user_id = repoOwnerUserId} <- lift (PG.runTransaction (PGQ.userByHandle userHandle)) `whenNothingM` throwError (DownloadEntitiesFailure . DownloadEntitiesUserNotFound $ IDs.toText @UserHandle userHandle)
          authZToken <- lift AuthZ.checkDownloadFromUserCodebase `whenLeftM` \_err -> throwError (DownloadEntitiesFailure $ DownloadEntitiesNoReadPermission repoInfo)
          let codebaseLoc = Codebase.codebaseLocationForUserCodebase repoOwnerUserId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
        Right (RepoInfoProjectBranch ProjectBranchShortHand {userHandle, projectSlug, contributorHandle}) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
            project <- (PGQ.projectByShortHand projectShortHand) `whenNothingM` throwError (DownloadEntitiesFailure . DownloadEntitiesProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            mayContributorUserId <- for contributorHandle \ch -> fmap user_id $ (PGQ.userByHandle ch) `whenNothingM` throwError (DownloadEntitiesFailure . DownloadEntitiesUserNotFound $ IDs.toText @UserHandle ch)
            pure (project, mayContributorUserId)
          authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (DownloadEntitiesFailure $ DownloadEntitiesNoReadPermission repoInfo)
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
        Right (RepoInfoProjectRelease ProjectReleaseShortHand {userHandle, projectSlug}) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
            project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (DownloadEntitiesFailure . DownloadEntitiesProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            pure (project, Nothing)
          authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (DownloadEntitiesFailure $ DownloadEntitiesNoReadPermission repoInfo)
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
    Env.Env {maxParallelismPerDownloadRequest} <- ask
    entities <- fmap NEMap.fromList . lift $ do
      UnliftIO.pooledForConcurrentlyN maxParallelismPerDownloadRequest (NESet.toList hashJWTs) (verifyAndFetchEntity codebase)
    -- Signing is sufficiently slow to sign in parallel
    signedEntities <-
      entities
        & traverseOf (unsafePartsOf (traverse . Share.entityHashes_)) \hashes -> lift do
          UnliftIO.pooledForConcurrentlyN maxParallelismPerDownloadRequest hashes \hash -> do
            HashJWT.signHashForUser mayUserId hash
    pure (DownloadEntitiesSuccess signedEntities)
  where
    verifyAndFetchEntity :: CodebaseEnv -> Hash.HashJWT -> WebApp (Hash32, Share.Entity Text Hash32 Hash32)
    verifyAndFetchEntity codebase hashJWT = do
      HashJWT.verifyHashJWT mayUserId hashJWT >>= \case
        Left ae -> respondError ae
        Right HashJWTClaims {hash} -> do
          entity <- Codebase.runCodebaseTransactionMode PG.ReadCommitted codebase $ SyncQ.expectEntity hash
          pure (hash, entity)

uploadEntitiesEndpoint :: UserId -> UploadEntitiesRequest -> WebApp UploadEntitiesResponse
uploadEntitiesEndpoint callingUserId (UploadEntitiesRequest {repoInfo, entities}) = do
  either id id <$> runExceptT do
    addRequestTag "repo-info" (unRepoInfo repoInfo)
    codebase <-
      case repoInfoKind repoInfo of
        Left err -> throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'InvalidRepoInfo err repoInfo)
        Right (RepoInfoUser userHandle) -> do
          User {user_id = repoOwnerUserId} <- lift (PG.runTransaction (PGQ.userByHandle userHandle)) `whenNothingM` throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'UserNotFound $ IDs.toText @UserHandle userHandle)
          authZToken <- lift (AuthZ.checkUploadToUserCodebase callingUserId repoOwnerUserId) `whenLeftM` \_err -> throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'NoWritePermission repoInfo)
          let codebaseLoc = Codebase.codebaseLocationForUserCodebase repoOwnerUserId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
        Right (RepoInfoProjectBranch ProjectBranchShortHand {userHandle, projectSlug, contributorHandle}) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {projectId, ownerUserId = projectOwnerUserId}, mayContributorUserId) <- ExceptT . PG.tryRunTransaction $ do
            project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'ProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            mayContributorUserId <- for contributorHandle \ch -> fmap user_id $ (PGQ.userByHandle ch) `whenNothingM` throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'UserNotFound $ IDs.toText @UserHandle ch)
            pure (project, mayContributorUserId)
          authZToken <- lift (AuthZ.checkUploadToProjectBranchCodebase callingUserId projectId mayContributorUserId) `whenLeftM` \_err -> throwError . Share.UploadEntitiesFailure $ Share.UploadEntitiesError'NoWritePermission repoInfo
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId mayContributorUserId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
        Right (RepoInfoProjectRelease ProjectReleaseShortHand {userHandle, projectSlug}) -> do
          let projectShortHand = ProjectShortHand {userHandle, projectSlug}
          (Project {projectId, ownerUserId = projectOwnerUserId}, mayContributorUserId) <- ExceptT . PG.tryRunTransaction $ do
            project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (Share.UploadEntitiesFailure $ Share.UploadEntitiesError'ProjectNotFound $ IDs.toText @ProjectShortHand projectShortHand)
            pure (project, Nothing)
          authZToken <- lift (AuthZ.checkUploadToProjectBranchCodebase callingUserId projectId mayContributorUserId) `whenLeftM` \_err -> throwError . Share.UploadEntitiesFailure $ Share.UploadEntitiesError'NoWritePermission repoInfo
          let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId mayContributorUserId
          pure $ Codebase.codebaseEnv authZToken codebaseLoc
    lift (insertEntitiesToCodebase codebase entities)
      <&> \case
        Left err -> UploadEntitiesFailure $ UploadEntitiesError'EntityValidationFailure err
        -- There were no remaining missing dependencies, so everything must have been
        -- flushed to main storage and we're done.
        Right Nothing -> UploadEntitiesSuccess
        -- There were missing dependencies, so we need to ask the client for them.
        Right (Just missingDeps) -> UploadEntitiesFailure $ Share.UploadEntitiesError'NeedDependencies (NeedDependencies missingDeps)

-- | Insert entities to the user's codebase (either temp storage or main), returning the hashes of any missing dependencies
-- we still need from the client.
insertEntitiesToCodebase ::
  (MonadLogger (AppM r)) =>
  CodebaseEnv ->
  NEMap Hash32 (Sync.Entity Text Hash32 Hash32) ->
  AppM r (Either Sync.EntityValidationError (Maybe (NESet Hash32)))
insertEntitiesToCodebase codebase entities = do
  Env.Env {maxParallelismPerUploadRequest} <- ask
  toIO <- UnliftIO.askRunInIO
  let isComponentHashMismatchAllowedIO expectedHash actualHash = toIO . PG.runTransaction $ HashQ.isComponentHashAllowedToBeMismatched expectedHash actualHash
  let isCausalHashMismatchAllowedIO expectedHash actualHash = toIO . PG.runTransaction $ HashQ.isCausalHashAllowedToBeMismatched expectedHash actualHash
  let entityBunch = groupEntities . NEMap.toList $ entities

  -- First import as many entities as possible from other codebases accessible to the caller.
  -- This is safe to run as a separate transaction.
  imported <- Codebase.runCodebaseTransaction codebase $ directlyImportDependencies entityBunch
  for_ imported \(causalId, userId) -> do
    Logging.logInfoText $ "Imported causal " <> tShow causalId <> " from user " <> tShow userId <> " to user " <> tShow (Codebase.codebaseOwner codebase)
  result <- Codebase.tryRunCodebaseTransaction codebase $ do
    locations <- entityLocations (fst <$> entityBunch)
    let (hashesAlreadyInTemp, _hashesInMain, unsavedHashes) = partitionLocations locations
    let unsavedEntities = unsavedHashes <&> \hash -> (hash, NEMap.toMap entities ^?! ix hash)
    mayErrs <- lift . PG.transactionUnsafeIO $ batchValidateEntities maxParallelismPerUploadRequest isComponentHashMismatchAllowedIO isCausalHashMismatchAllowedIO unsavedEntities
    case mayErrs of
      Nothing -> pure ()
      Just (err :| _errs) -> throwError err
    SyncQ.saveTempEntities unsavedEntities
    let hashesNowInTemp = Set.fromList (fst <$> Foldable.toList unsavedEntities) <> (Set.fromList . Foldable.toList $ hashesAlreadyInTemp)
    pure hashesNowInTemp

  case NESet.nonEmptySet <$> result of
    Left err -> pure $ Left err
    -- If all entities were already in main storage, then we're done.
    Right Nothing -> pure $ Right Nothing
    Right (Just hashesNowInTemp) -> do
      -- 'flushTempEntities' already loops on entities which may be ready to flush and should
      -- successfully clear them all, but:
      --
      -- 1. We definitely want each flush to be its own transaction so we don't thrash with
      --    serialization failures
      -- 2. Other requests may also be uploading and flushing entities
      --
      -- So we don't have a full transactional guarantee that we'll flush all entities in one
      -- go. Instead we'll just keep flushing until we have nothing left to flush. It's
      -- possible this worker will actually start flushing things uploaded by OTHER requests.
      --
      -- This is fine because that work would need to be done eventually anyways, and is
      -- better than the alternative where we might end up with things left in temp_entities.
      --
      -- TODO: Ensure we don't loop so long that we time-out.
      let flushLoop = do
            (mayNeededHashes, mayReadyToFlush) <- Codebase.runCodebaseTransaction codebase $ SyncQ.elaborateHashes hashesNowInTemp
            case mayReadyToFlush of
              Just readyToFlush -> do
                flushTempEntities codebase readyToFlush
                flushLoop
              Nothing -> pure mayNeededHashes
      mayNeededHashes <- flushLoop
      case mayNeededHashes of
        Nothing -> pure $ Right Nothing
        -- If some uploaded entities went to temp storage we may need to ask the client for missing dependencies.
        Just neededHashes -> pure $ Right (Just neededHashes)
  where
    partitionLocations :: EntityBunch (Hash32, Maybe SyncQ.EntityLocation) -> (EntityBunch Hash32, EntityBunch Hash32, EntityBunch Hash32)
    partitionLocations EntityBunch {causals, namespaces, terms, types, patches} =
      let sortLocation :: [(Hash32, Maybe SyncQ.EntityLocation)] -> ([Hash32], [Hash32], [Hash32])
          sortLocation = foldMap \case
            (hash, Nothing) -> (mempty, mempty, [hash])
            (hash, Just SyncQ.EntityInTempStorage) -> ([hash], mempty, mempty)
            (hash, Just SyncQ.EntityInMainStorage) -> (mempty, [hash], mempty)
          (causalsInTemp, causalsInMain, causalsNotSaved) = sortLocation causals
          (namespacesInTemp, namespacesInMain, namespacesNotSaved) = sortLocation namespaces
          (termsInTemp, termsInMain, termsNotSaved) = sortLocation terms
          (typesInTemp, typesInMain, typesNotSaved) = sortLocation types
          (patchesInTemp, patchesInMain, patchesNotSaved) = sortLocation patches
       in ( EntityBunch {causals = causalsInTemp, namespaces = namespacesInTemp, terms = termsInTemp, types = typesInTemp, patches = patchesInTemp},
            EntityBunch {causals = causalsInMain, namespaces = namespacesInMain, terms = termsInMain, types = typesInMain, patches = patchesInMain},
            EntityBunch {causals = causalsNotSaved, namespaces = namespacesNotSaved, terms = termsNotSaved, types = typesNotSaved, patches = patchesNotSaved}
          )

-- | Given a causal hash, check if it's in the codebase, if not, see if it's flushable from
-- temp_entities.
--
-- There's a bug somewhere in sync where sometimes entities that are ready to flush get stuck
-- in temp, and this can also happen if certain requests get interrupted.
-- Ideally we wouldn't need this, but it's easy enough to add and addresses this problem.
ensureCausalIsFlushed :: CodebaseEnv -> CausalHash -> AppM r (Maybe CausalId)
ensureCausalIsFlushed codebase causalHash = do
  Codebase.runCodebaseTransaction codebase (CausalQ.loadCausalIdByHash causalHash) >>= \case
    Just cid -> pure (Just cid)
    -- It's possible we have the causal in temp storage but just not flushed, so we'll try to flush it.
    Nothing -> do
      (_mayNeededHashes, mayReadyToFlush) <- Codebase.runCodebaseTransaction codebase $ SyncQ.elaborateHashes (NESet.singleton (into @Hash32 causalHash))
      case mayReadyToFlush of
        Just readyToFlush -> do
          flushTempEntities codebase readyToFlush
        Nothing -> pure ()
      Codebase.runCodebaseTransaction codebase (CausalQ.loadCausalIdByHash causalHash)

-- | Import as many entities as possible from other codebases accessible to the caller,
-- returning any dependencies we were NOT able to import.
directlyImportDependencies :: EntityBunch (Hash32, Share.Entity Text Hash32 Hash32) -> CodebaseM e [(CausalId, UserId)]
directlyImportDependencies EntityBunch {namespaces, causals} = do
  -- We don't care which children we imported or not, if we imported any of them, we'll
  -- automatically filter them out when checking missing dependencies.
  CausalQ.importAccessibleCausals (Set.fromList importableChildren <> importableCausals)
  where
    importableChildren =
      namespaces & foldMap \case
        (_hash, Sync.N (Sync.Namespace {childLookup})) -> snd <$> childLookup
        _ -> mempty
    importableCausals =
      causals & foldMap \case
        (hash, Sync.C (Sync.Causal {parents})) -> Set.insert hash parents
        _ -> mempty

groupEntities :: (Foldable f) => f ((Hash32, Share.Entity text hash hash')) -> EntityBunch (Hash32, Share.Entity text hash hash')
groupEntities = foldMap
  \(h, e) ->
    case entityKind e of
      CausalEntity -> mempty {causals = [(h, e)]}
      NamespaceEntity -> mempty {namespaces = [(h, e)]}
      TermEntity -> mempty {terms = [(h, e)]}
      TypeEntity -> mempty {types = [(h, e)]}
      PatchEntity -> mempty {patches = [(h, e)]}

-- | Find all entities which are in temp storage and are either not missing any
-- dependencies or whose missing deps are also in temp storage, then move them to main
-- storage and update any other entities who were depending on them.
--
-- Postcondition is that all entities in temp-storage have at least one dependency
-- which is not present in main storage.
--
-- Keep flushing to a fixed point (i.e. until we have no remaining entities that lack
-- missing dependencies)
flushTempEntities :: CodebaseEnv -> NESet Hash32 -> AppM r ()
flushTempEntities codebase hashesToCheck = do
  -- This is designed so that if multiple requests are working at once, they can
  -- all make progress using short transactions without accidentally working on the same
  -- entities.
  newMaybeFlushableHashes <-
    -- TODO: We still get a lot of PG serialization failures from this block :'(
    for (Foldable.toList hashesToCheck) \hashToCheck -> do
      Codebase.runCodebaseTransaction codebase $ do
        SyncQ.tryPopFlushableTempEntity hashToCheck >>= \case
          Nothing -> pure mempty
          Just (hash, te) -> do
            SyncQ.saveTempEntityInMain hash te
  case NESet.nonEmptySet (fold newMaybeFlushableHashes) of
    Nothing -> pure ()
    Just nonEmptyNewMaybeFlushableHashes -> flushTempEntities codebase nonEmptyNewMaybeFlushableHashes

-- | Validate entities in parallel, returning any validation errors.
batchValidateEntities ::
  (Traversable f) =>
  Int ->
  (ComponentHash -> ComponentHash -> IO Bool) ->
  (CausalHash -> CausalHash -> IO Bool) ->
  f (Hash32, Sync.Entity Text Hash32 Hash32) ->
  IO (Maybe (NonEmpty (Sync.EntityValidationError)))
batchValidateEntities maxParallelism checkIfComponentHashMismatchIsAllowed checkIfCausalHashMismatchIsAllowed entities = do
  errs <- UnliftIO.pooledForConcurrentlyN maxParallelism entities \(hash, entity) ->
    validateEntity checkIfComponentHashMismatchIsAllowed checkIfCausalHashMismatchIsAllowed hash entity
  pure . NEL.nonEmpty . catMaybes . Foldable.toList $ errs

validateEntity ::
  (Monad m) =>
  (ComponentHash -> ComponentHash -> m Bool) ->
  (CausalHash -> CausalHash -> m Bool) ->
  Hash32 ->
  Share.Entity Text Hash32 Hash32 ->
  m (Maybe Sync.EntityValidationError)
validateEntity checkIfComponentHashMismatchIsAllowed checkIfCausalHashMismatchIsAllowed hash entity = do
  case (Sync.validateEntity hash entity) of
    Just err@(Sync.EntityHashMismatch Sync.TermComponentType (Sync.HashMismatchForEntity {supplied = expectedHash, computed = actualHash})) ->
      checkIfComponentHashMismatchIsAllowed (ComponentHash . Hash32.toHash $ expectedHash) (ComponentHash . Hash32.toHash $ actualHash) >>= \case
        False -> pure (Just err)
        True -> pure Nothing
    Just err@(Sync.EntityHashMismatch Sync.CausalType (Sync.HashMismatchForEntity {supplied = expectedHash, computed = actualHash})) ->
      checkIfCausalHashMismatchIsAllowed (CausalHash . Hash32.toHash $ expectedHash) (CausalHash . Hash32.toHash $ actualHash) >>= \case
        False -> pure (Just err)
        True -> pure Nothing
    Just err ->
      -- This shouldn't happen unless the ucm client is buggy or malicious
      -- Either way, it's exceptional and we should know about it.
      pure (Just err)
    Nothing -> pure Nothing

-- | Get the actual codebase path from a Sync path, since the sync path has the user as the
-- first segment.
looseCodeCodebasePath :: Sync.Path -> Path.Path
looseCodeCodebasePath (Sync.Path (_ :| ps)) = Path.fromList (coerce ps)
