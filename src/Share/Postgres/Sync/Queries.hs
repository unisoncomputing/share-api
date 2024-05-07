-- | Queries related to sync and temp entities.
module Share.Postgres.Sync.Queries
  ( expectEntity,
    expectCausalEntity,
    entityLocations,
    saveTempEntityInMain,
    saveTempEntities,
    Share.EntityLocation (..),
    elaborateHashes,
    getEntitiesReadyToFlush,
    filterForFlushableHashes,
    tryPopFlushableTempEntity,
  )
where

import Control.Lens hiding (from)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Vector qualified as Vector
import Share.Codebase.Types (CodebaseM)
import Share.Codebase.Types qualified as Codebase
import Share.IDs
import Share.Postgres
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Definitions.Queries qualified as Defn
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Postgres.Definitions.Types (TermComponentElementBytes (TermComponentElementBytes))
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.Patches.Queries qualified as PatchQ
import Share.Postgres.Serialization qualified as S
import Share.Postgres.Sync.Conversions qualified as Cv
import Share.Postgres.Sync.Types (TypedTempEntity (..))
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (InternalServerError (..), ToServerError (..), Unimplemented (Unimplemented))
import Share.Web.UCM.Sync.Types
import Servant (ServerError (..), err500)
import U.Codebase.Branch qualified as V2
import U.Codebase.Causal qualified as U
import U.Codebase.Sqlite.Branch.Format (LocalBranchBytes (LocalBranchBytes))
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.Causal qualified as SqliteCausal
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Decode qualified as Decoders
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.LocalizeObject qualified as Localize
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull
import U.Codebase.Sqlite.Queries qualified as Share
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import Unison.Hash32
import Unison.Hash32 qualified as Hash32
import Unison.Sync.Common qualified as Share
import Unison.Sync.Types qualified as Share

data SyncQError
  = InvalidNamespaceBytes
  | InvalidPatchBytes
  | InvalidTermComponentBytes Decoders.DecodeError
  | InvalidDeclComponentBytes Decoders.DecodeError
  | ExpectedHashNotFound Hash32
  deriving (Show)

instance ToServerError SyncQError where
  toServerError = \case
    InvalidNamespaceBytes {} -> ("namespace-format-invalid", err500)
    InvalidPatchBytes {} -> ("patch-format-invalid", err500)
    InvalidTermComponentBytes {} -> ("term-format-invalid", err500)
    InvalidDeclComponentBytes {} -> ("decl-format-invalid", err500)
    ExpectedHashNotFound h -> ("expected-hash-not-found", err500 {errBody = BL.pack $ "Expected hash not found: " <> show h})

instance Logging.Loggable SyncQError where
  toLog = Logging.withSeverity Logging.Error . Logging.showLog

-- | Read an entity out of the database that we know is in main storage.
expectEntity :: HasCallStack => Hash32 -> CodebaseM e (Share.Entity Text Hash32 Hash32)
expectEntity hash = do
  expectEntityKindForHash hash >>= \case
    CausalEntity -> Share.C <$> expectCausalEntity (CausalHash . Hash32.toHash $ hash)
    NamespaceEntity -> Share.N <$> expectNamespaceEntity (BranchHash . Hash32.toHash $ hash)
    TermEntity -> Share.TC <$> expectTermComponentEntity (ComponentHash . Hash32.toHash $ hash)
    TypeEntity -> Share.DC <$> expectTypeComponentEntity (ComponentHash . Hash32.toHash $ hash)
    PatchEntity -> Share.P <$> expectPatchEntity (PatchHash . Hash32.toHash $ hash)
  where

expectCausalEntity :: HasCallStack => CausalHash -> CodebaseM e (Share.Causal Hash32)
expectCausalEntity hash = do
  causalId <- CausalQ.expectCausalIdByHash hash
  U.Causal {valueHash, parents} <- CausalQ.expectCausalNamespace causalId
  pure $
    ( Share.Causal
        { namespaceHash = Hash32.fromHash $ unBranchHash valueHash,
          parents = Set.map (Hash32.fromHash . unCausalHash) . Map.keysSet $ parents
        }
    )

expectNamespaceEntity :: HasCallStack => BranchHash -> CodebaseM e (Share.Namespace Text Hash32)
expectNamespaceEntity bh = do
  bhId <- HashQ.expectBranchHashId bh
  v2Branch <- CausalQ.expectNamespace bhId
  second Hash32.fromHash <$> branchToEntity v2Branch
  where
    branchToEntity branch = do
      branchFull <- Cv.branchV2ToBF branch
      let (BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup}, localBranch) = Localize.localizeBranchG branchFull
      let bytes = LocalBranchBytes $ S.encodeNamespace localBranch
      pure $
        Share.Namespace
          { textLookup = Vector.toList branchTextLookup,
            defnLookup = Vector.toList branchDefnLookup,
            patchLookup = Vector.toList branchPatchLookup,
            childLookup = Vector.toList branchChildLookup,
            bytes = bytes
          }

expectTermComponentEntity :: HasCallStack => ComponentHash -> CodebaseM e (Share.TermComponent Text Hash32)
expectTermComponentEntity hash = do
  chId <- HashQ.expectComponentHashId hash
  DefnQ.expectShareTermComponent chId

expectTypeComponentEntity :: HasCallStack => ComponentHash -> CodebaseM e (Share.DeclComponent Text Hash32)
expectTypeComponentEntity hash = do
  chId <- HashQ.expectComponentHashId hash
  DefnQ.expectShareTypeComponent chId

expectPatchEntity :: HasCallStack => PatchHash -> CodebaseM e (Share.Patch Text Hash32 Hash32)
expectPatchEntity patchHash = do
  patchId <- HashQ.expectPatchIdsOf id patchHash
  v2Patch <- PatchQ.expectPatch patchId
  patchToEntity v2Patch
  where
    patchToEntity :: V2.Patch -> CodebaseM e (Share.Patch Text Hash32 Hash32)
    patchToEntity patch = do
      let patchFull = Cv.patchV2ToPF patch
      let (PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup}, localPatch) = Localize.localizePatchG patchFull
      let bytes = S.encodePatch localPatch
      Share.Patch
        { textLookup = Vector.toList patchTextLookup,
          oldHashLookup = Vector.toList patchHashLookup,
          newHashLookup = Vector.toList patchDefnLookup,
          bytes
        }
        & Share.patchOldHashes_ %~ Hash32.fromHash
        & Share.patchNewHashes_ %~ Hash32.fromHash
        & pure

-- | Determine the kind of an arbitrary hash.
expectEntityKindForHash :: HasCallStack => Hash32 -> CodebaseM e EntityKind
expectEntityKindForHash h =
  do
    queryExpect1Row
      [sql|
      SELECT
        EXISTS(SELECT FROM causals WHERE hash = #{h}),
        EXISTS(SELECT FROM branch_hashes WHERE branch_hashes.base32 = #{h}),
        EXISTS(SELECT FROM terms JOIN component_hashes ON component_hashes.id = terms.component_hash_id WHERE component_hashes.base32 = #{h}),
        EXISTS(SELECT FROM types JOIN component_hashes ON component_hashes.id = types.component_hash_id WHERE component_hashes.base32 = #{h}),
        EXISTS(SELECT FROM patches WHERE hash = #{h})
    |]
    >>= \case
      (True, _, _, _, _) -> pure CausalEntity
      (_, True, _, _, _) -> pure NamespaceEntity
      (_, _, True, _, _) -> pure TermEntity
      (_, _, _, True, _) -> pure TypeEntity
      (_, _, _, _, True) -> pure PatchEntity
      _ -> lift . unrecoverableError $ InternalServerError "expected-hash-not-found" (ExpectedHashNotFound h)

-- | Efficiently determine the location of a batch of entities.
entityLocations :: EntityBunch Hash32 -> CodebaseM e (EntityBunch (Hash32, Maybe Share.EntityLocation))
entityLocations sortedEntities = do
  codebaseUserId <- asks Codebase.codebaseOwner
  causalLocations <- getCausalLocations codebaseUserId (causals sortedEntities)
  namespaceLocations <- getNamespaceLocations codebaseUserId (namespaces sortedEntities)
  termLocations <- getTermLocations codebaseUserId (terms sortedEntities)
  typeLocations <- getTypeLocations codebaseUserId (types sortedEntities)
  patchLocations <- getPatchLocations codebaseUserId (patches sortedEntities)
  pure
    EntityBunch
      { causals = causalLocations,
        namespaces = namespaceLocations,
        terms = termLocations,
        types = typeLocations,
        patches = patchLocations
      }
  where
    toEntityLocation :: UserId -> (Hash32, Bool, Bool) -> (Hash32, Maybe Share.EntityLocation)
    toEntityLocation _codebaseUserId = \(hash, inMain, inTemp) ->
      case (inMain, inTemp) of
        (True, True) ->
          -- TODO: This should never happen, but it does. Figure out why.
          -- error $ "entityLocationsOf: Entity in both main and temp storage: " <> show (hash, codebaseUserId)
          (hash, Just Share.EntityInMainStorage)
        (True, False) -> (hash, Just Share.EntityInMainStorage)
        (False, True) -> (hash, Just Share.EntityInTempStorage)
        (False, False) -> (hash, Nothing)

    getCausalLocations :: UserId -> [Hash32] -> CodebaseM e [(Hash32, Maybe Share.EntityLocation)]
    getCausalLocations codebaseUserId causalHashes = do
      queryListRows @(Hash32, Bool, Bool)
        [sql|
          WITH entities(hash) AS (
            SELECT * FROM ^{singleColumnTable causalHashes}
          )
        SELECT entities.hash,
          EXISTS(
          SELECT FROM causals causal
            JOIN causal_ownership co ON co.causal_id = causal.id
            WHERE causal.hash = entities.hash AND co.user_id = #{codebaseUserId}
          ),
          EXISTS(
            SELECT FROM temp_entity
              WHERE temp_entity.hash = entities.hash
                AND temp_entity.user_id = #{codebaseUserId}
            )
          FROM entities
        |]
        <&> fmap (toEntityLocation codebaseUserId)
    getNamespaceLocations :: UserId -> [Hash32] -> CodebaseM e [(Hash32, Maybe Share.EntityLocation)]
    getNamespaceLocations codebaseUserId namespaceHashes = do
      queryListRows @(Hash32, Bool, Bool)
        [sql|
          WITH entities(hash) AS (
            SELECT * FROM ^{singleColumnTable namespaceHashes}
          )
        SELECT entities.hash,
          EXISTS(
          SELECT FROM namespaces
            JOIN branch_hashes ON branch_hashes.id = namespaces.namespace_hash_id
            JOIN namespace_ownership ON namespace_ownership.namespace_hash_id = namespaces.namespace_hash_id
            WHERE branch_hashes.base32 = entities.hash AND namespace_ownership.user_id = #{codebaseUserId}
          ),
          EXISTS(
            SELECT FROM temp_entity
              WHERE temp_entity.hash = entities.hash
                AND temp_entity.user_id = #{codebaseUserId}
            )
          FROM entities
        |]
        <&> fmap (toEntityLocation codebaseUserId)
    getTermLocations :: UserId -> [Hash32] -> CodebaseM e [(Hash32, Maybe Share.EntityLocation)]
    getTermLocations codebaseUserId termHashes = do
      queryListRows @(Hash32, Bool, Bool)
        [sql|
          WITH entities(hash) AS (
            SELECT * FROM ^{singleColumnTable termHashes}
          )
        SELECT entities.hash,
          EXISTS(
          SELECT FROM terms
            JOIN sandboxed_terms ON sandboxed_terms.term_id = terms.id
            JOIN component_hashes ON component_hashes.id = terms.component_hash_id
            WHERE component_hashes.base32 = entities.hash
              AND sandboxed_terms.user_id = #{codebaseUserId}
          ),
          EXISTS(
            SELECT FROM temp_entity
              WHERE temp_entity.hash = entities.hash
                AND temp_entity.user_id = #{codebaseUserId}
            )
          FROM entities
        |]
        <&> fmap (toEntityLocation codebaseUserId)
    getTypeLocations :: UserId -> [Hash32] -> CodebaseM e [(Hash32, Maybe Share.EntityLocation)]
    getTypeLocations codebaseUserId typeHashes = do
      queryListRows @(Hash32, Bool, Bool)
        [sql|
          WITH entities(hash) AS (
            SELECT * FROM ^{singleColumnTable typeHashes}
          )
        SELECT entities.hash,
          EXISTS(
          SELECT FROM types
            JOIN component_hashes ON component_hashes.id = types.component_hash_id
            JOIN sandboxed_types ON sandboxed_types.type_id = types.id
            WHERE component_hashes.base32 = entities.hash
              AND sandboxed_types.user_id = #{codebaseUserId}
          ),
          EXISTS(
            SELECT FROM temp_entity
              WHERE temp_entity.hash = entities.hash
                AND temp_entity.user_id = #{codebaseUserId}
            )
          FROM entities
        |]
        <&> fmap (toEntityLocation codebaseUserId)
    getPatchLocations :: UserId -> [Hash32] -> CodebaseM e [(Hash32, Maybe Share.EntityLocation)]
    getPatchLocations codebaseUserId patchHashes = do
      queryListRows @(Hash32, Bool, Bool)
        [sql|
          WITH entities(hash) AS (
            SELECT * FROM ^{singleColumnTable patchHashes}
          )
        SELECT entities.hash,
          EXISTS(
          SELECT FROM patches
            JOIN patch_ownership ON patch_ownership.patch_id = patches.id
            WHERE patches.hash = entities.hash AND patch_ownership.user_id = #{codebaseUserId}
          ),
          EXISTS(
            SELECT FROM temp_entity
              WHERE temp_entity.hash = entities.hash
                AND temp_entity.user_id = #{codebaseUserId}
            )
          FROM entities
        |]
        <&> fmap (toEntityLocation codebaseUserId)

-- | Save a temp entity to the temp entities table, also tracking its missing dependencies.
-- You can pass ALL the dependencies of the temp entity, the query will determine which ones
-- are missing.
saveTempEntities :: Foldable f => f (Hash32, Share.Entity Text Hash32 Hash32) -> CodebaseM e ()
saveTempEntities entities = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  let tempEntities =
        Foldable.toList entities <&> \(hash, entity) ->
          let te = Share.entityToTempEntity id entity
           in (hash, te, Entity.entityType te, Share.entityDependencies entity)
  let tempEntitiesTable :: [(Hash32, TempEntity, TempEntityType)]
      tempEntitiesTable = tempEntities <&> \(hash, te, entityType, _) -> (hash, te, entityType)
  let dependenciesTable :: [(Hash32, Hash32)]
      dependenciesTable = tempEntities & foldMap (\(hash, _, _, deps) -> (hash,) <$> Set.toList deps)
  execute_
    [sql|
      WITH temp_entities (hash, bytes, kind) AS (
        SELECT * FROM ^{toTable tempEntitiesTable}
      )
      INSERT INTO temp_entity (user_id, hash, bytes, kind)
        SELECT #{codebaseOwnerUserId}, hash, bytes, kind::entity_kind FROM temp_entities
      ON CONFLICT DO NOTHING
    |]
  execute_
    [sql|
        WITH dependencies (dependent, dependency) AS (
          SELECT * FROM ^{toTable dependenciesTable}
        )
        INSERT INTO temp_entity_missing_dependency (user_id, dependent, dependency)
        SELECT #{codebaseOwnerUserId}, dependencies.dependent, dependencies.dependency
          FROM dependencies
          -- Filter out any dependencies that we already have in main storage.
          WHERE NOT have_hash_in_codebase(#{codebaseOwnerUserId}, dependencies.dependency)
          ON CONFLICT DO NOTHING
    |]

-- | Filter down the given hashes to ones which are ready to flush.
filterForFlushableHashes :: Set Hash32 -> CodebaseM e (Set Hash32)
filterForFlushableHashes hashesToCheck = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  queryListCol
    [sql|
    WITH hashes_to_check(hash) AS (
      SELECT * FROM ^{singleColumnTable (Set.toList hashesToCheck)}
    )
      SELECT hash FROM hashes_to_check
        WHERE NOT EXISTS(SELECT FROM temp_entity_missing_dependency missing_dep
                          WHERE missing_dep.dependent = hashes_to_check.hash
                            AND missing_dep.user_id = #{codebaseOwnerUserId}
                        )
  |]
    <&> Set.fromList

-- | Remove a single entity that's ready to flush from the temp entities table,
-- and return it. If the provided hash isn't ready to flush, don't return anything.
tryPopFlushableTempEntity :: Hash32 -> CodebaseM e (Maybe (Hash32, TempEntity))
tryPopFlushableTempEntity hashToCheck = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  query1Row
    [sql|
    DELETE FROM temp_entity
      WHERE temp_entity.user_id = #{codebaseOwnerUserId}
        AND temp_entity.hash = #{hashToCheck}
        AND NOT EXISTS(SELECT FROM temp_entity_missing_dependency missing_dep
                          WHERE missing_dep.dependent = #{hashToCheck}
                            AND missing_dep.user_id = #{codebaseOwnerUserId}
                        )
      RETURNING hash, kind, bytes
  |]
    <&> fmap \((Only hash) :. TypedTempEntity te) -> (hash, te)

-- | Delete missing-dependency rows for an entity which has been
-- saved to main storage, returning the set of dependents.
clearTempDependencies :: Hash32 -> CodebaseM e (Set Hash32)
clearTempDependencies hash = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  queryListCol @Hash32
    [sql|
    DELETE FROM temp_entity_missing_dependency dep
      WHERE dep.user_id = #{codebaseOwnerUserId}
        AND dep.dependency = #{hash}
      RETURNING dependent
  |]
    <&> Set.fromList

-- | Save a temp entity to main storage, and clear any missing dependency rows for it, and
-- return the set of hashes which dependended on it and _might_ now be ready to flush.
saveTempEntityInMain :: forall e. HasCallStack => Hash32 -> TempEntity -> CodebaseM e (Set Hash32)
saveTempEntityInMain hash entity = do
  saveEntity entity
  clearTempDependencies hash
  where
    saveEntity :: TempEntity -> CodebaseM e ()
    saveEntity = \case
      Entity.TC (TermFormat.SyncTerm syncTermComponent) -> doTermComponent syncTermComponent
      Entity.DC (DeclFormat.SyncDecl syncDecl) -> doDeclComponent syncDecl
      Entity.N BranchFormat.SyncDiff {} ->
        lift . unrecoverableError $ InternalServerError "namespace-diffs-not-supported" Unimplemented
      Entity.N (BranchFormat.SyncFull localIds bytes) -> doNamespace localIds bytes
      Entity.P PatchFormat.SyncDiff {} -> lift . unrecoverableError $ InternalServerError "patch-diffs-not-supported" Unimplemented
      Entity.P (PatchFormat.SyncFull localIds patchBytes) -> doPatch localIds patchBytes
      Entity.C (SqliteCausal.SyncCausalFormat {valueHash, parents}) -> doCausal valueHash parents
    doTermComponent ::
      TermFormat.SyncLocallyIndexedComponent' Text Hash32 ->
      CodebaseM e ()
    doTermComponent syncTermComponent = do
      let (TermFormat.SyncLocallyIndexedComponent termComponentElements) = syncTermComponent
      elementsWithDecodedType <-
        for termComponentElements \(localIds, termElementBytes) ->
          case S.decodeTermComponentElementType termElementBytes of
            Left decodeErr -> lift . unrecoverableError $ InternalServerError "term-format-invalid" (InvalidTermComponentBytes $ from decodeErr)
            Right typ -> pure (localIds, TermComponentElementBytes termElementBytes, typ)
      componentWithIds <-
        ( Defn.ensureTextIdsOf (traversed . _1 . LocalIds.t_) elementsWithDecodedType
            >>= HashQ.ensureComponentHashIdsOf (traversed . _1 . LocalIds.h_ . hash32AsComponentHash_)
          )
          <&> Vector.toList
      Defn.saveEncodedTermComponent componentHash componentWithIds
    doDeclComponent ::
      DeclFormat.SyncLocallyIndexedComponent' Text Hash32 ->
      CodebaseM e ()
    doDeclComponent syncDecl = do
      declComponentElements <- case Decoders.unsyncDeclComponent syncDecl of
        Left decodeErr -> lift . unrecoverableError $ InternalServerError "term-format-invalid" (InvalidTermComponentBytes decodeErr)
        Right (DeclFormat.LocallyIndexedComponent elements) -> pure elements
      componentWithIds <-
        ( Defn.ensureTextIdsOf (traversed . _1 . LocalIds.t_) declComponentElements
            >>= HashQ.ensureComponentHashIdsOf (traversed . _1 . LocalIds.h_ . hash32AsComponentHash_)
          )
          <&> Vector.toList
      Defn.saveTypeComponent componentHash componentWithIds

    doNamespace ::
      BranchFormat.BranchLocalIds' Text Hash32 Hash32 (Hash32, Hash32) ->
      LocalBranchBytes ->
      CodebaseM e ()
    doNamespace localIds bytes = do
      pgNamespace <-
        (expandNamespaceFormat localIds bytes)
          >>= Defn.ensureTextIdsOf BranchFull.t_
          >>= HashQ.expectComponentHashIdsOf (BranchFull.h_ . hash32AsComponentHash_)
          >>= HashQ.expectPatchIdsOf (BranchFull.p_ . hash32AsPatchHash_)
          >>= HashQ.expectCausalIdsOf (BranchFull.c_ . childHashT)
      void $ CausalQ.savePgNamespace (Just (BranchHash . Hash32.toHash $ hash)) pgNamespace

    doPatch ::
      PatchFormat.PatchLocalIds' Text Hash32 Hash32 ->
      ByteString ->
      CodebaseM e ()
    doPatch localIds patchBytes = do
      pgPatch <-
        expandPatchFormat localIds patchBytes
          >>= Defn.ensureTextIdsOf PatchFull.patchT_
          -- The LHS patch component refs need to be 'ensured', not 'expected', because they
          -- aren't technically dependencies of the patch and there's no guarantee they were
          -- synced. Whereas for all other hashes there's a bug in dependency resolution
          -- if for some reason they don't already exist.
          >>= HashQ.ensureComponentHashIdsOf (PatchFull.patchH_ . hash32AsComponentHash_)
          >>= HashQ.expectComponentHashIdsOf (PatchFull.patchO_ . hash32AsComponentHash_)
      void $ PatchQ.savePatch (PatchHash . Hash32.toHash $ hash) pgPatch

    doCausal ::
      Hash32 ->
      Vector.Vector Hash32 ->
      CodebaseM e ()
    doCausal valueHash parents = do
      bhId <- HashQ.expectBranchHashId $ BranchHash (Hash32.toHash valueHash)
      parentIds <-
        HashQ.expectCausalIdsOf (traversed . hash32AsCausalHash_) parents
          <&> (Vector.toList >>> fmap snd >>> Set.fromList)
      void $ CausalQ.saveCausal (Just . CausalHash . Hash32.toHash $ hash) bhId parentIds

    componentHash :: ComponentHash
    componentHash = ComponentHash . Hash32.toHash $ hash
    -- This traversal takes a (branchHash, causalHash) pair, discards the branch hash, then
    -- takes the ids expanded by the action and returns a (branchHashId, causalId) pair.
    childHashT :: Traversal (Hash32, Hash32) (BranchHashId, CausalId) CausalHash (BranchHashId, CausalId)
    childHashT f (_branchHash, causalHash) = f (CausalHash . Hash32.toHash $ causalHash)
    expandNamespaceFormat ::
      (Ord text, Ord defn) =>
      (BranchFormat.BranchLocalIds' text defn patch child) ->
      BranchFormat.LocalBranchBytes ->
      CodebaseM e (BranchFull.Branch' text defn patch child)
    expandNamespaceFormat localIds (BranchFormat.LocalBranchBytes bytes) = do
      case S.decodeNamespace bytes of
        Left {} -> lift $ unrecoverableError $ InternalServerError "namespace-format-invalid" InvalidNamespaceBytes
        Right localNamespace -> do
          pure $ BranchFormat.localToBranch localIds localNamespace
    expandPatchFormat :: (Ord text, Ord hash, Ord defn) => PatchFormat.PatchLocalIds' text hash defn -> ByteString -> CodebaseM e (PatchFull.Patch' text hash defn)
    expandPatchFormat patchLocalIds patchBytes = do
      case S.decodePatch patchBytes of
        Left {} -> lift $ unrecoverableError $ InternalServerError "patch-format-invalid" InvalidPatchBytes
        Right localPatch -> do
          pure $ PatchFormat.localPatchToPatch' patchLocalIds localPatch

-- | "Elaborate" a set of hashes that we are considering asking to client to upload to Unison Share.
--
-- For each hash, we transitively follow its dependencies in temp_entity_missing_dependency
-- until we find the tips of the dependency graph. We then return the set of those
-- dependencies which still need to be uploaded by the client, and the set which are ready to
-- be flushed to main storage.
elaborateHashes :: NESet Hash32 -> CodebaseM e (Maybe (NESet Hash32 {- needed from client -}), Maybe (NESet Hash32 {- ready to flush -}))
elaborateHashes hashSet = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  rows <-
    queryListRows @(Hash32, Bool, Bool)
      [sql|
            WITH RECURSIVE unelaborated_dependency(hash) AS (
              SELECT * FROM ^{singleColumnTable (Foldable.toList hashSet)}
            ),
            elaborated_dependency (hash) AS (
              SELECT unelaborated_dependency.hash FROM unelaborated_dependency
              UNION
              (SELECT temp_entity_missing_dependency.dependency
              FROM temp_entity_missing_dependency
                JOIN elaborated_dependency
                  ON temp_entity_missing_dependency.dependent = elaborated_dependency.hash
              WHERE
                temp_entity_missing_dependency.user_id = #{codebaseOwnerUserId}
              )
            ), tagged_hashes (hash, in_temp, has_dependencies) AS (
              SELECT elaborated_dependency.hash,
                     EXISTS(SELECT FROM temp_entity
                            WHERE elaborated_dependency.hash = temp_entity.hash
                              AND temp_entity.user_id = #{codebaseOwnerUserId}
                           ) AS in_temp,
                     EXISTS(SELECT FROM temp_entity_missing_dependency missing_dep
                                WHERE missing_dep.dependent = elaborated_dependency.hash
                                  AND missing_dep.user_id = #{codebaseOwnerUserId}
                               ) AS has_dependencies
                FROM elaborated_dependency
            ) SELECT tagged_hashes.hash, tagged_hashes.in_temp, tagged_hashes.has_dependencies
              FROM tagged_hashes
              -- If something is already in_temp but still has dependencies, then we don't care
              -- about it.
                WHERE NOT (tagged_hashes.in_temp AND tagged_hashes.has_dependencies)
          |]
  let (neededHashes, readyToFlushHashes) =
        rows & foldMap \case
          (hash, inTemp, hasDependencies)
            | not inTemp -> (Set.singleton hash, Set.empty)
            | inTemp && not hasDependencies -> (Set.empty, Set.singleton hash)
            | otherwise ->
                -- These should already be filtered out by the SQL query, we don't care about them.
                mempty
  pure $ (NESet.nonEmptySet neededHashes, NESet.nonEmptySet readyToFlushHashes)

getEntitiesReadyToFlush :: CodebaseM e [Hash32]
getEntitiesReadyToFlush = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  queryListCol
    [sql|
      SELECT hash FROM temp_entity
        WHERE user_id = #{codebaseOwnerUserId}
          AND NOT EXISTS(SELECT FROM temp_entity_missing_dependency missing_dep
                          WHERE missing_dep.dependent = temp_entity.hash
                            AND missing_dep.user_id = #{codebaseOwnerUserId}
                        )
    |]
