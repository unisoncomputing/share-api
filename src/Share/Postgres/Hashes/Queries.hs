-- | Module for resolving hash-related queries.
-- Note that for most API calls we need to somehow validate that the caller has access to the
-- resource they're accessing.
--
-- The rule goes like this:
-- * If a hash is provided, we should validate that the user has access to that resource by
-- checking that it's in their codebase via the appropriate ownership table, and then convert
-- it into the appropriate ID rather than Hash and pass the ID around from that point onwards.
-- * If an ID is provided, we assume that the caller has already validated that they have access
-- to that resource and any resources contained within it. E.g. if a call provides a CausalId,
-- it's assumed the caller has already validated that they have access to the Causal and by
-- extension all branches, definitions, patches, and metadata contained inside.
module Share.Postgres.Hashes.Queries
  ( ensureComponentHashId,
    ensureComponentHashIdsOf,
    expectComponentHashIdsOf,
    expectComponentHashesOf,
    expectComponentHashId,
    expectComponentHash,
    expectPatchHashesOf,
    expectPatchIdsOf,
    ensureBranchHashId,
    loadBranchHashId,
    expectBranchHashId,
    expectBranchHash,
    expectCausalHashesByIdsOf,
    expectCausalAndBranchHashesOf,
    expectCausalIdsOf,
    loadCausalIdByHash,
    expectCausalIdByHash,
    expectNamespaceIdsByCausalIdsOf,
    expectNamespaceHashesByNamespaceHashIdsOf,
    isComponentHashAllowedToBeMismatched,
    isCausalHashAllowedToBeMismatched,
    addKnownComponentHashMismatch,
    addKnownCausalHashMismatch,
  )
where

import Control.Lens
import Data.Text qualified as Text
import Share.Codebase.Types (CodebaseM)
import Share.Codebase.Types qualified as Codebase
import Share.Postgres
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.Postgres (ordered)
import Share.Web.Errors (EntityMissing (EntityMissing), MissingExpectedEntity (..))

-- | Save a component hash, or return the existing hash id if it already exists
ensureComponentHashId :: (HasCallStack, QueryM m) => ComponentHash -> m ComponentHashId
ensureComponentHashId componentHash = ensureComponentHashIdsOf id componentHash

-- | Get the matching hashId for every component hash, saving any that are missing.
-- Maintains the invariant that the returned list matches the positions of the input list.
ensureComponentHashIdsOf :: forall m s t. (QueryM m, HasCallStack) => Traversal s t ComponentHash ComponentHashId -> s -> m t
ensureComponentHashIdsOf trav s =
  s
    & unsafePartsOf trav %%~ \componentHashes ->
      do
        let numberedHashIds = zip [1 :: Int32 ..] componentHashes
        results <-
          queryListCol @ComponentHashId
            [sql|
    WITH new_hashes(ord, hash) AS (
          SELECT * FROM ^{toTable numberedHashIds}
    ), inserted_hashes(hash, id) AS (
      INSERT INTO component_hashes (base32)
        SELECT DISTINCT new_hashes.hash FROM new_hashes
        ON CONFLICT DO NOTHING
        RETURNING base32, id
    )
    SELECT COALESCE(inserted_hashes.id, existing_hash.id)
      FROM new_hashes
        LEFT JOIN component_hashes existing_hash ON existing_hash.base32 = new_hashes.hash
        LEFT JOIN inserted_hashes ON inserted_hashes.hash = new_hashes.hash
      ORDER BY new_hashes.ord ASC
    |]
        if length results /= length componentHashes
          then error "ensureComponentHashIdsOf: Missing expected component hash"
          else pure results

expectComponentHashIdsOf :: (HasCallStack, QueryM m) => Traversal s t ComponentHash ComponentHashId -> s -> m t
expectComponentHashIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \componentHashes -> do
      let numberedHashes = zip [1 :: Int32 ..] componentHashes
      results :: [ComponentHashId] <-
        queryListCol
          [sql|
      WITH hashes(ord, hash) AS (
        SELECT * FROM ^{toTable numberedHashes}
      )
      SELECT component_hashes.id FROM component_hashes JOIN hashes ON component_hashes.base32 = hashes.hash
        ORDER BY hashes.ord ASC
      |]
      if length results /= length componentHashes
        then error "expectComponentHashIdsOf: Missing expected component hash"
        else pure results

expectComponentHashId :: (HasCallStack, QueryM m) => ComponentHash -> m ComponentHashId
expectComponentHashId componentHash = do
  queryExpect1Col [sql|SELECT id FROM component_hashes WHERE base32 = #{componentHash}|]

expectComponentHashesOf :: (HasCallStack, QueryM m) => Traversal s t ComponentHashId ComponentHash -> s -> m t
expectComponentHashesOf trav = do
  unsafePartsOf trav %%~ \hashIds -> do
    let numberedHashIds = zip [0 :: Int32 ..] hashIds
    results :: [ComponentHash] <-
      queryListCol
        [sql|
      WITH hash_ids(ord, id) AS (
        SELECT * FROM ^{toTable numberedHashIds}
      )
      SELECT base32 FROM component_hashes JOIN hash_ids ON component_hashes.id = hash_ids.id
        ORDER BY hash_ids.ord ASC
      |]
    if length results /= length hashIds
      then error "expectComponentHashesOf: Missing expected component hash"
      else pure results

expectComponentHash :: (HasCallStack, QueryM m) => ComponentHashId -> m ComponentHash
expectComponentHash hashId = queryExpect1Col [sql|SELECT base32 FROM component_hashes WHERE id = #{hashId}|]

expectPatchHashesOf :: (HasCallStack, QueryM m) => Traversal s t PatchId PatchHash -> s -> m t
expectPatchHashesOf trav = do
  unsafePartsOf trav %%~ \hashIds -> do
    let numberedHashIds = zip [0 :: Int32 ..] hashIds
    results :: [PatchHash] <-
      queryListCol
        [sql|
      WITH hash_ids(ord, id) AS (
        SELECT * FROM ^{toTable numberedHashIds}
      )
      SELECT patch.hash FROM patches patch JOIN hash_ids ON patch.id = hash_ids.id
        ORDER BY hash_ids.ord ASC
      |]
    if length results /= length hashIds
      then error "expectPatchHashesOf: Missing expected patch hash"
      else pure results

expectPatchIdsOf :: (HasCallStack) => Traversal s t PatchHash PatchId -> s -> CodebaseM e t
expectPatchIdsOf trav = do
  unsafePartsOf trav %%~ \hashes -> do
    codebaseOwner <- asks Codebase.codebaseOwner
    let numberedHashes = zip [0 :: Int32 ..] hashes
    results :: [PatchId] <-
      queryListCol
        [sql|
      WITH hashes(ord, hash) AS (
        SELECT * FROM ^{toTable numberedHashes}
      )
      SELECT p.id FROM patches p JOIN hashes h ON p.hash = h.hash
        WHERE EXISTS (
          SELECT FROM patch_ownership po
            WHERE po.patch_id = p.id
              AND po.user_id = #{codebaseOwner}
        )
        ORDER BY h.ord ASC
      |]
    if length results /= length hashes
      then unrecoverableError $ EntityMissing "expected-patch-for-hash" $ "Missing patch for one of the provided hashes: " <> Text.intercalate ", " (into @Text <$> hashes)
      else pure results

-- | Save a branch hash, or return the existing hash id if it already exists
ensureBranchHashId :: (HasCallStack, QueryM m) => BranchHash -> m BranchHashId
ensureBranchHashId branchHash = do
  query1Col [sql|SELECT id FROM branch_hashes WHERE base32 = #{branchHash}|] >>= \case
    Just hashId -> pure hashId
    Nothing -> queryExpect1Col [sql|INSERT INTO branch_hashes (base32) VALUES (#{branchHash}) RETURNING id|]

loadBranchHashId :: BranchHash -> CodebaseM e (Maybe BranchHashId)
loadBranchHashId branchHash = do
  codebaseOwner <- asks Codebase.codebaseOwner
  query1Col
    [sql|
    SELECT bh.id FROM branch_hashes bh
      WHERE bh.base32 = #{branchHash}
        AND EXISTS (
          SELECT FROM namespace_ownership no
            WHERE no.namespace_hash_id = bh.id
              AND no.user_id = #{codebaseOwner}
      )
    |]

expectBranchHashId :: (HasCallStack) => BranchHash -> CodebaseM e BranchHashId
expectBranchHashId branchHash = do
  loadBranchHashId branchHash >>= \case
    Just hashId -> pure hashId
    Nothing -> unrecoverableError $ EntityMissing "missing-namespace-for-hash" ("Namespace not found for hash: " <> into @Text branchHash)

expectBranchHash :: (HasCallStack, QueryM m) => BranchHashId -> m BranchHash
expectBranchHash hashId = queryExpect1Col [sql|SELECT base32 FROM branch_hashes WHERE id = #{hashId}|]

-- | Returns whether we should allow a hash mismatch for the given pair of hashes.
isComponentHashAllowedToBeMismatched :: (HasCallStack, QueryM m) => ComponentHash -> ComponentHash -> m Bool
isComponentHashAllowedToBeMismatched providedHash actualHash = do
  queryExpect1Col
    [sql|
    SELECT EXISTS(
      SELECT FROM known_component_hash_mismatches mismatch
        WHERE mismatch.provided_component_hash = #{providedHash}
          AND mismatch.actual_component_hash = #{actualHash}
      )
    |]

-- | Returns whether we should allow a hash mismatch for the given pair of hashes.
isCausalHashAllowedToBeMismatched :: (HasCallStack, QueryM m) => CausalHash -> CausalHash -> m Bool
isCausalHashAllowedToBeMismatched providedHash actualHash = do
  queryExpect1Col
    [sql|
    SELECT EXISTS(
      SELECT FROM known_causal_hash_mismatches mismatches
        WHERE mismatches.provided_causal_hash = #{providedHash}
          AND mismatches.actual_causal_hash = #{actualHash}
      )
    |]

addKnownComponentHashMismatch :: (HasCallStack, QueryM m) => ComponentHash -> ComponentHash -> m ()
addKnownComponentHashMismatch providedHash actualHash = do
  execute_
    [sql|
    INSERT INTO known_component_hash_mismatches (provided_component_hash, actual_component_hash)
      VALUES (#{providedHash}, #{actualHash})
      ON CONFLICT DO NOTHING
    |]

addKnownCausalHashMismatch :: (HasCallStack, QueryM m) => CausalHash -> CausalHash -> m ()
addKnownCausalHashMismatch providedHash actualHash = do
  execute_
    [sql|
    INSERT INTO known_causal_hash_mismatches (provided_causal_hash, actual_causal_hash)
      VALUES (#{providedHash}, #{actualHash})
      ON CONFLICT DO NOTHING
    |]

-- | Generic helper which fetches both branch hashes and causal hashes
expectCausalHashesOfG :: (HasCallStack, QueryM m) => ((BranchHash, CausalHash) -> h) -> Traversal s t CausalId h -> s -> m t
expectCausalHashesOfG project trav = do
  unsafePartsOf trav %%~ \hashIds -> do
    let numberedHashIds = zip [0 :: Int32 ..] hashIds
    results :: [(BranchHash, CausalHash)] <-
      queryListRows
        [sql|
      WITH causal_ids(ord, id) AS (
        SELECT * FROM ^{toTable numberedHashIds}
      )
      SELECT bh.base32, causal.hash
        FROM causal_ids
          JOIN causals causal ON causal.id = causal_ids.id
          JOIN branch_hashes bh ON causal.namespace_hash_id = bh.id
        ORDER BY causal_ids.ord ASC
      |]
    if length results /= length hashIds
      then error "expectCausalHashesOf: Missing expected causal hash"
      else pure (project <$> results)

expectCausalAndBranchHashesOf :: (HasCallStack, QueryM m) => Traversal s t CausalId (BranchHash, CausalHash) -> s -> m t
expectCausalAndBranchHashesOf = expectCausalHashesOfG id

expectCausalHashesByIdsOf :: (HasCallStack, QueryM m) => Traversal s t CausalId CausalHash -> s -> m t
expectCausalHashesByIdsOf = expectCausalHashesOfG snd

expectCausalIdsOf :: (HasCallStack) => Traversal s t CausalHash (BranchHashId, CausalId) -> s -> CodebaseM e t
expectCausalIdsOf trav = do
  unsafePartsOf trav %%~ \hashes -> do
    codebaseOwnerId <- asks Codebase.codebaseOwner
    let numberedHashes = zip [0 :: Int32 ..] hashes
    results :: [(BranchHashId, CausalId)] <-
      queryListRows
        [sql|
      WITH hashes(ord, hash) AS (
        SELECT * FROM ^{toTable numberedHashes}
      )
      SELECT bh.id, causal.id
        FROM hashes
          JOIN causals causal ON causal.hash = hashes.hash
          JOIN branch_hashes bh ON causal.namespace_hash_id = bh.id
          WHERE EXISTS (
            SELECT FROM causal_ownership co
              WHERE co.causal_id = causal.id
                AND co.user_id = #{codebaseOwnerId}
          )
        ORDER BY hashes.ord ASC
      |]
    if length results /= length hashes
      then unrecoverableError $ EntityMissing "missing-expected-causal" $ "Missing one of these causals: " <> Text.intercalate ", " (into @Text <$> hashes)
      else pure results

expectNamespaceIdsByCausalIdsOf :: QueryM m => Traversal s t CausalId BranchHashId -> s -> m t
expectNamespaceIdsByCausalIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \causalIds -> do
      let causalIdsTable = ordered causalIds
      results <-
        queryListCol @(BranchHashId)
          [sql| WITH causal_ids(ord, causal_id) AS (
                SELECT ord, causal_id FROM ^{toTable causalIdsTable} as t(ord, causal_id)
              )
              SELECT c.namespace_hash_id
                FROM causal_ids cid
                JOIN causals c ON cid.causal_id = c.id
                ORDER BY cid.ord
        |]
      if length results /= length causalIds
        then unrecoverableError . MissingExpectedEntity $ "expectNamespaceIdsByCausalIdsOf: Expected to get the same number of results as causal ids. " <> tShow causalIds
        else pure results

expectNamespaceHashesByNamespaceHashIdsOf :: (HasCallStack, QueryM m) => Traversal s t BranchHashId BranchHash -> s -> m t
expectNamespaceHashesByNamespaceHashIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \namespaceHashIds -> do
      let namespaceHashIdsTable = ordered namespaceHashIds
      results <-
        queryListCol @(BranchHash)
          [sql| WITH namespace_hash_ids(ord, namespace_hash_id) AS (
                SELECT ord, namespace_hash_id FROM ^{toTable namespaceHashIdsTable} as t(ord, namespace_hash_id)
              )
              SELECT bh.base32
                FROM namespace_hash_ids nhi
                JOIN branch_hashes bh ON nhi.namespace_hash_id = bh.id
                ORDER BY nhi.ord
        |]
      if length results /= length namespaceHashIds
        then unrecoverableError . MissingExpectedEntity $ "expectNamespaceHashesByNamespaceHashIdsOf: Expected to get the same number of results as namespace hash ids. " <> tShow namespaceHashIds
        else pure results

loadCausalIdByHash :: CausalHash -> Codebase.CodebaseM e (Maybe CausalId)
loadCausalIdByHash causalHash = do
  codebaseOwner <- asks Codebase.codebaseOwner
  query1Col
    [sql| SELECT causals.id FROM causals
            WHERE causals.hash = #{causalHash}
              AND EXISTS (SELECT FROM causal_ownership o WHERE o.causal_id = causals.id AND o.user_id = #{codebaseOwner})
    |]

expectCausalIdByHash :: HasCallStack => CausalHash -> Codebase.CodebaseM e CausalId
expectCausalIdByHash causalHash = do
  loadCausalIdByHash causalHash
    `whenNothingM` unrecoverableError (MissingExpectedEntity $ "Expected causal id for hash: " <> tShow causalHash)
