module Share.Postgres.LooseCode.Queries
  ( loadLooseCodeRoot,
    expectLooseCodeRoot,
    ensureLooseCodeRootHash,
    setLooseCodeRoot,
    initialize,
  )
where

import Share.Codebase.Types (CodebaseEnv (..))
import Share.IDs
import Share.Postgres (QueryM, unrecoverableError)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Causal.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.Web.Errors
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull

-- | Initialize a user's loose code root if it doesn't already exist.
--
-- The caller must ensure the Causal Hash is in the user's codebase.
ensureLooseCodeRootHash :: (QueryM m) => CodebaseEnv -> CausalId -> m ()
ensureLooseCodeRootHash codebase@(CodebaseEnv {codebaseOwner}) causalId = do
  loadLooseCodeRoot codebase >>= \case
    Just _ ->
      -- A root hash for this codebase already exists.
      pure ()
    Nothing -> do
      PG.execute_ (updateReflogSQL codebaseOwner)
      PG.execute_ (setRootSQL codebaseOwner)
  where
    description :: Text
    description = "Created"
    updateReflogSQL ownerUserId =
      [PG.sql|
        INSERT INTO loose_code_reflog(
          loose_code_user_id,
          old_causal_id,
          new_causal_id,
          editor_user_id,
          description
        )
        SELECT
          #{ownerUserId},
          NULL,
          #{causalId},
          #{ownerUserId},
          #{description}
          |]
    -- Just in case another request has gotten in and created the codebase or pushed some root
    -- to the codebase before we got here, we avoid clobbering and just succeed
    setRootSQL ownerUserId =
      [PG.sql|
            INSERT INTO loose_code_roots (user_id, causal_id)
              VALUES (#{ownerUserId}, #{causalId})
          |]

loadLooseCodeRoot :: (QueryM m) => CodebaseEnv -> m (Maybe (CausalId, CausalHash))
loadLooseCodeRoot (CodebaseEnv {codebaseOwner}) = do
  PG.query1Row
    [PG.sql|
        SELECT causal.id, causal.hash
        FROM loose_code_roots lcr
        JOIN causals causal ON lcr.causal_id = causal.id
        WHERE lcr.user_id = #{codebaseOwner}
      |]

expectLooseCodeRoot :: (QueryM m) => CodebaseEnv -> m (CausalId, CausalHash)
expectLooseCodeRoot codebase@(CodebaseEnv {codebaseOwner}) = do
  loadLooseCodeRoot codebase >>= \case
    Nothing -> unrecoverableError . InternalServerError @Text "missing-loose-code-root" $ "Missing loose code root for user: " <> tShow codebaseOwner
    Just h -> pure h

-- | Set a user's loose code root and update the reflog.
setLooseCodeRoot :: (QueryM m) => CodebaseEnv -> NameLookupReceipt -> UserId -> Maybe Text -> CausalId -> m ()
setLooseCodeRoot codebase@(CodebaseEnv {codebaseOwner}) !_nlReceipt callerUserId description newCausalId = do
  newCausalHash <- HashQ.expectCausalHashesByIdsOf id newCausalId
  -- Seems redundant, but ensures the codebase actually contains the causal we're about to
  -- set.
  _ <- HashQ.expectCausalIdByHash codebase newCausalHash
  PG.execute_ (updateReflogSQL codebaseOwner)
  PG.execute_ (setRootSQL codebaseOwner)
  where
    updateReflogSQL codebaseOwnerUserId =
      [PG.sql|
        INSERT INTO loose_code_reflog(
          loose_code_user_id,
          old_causal_id,
          new_causal_id,
          editor_user_id,
          description
        )
        SELECT
          #{codebaseOwnerUserId},
          loose_code_roots.causal_id,
          #{newCausalId},
          #{callerUserId},
          #{description}
        FROM loose_code_roots
        WHERE user_id = #{codebaseOwnerUserId}
          |]
    setRootSQL codebaseOwnerUserId =
      [PG.sql|
            INSERT INTO loose_code_roots (user_id, causal_id)
              VALUES (#{codebaseOwnerUserId}, #{newCausalId})
              ON CONFLICT (user_id)
                DO UPDATE
                SET causal_id = #{newCausalId}
          |]

-- | Initializes a codebase for a new user
initialize :: (QueryM m) => CodebaseEnv -> m ()
initialize codebase = do
  (emptyBhId, _) <- CausalQ.savePgNamespace codebase Nothing Nothing BranchFull.emptyBranch
  (cid, _causalHash) <- CausalQ.saveCausal codebase Nothing Nothing emptyBhId mempty
  ensureLooseCodeRootHash codebase cid
