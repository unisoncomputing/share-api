module Share.Postgres.LooseCode.Queries
  ( loadLooseCodeRoot,
    expectLooseCodeRoot,
    ensureLooseCodeRootHash,
    setLooseCodeRoot,
    initialize,
  )
where

import Share.Codebase.Types (CodebaseM)
import Share.Codebase.Types qualified as Codebase
import Share.IDs
import Share.Postgres (unrecoverableError)
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
ensureLooseCodeRootHash :: CausalId -> CodebaseM e ()
ensureLooseCodeRootHash causalId = do
  ownerUserId <- asks Codebase.codebaseOwner
  loadLooseCodeRoot >>= \case
    Just _ ->
      -- A root hash for this codebase already exists.
      pure ()
    Nothing -> do
      PG.execute_ (updateReflogSQL ownerUserId)
      PG.execute_ (setRootSQL ownerUserId)
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

loadLooseCodeRoot :: CodebaseM e (Maybe (CausalId, CausalHash))
loadLooseCodeRoot = do
  userId <- asks Codebase.codebaseOwner
  PG.query1Row
    [PG.sql|
        SELECT causal.id, causal.hash
        FROM loose_code_roots lcr
        JOIN causals causal ON lcr.causal_id = causal.id
        WHERE lcr.user_id = #{userId}
      |]

expectLooseCodeRoot :: CodebaseM e (CausalId, CausalHash)
expectLooseCodeRoot = do
  userId <- asks Codebase.codebaseOwner
  loadLooseCodeRoot >>= \case
    Nothing -> lift . unrecoverableError . InternalServerError @Text "missing-loose-code-root" $ "Missing loose code root for user: " <> tShow userId
    Just h -> pure h

-- | Set a user's loose code root and update the reflog.
setLooseCodeRoot :: NameLookupReceipt -> UserId -> Maybe Text -> CausalId -> CodebaseM e ()
setLooseCodeRoot !_nlReceipt callerUserId description newCausalId = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  newCausalHash <- HashQ.expectCausalHashesByIdsOf id newCausalId
  -- Seems redundant, but ensures the codebase actually contains the causal we're about to
  -- set.
  _ <- HashQ.expectCausalIdByHash newCausalHash
  PG.execute_ (updateReflogSQL codebaseOwnerUserId)
  PG.execute_ (setRootSQL codebaseOwnerUserId)
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
initialize :: CodebaseM e ()
initialize = do
  (emptyBhId, _) <- CausalQ.savePgNamespace Nothing BranchFull.emptyBranch
  (cid, _causalHash) <- CausalQ.saveCausal Nothing Nothing emptyBhId mempty
  ensureLooseCodeRootHash cid
