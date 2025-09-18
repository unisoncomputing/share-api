module Share.Codebase.CodebaseRuntime
  ( withCodebaseRuntime,
    UnisonRuntime,
  )
where

import Share.Codebase qualified as Codebase
import Share.Codebase.CodeCache qualified as CC
import Share.Codebase.Types
import Share.Postgres qualified as PG
import Share.Prelude
import Unison.Runtime (Runtime)
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import UnliftIO qualified

type UnisonRuntime = Runtime Symbol

-- | Construct a Runtime linked to a specific codebase and transaction.
-- Don't use the runtime for one codebase with another codebase.
-- Don't use this runtime in any transaction other than the one where it's created.
--
--
-- Ideally, we'd use a runtime with lookup actions in transaction, not IO. But that will require refactoring to
-- the runtime interface in ucm, so we can't use it for now.
--
-- As a result, the 'IO' actions embedded in the CodebaseRuntime are scoped to the transaction
-- in which it is created, thus we use a scoped skolem type variable `s` to prevent the runtime from escaping the transaction so it's safe(r).
withCodebaseRuntime :: (Exception e) => CodebaseEnv -> Runtime Symbol -> (forall s. CodebaseRuntime s IO -> PG.Transaction e r) -> PG.Transaction e r
withCodebaseRuntime codebase sandboxedRuntime f = do
  CC.withCodeCache codebase \codeCache -> do
    withCodebaseRuntimeTransaction sandboxedRuntime codeCache \cr -> do
      PG.asUnliftIOTransaction $ do
        UnliftIO.withRunInIO \toIO -> do
          toIO . PG.UnliftIOTransaction $ f $ hoistCodebaseRuntime (toIO . PG.UnliftIOTransaction) cr

-- | Ideally, we'd use this – a runtime with lookup actions in transaction, not IO. But that will require refactoring to
-- the runtime interface in ucm, so we can't use it for now. That's bad: we end up unsafely running separate
-- transactions for inner calls to 'codeLookup' / 'cachedEvalResult', which can lead to deadlock due to a starved
-- connection pool.
withCodebaseRuntimeTransaction :: Runtime Symbol -> CodeCache s1 -> (forall s2. CodebaseRuntime s2 (PG.Transaction e) -> PG.Transaction e a) -> PG.Transaction e a
withCodebaseRuntimeTransaction sandboxedRuntime codeCache@(CodeCache {codeCacheCodebaseEnv}) f = do
  f $
    CodebaseRuntime
      { codeLookup = CC.toCodeLookup codeCache,
        codeCache,
        cachedEvalResult = (fmap . fmap) Term.unannotate . Codebase.loadCachedEvalResult codeCacheCodebaseEnv,
        unisonRuntime = sandboxedRuntime
      }
