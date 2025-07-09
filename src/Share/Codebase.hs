{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Codebase
  ( shorthashLength,
    runCodebaseTransaction,
    runCodebaseTransactionOrRespondError,
    runCodebaseTransactionModeOrRespondError,
    runCodebaseTransactionMode,
    tryRunCodebaseTransaction,
    tryRunCodebaseTransactionMode,
    codebaseMToTransaction,
    CodebaseM,
    CodebaseEnv,
    codebaseOwner,
    CodebaseRuntime (..),
    codebaseEnv,
    codebaseRuntime,
    codebaseRuntimeTransaction,
    badAskUnliftCodebaseRuntime,
    codebaseForProjectBranch,
    codebaseLocationForUserCodebase,
    codebaseLocationForProjectBranchCodebase,
    codebaseLocationForProjectRelease,
    CodebaseLocation (..),

    -- * Definitions
    loadTerm,
    expectTerm,
    loadTypeOfTerm,
    expectTypeOfTerm,
    expectTypeOfTerms,
    expectTypeOfReferent,
    expectTypeOfReferents,
    expectTypeOfConstructor,
    loadTypeOfConstructor,
    loadTypeOfReferent,
    loadTypeDeclaration,
    loadTypeDeclarationForCodeLookup,
    expectTypeDeclaration,
    loadDeclKind,
    loadDeclKindsOf,
    expectDeclKind,
    expectDeclKindsOf,
    termReferentsByShortHash,
    typeReferencesByShortHash,
    DefnQ.termTagsByReferentsOf,
    DefnQ.typeTagsByReferencesOf,

    -- * Eval
    loadCachedEvalResult,
    saveCachedEvalResult,
    codeLookupForUser,

    -- * Causals
    CausalQ.loadCausalNamespace,
    CausalQ.expectCausalNamespace,
    loadCausalNamespaceAtPath,
    expectCausalIdByHash,
    squashCausalAndAddToCodebase,
    CausalQ.importCausalIntoCodebase,
    CausalQ.bestCommonAncestor,

    -- * Loose Code
    LCQ.expectLooseCodeRoot,
    LCQ.ensureLooseCodeRootHash,
    setLooseCodeRoot,

    -- * Conversions
    convertTerm2to1,

    -- * Utilities
    cachedCodebaseResponse,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens
import Control.Monad.Morph (hoist)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Servant qualified
import Servant.Server (err500)
import Share.Branch (Branch (..))
import Share.Codebase.Types
import Share.Codebase.Types qualified as Codebase
import Share.Env qualified as Env
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres (unrecoverableError)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries (loadCausalNamespaceAtPath)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.LooseCode.Queries qualified as LCQ
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Prelude
import Share.Project (Project (..))
import Share.Utils.Caching (Cached)
import Share.Utils.Caching qualified as Caching
import Share.Utils.Logging
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Authorization.Types (AuthZReceipt)
import Share.Web.Authorization.Types qualified as AuthZ
import Share.Web.Errors
import U.Codebase.Branch qualified as V2
import U.Codebase.Causal qualified as Causal
import U.Codebase.Decl qualified as V2
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2
import U.Codebase.Sqlite.Symbol qualified as V2
import U.Codebase.Term qualified as V2.Term
import Unison.Builtin qualified as Builtin
import Unison.Builtin qualified as Builtins
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration qualified as V1
import Unison.Hash (Hash)
import Unison.Parser.Ann
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude (askUnliftIO)
import Unison.Reference (TermReferenceId)
import Unison.Reference qualified as Reference
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1
import Unison.Referent qualified as V1Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.ShortHash
import Unison.ShortHash qualified as ShortHash
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Term qualified as V1
import Unison.Type qualified as V1
import UnliftIO qualified

data CodebaseError
  = MissingTypeForTerm Reference.Reference
  | MissingDecl Reference.Id
  | MissingTerm Reference.Id
  | NoLooseCodeRootHash UserId
  | MissingCausalForHash UserId CausalHash
  | MissingTypeForConstructor Reference.Reference V2.ConstructorId
  deriving (Show)

instance ToServerError CodebaseError where
  toServerError = \case
    (MissingTypeForTerm {}) ->
      (ErrorID "backend:missing-type-for-term", err500)
    (MissingDecl {}) ->
      (ErrorID "backend:missing-decl", err500)
    (MissingTerm {}) ->
      (ErrorID "backend:missing-term", err500)
    (NoLooseCodeRootHash {}) ->
      (ErrorID "backend:no-loose-code-root-hash", err500)
    (MissingCausalForHash _userId causalHash) ->
      (ErrorID "backend:missing-causal-for-hash", Servant.err404 {Servant.errBody = "Causal not found for hash: " <> BL.fromStrict (Text.encodeUtf8 (tShow causalHash))})
    (MissingTypeForConstructor {}) ->
      (ErrorID "backend:missing-type-for-constructor", err500)

instance Logging.Loggable CodebaseError where
  toLog = \case
    e@(MissingTypeForTerm {}) -> Logging.withSeverity Logging.Error $ Logging.showLog e
    e@(MissingDecl {}) -> Logging.withSeverity Logging.Error $ Logging.showLog e
    e@(MissingTerm {}) -> Logging.withSeverity Logging.Error $ Logging.showLog e
    e@(NoLooseCodeRootHash {}) -> Logging.withSeverity Logging.Error $ Logging.showLog e
    e@(MissingCausalForHash {}) -> Logging.withSeverity Logging.UserFault $ Logging.showLog e
    e@(MissingTypeForConstructor {}) -> Logging.withSeverity Logging.Error $ Logging.showLog e

shorthashLength :: Int
shorthashLength = 10

-- | Construct a CodebaseEnv allowing you to run transactions against a given codebase.
codebaseEnv :: AuthZReceipt -> CodebaseLocation -> CodebaseEnv
codebaseEnv !_authZReceipt codebaseLoc = do
  let codebaseOwner = Codebase.codebaseOwnerUserId codebaseLoc
   in CodebaseEnv {codebaseOwner}

-- | Construct a Runtime linked to a specific codebase.
-- Don't use the runtime for one codebase with another codebase.
codebaseRuntime :: (MonadReader (Env.Env x) m, MonadUnliftIO m, Env.HasTags x) => CodebaseEnv -> m (CodebaseRuntime IO)
codebaseRuntime codebase = do
  unisonRuntime <- asks Env.sandboxedRuntime
  rt <- liftIO (codebaseRuntimeTransaction unisonRuntime codebase)
  unlift <- badAskUnliftCodebaseRuntime
  pure (unlift rt)

-- | Ideally, we'd use this – a runtime with lookup actions in transaction, not IO. But that will require refactoring to
-- the runtime interface in ucm, so we can't use it for now. That's bad: we end up unsafely running separate
-- transactions for inner calls to 'codeLookup' / 'cachedEvalResult', which can lead to deadlock due to a starved
-- connection pool.
codebaseRuntimeTransaction :: Runtime Symbol -> CodebaseEnv -> IO (CodebaseRuntime (PG.Transaction e))
codebaseRuntimeTransaction unisonRuntime CodebaseEnv {codebaseOwner} = do
  cacheVar <- newTVarIO (CodeLookupCache mempty mempty)
  pure
    CodebaseRuntime
      { codeLookup = codeLookupForUser cacheVar codebaseOwner,
        cachedEvalResult = (fmap . fmap) Term.unannotate . loadCachedEvalResult codebaseOwner,
        unisonRuntime
      }

-- Why bad: see above comment on `codebaseRuntimeTransaction`. We don't want to use a `CodebaseRuntime IO`, because it
-- will run every lookup in a separate transaction. But we can't use a `CodebaseRuntime Transaction` because we call
-- back into UCM library code that expects a `CodebaseRuntime IO`.
badAskUnliftCodebaseRuntime ::
  (MonadReader (Env.Env x) m, MonadUnliftIO m, Env.HasTags x) =>
  m (CodebaseRuntime (PG.Transaction Void) -> CodebaseRuntime IO)
badAskUnliftCodebaseRuntime = do
  UnliftIO.UnliftIO toIO <- askUnliftIO
  pure \rt@CodebaseRuntime {codeLookup, cachedEvalResult} ->
    rt
      { codeLookup = hoist (toIO . PG.runTransaction) codeLookup,
        cachedEvalResult = toIO . PG.runTransaction . cachedEvalResult
      }

runCodebaseTransaction :: (MonadReader (Env.Env x) m, MonadIO m, Env.HasTags x) => CodebaseEnv -> CodebaseM Void a -> m a
runCodebaseTransaction codebaseEnv m = do
  either absurd id <$> tryRunCodebaseTransaction codebaseEnv m

-- | Run a CodebaseM transaction using the specified mode.
runCodebaseTransactionMode :: (MonadReader (Env.Env x) m, MonadIO m, Env.HasTags x) => PG.IsolationLevel -> PG.Mode -> CodebaseEnv -> CodebaseM Void a -> m a
runCodebaseTransactionMode isoLevel rwmode codebaseEnv m = do
  either absurd id <$> tryRunCodebaseTransactionMode isoLevel rwmode codebaseEnv m

runCodebaseTransactionOrRespondError :: (ToServerError e, Loggable e) => CodebaseEnv -> CodebaseM e a -> WebApp a
runCodebaseTransactionOrRespondError codebaseEnv m = do
  tryRunCodebaseTransaction codebaseEnv m >>= \case
    Left e -> respondError e
    Right a -> pure a

runCodebaseTransactionModeOrRespondError :: (ToServerError e, Loggable e) => PG.IsolationLevel -> PG.Mode -> CodebaseEnv -> CodebaseM e a -> WebApp a
runCodebaseTransactionModeOrRespondError isoLevel mode codebaseEnv m = do
  tryRunCodebaseTransactionMode isoLevel mode codebaseEnv m >>= \case
    Left e -> respondError e
    Right a -> pure a

tryRunCodebaseTransactionMode :: (MonadReader (Env.Env x) m, MonadIO m, Env.HasTags x) => PG.IsolationLevel -> PG.Mode -> CodebaseEnv -> CodebaseM e a -> m (Either e a)
tryRunCodebaseTransactionMode isoLevel rwmode codebaseEnv m = do
  PG.tryRunTransactionMode isoLevel rwmode . codebaseMToTransaction codebaseEnv $ m

tryRunCodebaseTransaction :: (MonadReader (Env.Env x) m, MonadIO m, Env.HasTags x) => CodebaseEnv -> CodebaseM e a -> m (Either e a)
tryRunCodebaseTransaction codebaseEnv m = do
  PG.tryRunTransaction . codebaseMToTransaction codebaseEnv $ m

codebaseMToTransaction :: CodebaseEnv -> CodebaseM e a -> PG.Transaction e a
codebaseMToTransaction codebaseEnv m = runReaderT m codebaseEnv

-- | Wrap a response in caching.
-- This combinator respects the cachability stored on the provided auth receipt.
cachedCodebaseResponse ::
  forall ct a.
  (Servant.MimeRender ct a) =>
  AuthZ.AuthZReceipt ->
  CodebaseLocation ->
  -- | The name of the endpoint we're caching. Must be unique.
  Text ->
  -- | All parameters which affect the response
  [Text] ->
  -- | The root hash the cache is keyed on.
  CausalId ->
  -- | How to generate the response if it's not in the cache.
  WebApp a ->
  WebApp (Cached ct a)
cachedCodebaseResponse authzReceipt codebaseOwner endpointName providedCacheParams rootCausalId action = do
  let cacheParams = ["codebase", codebaseViewCacheKey, "root-hash", Caching.causalIdCacheKey rootCausalId] <> providedCacheParams
  Caching.cachedResponse authzReceipt endpointName cacheParams action
  where
    codebaseViewCacheKey :: Text
    codebaseViewCacheKey = IDs.toText (codebaseOwnerUserId codebaseOwner)

-- | Load a term and its type.
loadTerm :: TermReferenceId -> CodebaseM e (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann))
loadTerm refId = do
  codebaseUser <- asks codebaseOwner
  lift $ loadTermForCodeLookup codebaseUser refId

-- | Load a term and its type.
loadTermForCodeLookup :: UserId -> TermReferenceId -> PG.Transaction e (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann))
loadTermForCodeLookup codebaseUser refId@(Reference.Id h _) = runMaybeT $ do
  (v2Term, v2Type) <- MaybeT $ DefnQ.loadTerm codebaseUser refId
  convertTerm2to1 h v2Term v2Type

convertTerm2to1 :: (PG.QueryM m) => Hash -> V2.Term.Term V2.Symbol -> V2.Term.Type V2.Symbol -> m (V1.Term Symbol Ann, V1.Type Symbol Ann)
convertTerm2to1 h v2Term v2Type = do
  v1Term <- Cv.term2to1 h expectDeclKind v2Term
  let v1Type = Cv.ttype2to1 v2Type
  pure (v1Term, v1Type)

expectTerm :: TermReferenceId -> CodebaseM e (V1.Term Symbol Ann, V1.Type Symbol Ann)
expectTerm refId = loadTerm refId `whenNothingM` lift (unrecoverableError (MissingTerm refId))

-- | Load the type of a term.
-- Differs from `loadTerm` in that it can also return the type of builtins.
loadTypeOfTerm :: V2.Reference -> CodebaseM e (Maybe (V1.Type Symbol Ann))
loadTypeOfTerm r = case r of
  Reference.DerivedId h -> fmap snd <$> loadTerm h
  r@Reference.Builtin {} -> do
    let builtinType =
          Map.lookup r Builtin.termRefTypes
            <&> \typ ->
              (typ $> builtinAnnotation)
    pure builtinType
  where
    builtinAnnotation = Ann.Intrinsic

expectTypeOfTerm :: V2.Reference -> CodebaseM e (V1.Type Symbol Ann)
expectTypeOfTerm r = loadTypeOfTerm r `whenNothingM` lift (unrecoverableError (MissingTypeForTerm r))

expectTypeOfTerms :: Traversal s t V2.Reference (V1.Type Symbol Ann) -> s -> CodebaseM e t
expectTypeOfTerms trav s = do
  s & trav %%~ expectTypeOfTerm

expectTypeOfReferent :: V2.Referent -> CodebaseM e (V1.Type Symbol Ann)
expectTypeOfReferent = \case
  V2.Ref r -> expectTypeOfTerm r
  V2.Con r conId -> expectTypeOfConstructor r conId

expectTypeOfReferents :: Traversal s t V2.Referent (V1.Type Symbol Ann) -> s -> CodebaseM e t
expectTypeOfReferents trav s = do
  s & trav %%~ expectTypeOfReferent

expectDeclKind :: (PG.QueryM m) => Reference.TypeReference -> m CT.ConstructorType
expectDeclKind r = loadDeclKind r `whenNothingM` unrecoverableError (DefnQ.missingDeclKindError r)

expectDeclKindsOf :: (PG.QueryM m) => Traversal s t Reference.TypeReference CT.ConstructorType -> s -> m t
expectDeclKindsOf trav s = do
  s
    & unsafePartsOf trav %%~ \refs -> do
      results <- loadDeclKindsOf traversed refs
      for (zip refs results) \case
        (r, Nothing) -> unrecoverableError (DefnQ.missingDeclKindError r)
        (_, Just ct) -> pure ct

loadDeclKind :: (PG.QueryM m) => V2.TypeReference -> m (Maybe CT.ConstructorType)
loadDeclKind = loadDeclKindsOf id

loadDeclKindsOf :: (PG.QueryM m) => Traversal s t Reference.TypeReference (Maybe CT.ConstructorType) -> s -> m t
loadDeclKindsOf trav s =
  s
    & unsafePartsOf trav %%~ \refs -> do
      xs <-
        refs
          & ( fmap \case
                V2.ReferenceBuiltin t -> Left t
                V2.ReferenceDerived refId -> Right refId
            )
          & traversed . _Left %~ (\t -> Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType)
          & DefnQ.loadDeclKindsOf (traversed . _Right)
      pure (fmap (either id id) xs)

loadTypeDeclaration :: Reference.Id -> CodebaseM e (Maybe (V1.Decl Symbol Ann))
loadTypeDeclaration refId = do
  codebaseUser <- asks codebaseOwner
  lift $ loadTypeDeclarationForCodeLookup codebaseUser refId

loadTypeDeclarationForCodeLookup :: UserId -> Reference.Id -> PG.Transaction e (Maybe (V1.Decl Symbol Ann))
loadTypeDeclarationForCodeLookup codebaseUser refId@(Reference.Id h _) =
  fmap (Cv.decl2to1 h) <$> DefnQ.loadDecl codebaseUser refId

expectTypeDeclaration :: Reference.Id -> CodebaseM e (V1.Decl Symbol Ann)
expectTypeDeclaration refId = loadTypeDeclaration refId `whenNothingM` lift (unrecoverableError (MissingDecl refId))

-- | Find all the term referents that match the given prefix.
-- Includes decl constructors.
termReferentsByShortHash :: ShortHash -> PG.Transaction e (Set V1.Referent)
termReferentsByShortHash = \case
  ShortHash.Builtin b ->
    Builtin.intrinsicTermReferences
      & Set.filter (\r -> V1.ReferenceBuiltin b == r)
      & Set.map V1Referent.Ref
      & pure
  ShortHash.ShortHash prefix cycle cid -> do
    termReferents <-
      DefnQ.termReferencesByPrefix prefix cycle
        <&> Set.map (V1Referent.Ref . V1.ReferenceDerived)
    constructorReferents <- DefnQ.constructorReferentsByPrefix prefix cycle cid
    pure $ termReferents <> constructorReferents

typeReferencesByShortHash :: ShortHash -> PG.Transaction e (Set V1.Reference)
typeReferencesByShortHash = \case
  ShortHash.Builtin b ->
    Builtin.intrinsicTypeReferences
      & Set.filter (\r -> V1.ReferenceBuiltin b == r)
      & pure
  -- type references shouldn't have a constructor.
  (ShortHash.ShortHash _prefix _cycle (Just {})) -> pure mempty
  (ShortHash.ShortHash prefix cycle Nothing) -> do
    DefnQ.declReferencesByPrefix prefix cycle
      <&> Set.map V1.ReferenceDerived

-- | Get the type of a referent.
loadTypeOfReferent ::
  V2.Referent ->
  CodebaseM e (Maybe (V1.Type Symbol Ann))
loadTypeOfReferent = \case
  V2.Ref r -> loadTypeOfTerm r
  V2.Con r conId -> loadTypeOfConstructor r conId

-- | Load the type of a constructor.
loadTypeOfConstructor :: V2.TypeReference -> V2.ConstructorId -> CodebaseM e (Maybe (V1.Type Symbol Ann))
loadTypeOfConstructor ref conId = case ref of
  -- No constructors for builtin types.
  Reference.Builtin _txt -> pure Nothing
  Reference.DerivedId refId -> do
    decl <- loadTypeDeclaration refId `whenNothingM` lift (unrecoverableError (MissingDecl refId))
    pure $ DD.typeOfConstructor (DD.asDataDecl decl) conId

expectTypeOfConstructor :: V2.TypeReference -> V2.ConstructorId -> CodebaseM e (V1.Type Symbol Ann)
expectTypeOfConstructor ref conId =
  loadTypeOfConstructor ref conId >>= \case
    Just typ -> pure typ
    Nothing -> lift (unrecoverableError (MissingTypeForConstructor ref conId))

data CodeLookupCache = CodeLookupCache
  { termCache :: Map Reference.Id (V1.Term Symbol Ann, V1.Type Symbol Ann),
    typeCache :: Map Reference.Id (V1.Decl Symbol Ann)
  }

codeLookupForUser :: TVar CodeLookupCache -> UserId -> CL.CodeLookup Symbol (PG.Transaction e) Ann
codeLookupForUser cacheVar codebaseOwner = do
  CL.CodeLookup (fmap (fmap fst) . getTermAndType) (fmap (fmap snd) . getTermAndType) getTypeDecl
    <> Builtin.codeLookup
    <> IOSource.codeLookupM
  where
    getTermAndType ::
      Reference.Id ->
      PG.Transaction e (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann))
    getTermAndType r = do
      CodeLookupCache {termCache} <- PG.transactionUnsafeIO (readTVarIO cacheVar)
      case Map.lookup r termCache of
        Just termAndType -> pure (Just termAndType)
        Nothing -> do
          maybeTermAndType <- loadTermForCodeLookup codebaseOwner r
          whenJust maybeTermAndType \termAndType -> do
            PG.transactionUnsafeIO do
              atomically do
                modifyTVar' cacheVar \CodeLookupCache {termCache, ..} ->
                  CodeLookupCache {termCache = Map.insert r termAndType termCache, ..}
          pure maybeTermAndType

    getTypeDecl :: Reference.Id -> PG.Transaction e (Maybe (V1.Decl Symbol Ann))
    getTypeDecl r = do
      CodeLookupCache {typeCache} <- PG.transactionUnsafeIO (readTVarIO cacheVar)
      case Map.lookup r typeCache of
        Just typ -> pure (Just typ)
        Nothing -> do
          maybeType <- loadTypeDeclarationForCodeLookup codebaseOwner r
          whenJust maybeType \typ ->
            PG.transactionUnsafeIO do
              atomically do
                modifyTVar' cacheVar \CodeLookupCache {typeCache, ..} ->
                  CodeLookupCache {typeCache = Map.insert r typ typeCache, ..}
          pure maybeType

-- | Look up the result of evaluating a term if we have it cached.
--
-- This is intentionally not in CodebaseM because it's used to build the CodebaseEnv.
loadCachedEvalResult :: UserId -> Reference.Id -> PG.Transaction e (Maybe (V1.Term Symbol Ann))
loadCachedEvalResult codebaseOwnerUserId ref@(Reference.Id h _) = runMaybeT do
  v2Term <- MaybeT $ DefnQ.loadCachedEvalResult codebaseOwnerUserId ref
  lift $ Cv.term2to1 h expectDeclKind v2Term

saveCachedEvalResult :: Reference.Id -> V1.Term Symbol Ann -> CodebaseM e ()
saveCachedEvalResult refId@(Reference.Id h _) term = do
  let v2Term = Cv.term1to2 h term
  DefnQ.saveCachedEvalResult refId v2Term

expectCausalIdByHash :: CausalHash -> CodebaseM e CausalId
expectCausalIdByHash causalHash = do
  codebaseOwnerUserId <- asks codebaseOwner
  (CausalQ.loadCausalIdByHash causalHash) `whenNothingM` lift (unrecoverableError (MissingCausalForHash codebaseOwnerUserId causalHash))

-- | Sets the loose code root hash for a user and ensures the appropriate name lookup
-- exists.
setLooseCodeRoot :: UserId -> Maybe Text -> CausalId -> CodebaseM e ()
setLooseCodeRoot caller description causalId = do
  branchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id causalId
  nlReceipt <- NLOps.ensureNameLookupForBranchId branchHashId
  LCQ.setLooseCodeRoot nlReceipt caller description causalId

-- | Squash a causal and add the result to the codebase.
-- Also adds a name lookup for the squashed branch hash.
squashCausalAndAddToCodebase ::
  CausalId ->
  -- Returns the new causal hash if successful, or Nothing if the source causal doesn't
  -- exist.
  CodebaseM e (Maybe CausalId)
squashCausalAndAddToCodebase causalId = runMaybeT $ do
  causalBranch <- MaybeT (CausalQ.loadCausalNamespace causalId)
  (squashedCausalId, _squashedCausal) <- lift $ squashCausal causalBranch
  squashedBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id squashedCausalId
  lift . lift $ NLOps.ensureNameLookupForBranchId squashedBranchHashId
  pure squashedCausalId

-- Recursively discards history, resulting in a namespace tree with only single a single
-- Causal node at every level.
squashCausal :: V2.CausalBranch (CodebaseM e) -> CodebaseM e (CausalId, V2.CausalBranch (CodebaseM e))
squashCausal Causal.Causal {valueHash = unsquashedBranchHash, value} = do
  mayCachedSquashResult <- runMaybeT $ do
    causalId <- MaybeT (CausalQ.tryGetCachedSquashResult unsquashedBranchHash)
    fmap (causalId,) . MaybeT $ CausalQ.loadCausalNamespace causalId
  case mayCachedSquashResult of
    Just cb -> pure cb
    Nothing -> do
      branch@V2.Branch {children} <- value
      squashedChildren <- traverse squashCausal children
      let squashedBranchHead = branch {V2.children = snd <$> squashedChildren}
      (squashedBranchHashId, squashedBranchHash) <- CausalQ.saveV2BranchShallow squashedBranchHead
      let ancestors = mempty
      (squashedCausalId, squashedCausalHash) <- CausalQ.saveCausal Nothing Nothing squashedBranchHashId ancestors
      let squashedCausalBranch =
            Causal.Causal
              { causalHash = squashedCausalHash,
                valueHash = squashedBranchHash,
                parents = mempty,
                value = pure squashedBranchHead
              }
      CausalQ.saveSquashResult unsquashedBranchHash squashedCausalId
      pure (squashedCausalId, squashedCausalBranch)

codebaseForProjectBranch :: AuthZReceipt -> Project -> Branch causal -> CodebaseEnv
codebaseForProjectBranch !_authZReceipt Project {ownerUserId} Branch {contributorId} =
  let loc = codebaseLocationForProjectBranchCodebase ownerUserId contributorId
   in CodebaseEnv {codebaseOwner = codebaseOwnerUserId loc}
