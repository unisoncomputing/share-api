{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Codebase
  ( shorthashLength,
    CodebaseEnv,
    codebaseOwner,
    CodebaseRuntime (..),
    codebaseEnv,
    withCodebaseRuntime,
    codebaseRuntimeTransaction,
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
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Servant qualified
import Servant.Server (err500)
import Share.Branch (Branch (..))
import Share.Codebase.Types
import Share.Codebase.Types qualified as Codebase
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres (QueryA, QueryM, unrecoverableError)
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

-- | Construct a Runtime linked to a specific codebase and transaction.
-- Don't use the runtime for one codebase with another codebase.
-- Don't use this runtime in any transaction other than the one where it's created.
withCodebaseRuntime :: (Exception e) => CodebaseEnv -> Runtime Symbol -> (forall s. CodebaseRuntime s IO -> PG.Transaction e r) -> PG.Transaction e r
withCodebaseRuntime codebaseEnv sandboxedRuntime f = do
  rt <- PG.transactionUnsafeIO (codebaseRuntimeTransaction sandboxedRuntime codebaseEnv)
  PG.asUnliftIOTransaction $ do
    UnliftIO.withRunInIO \toIO -> do
      toIO . PG.UnliftIOTransaction $ f $ hoistCodebaseRuntime (toIO . PG.UnliftIOTransaction) rt

-- | Ideally, we'd use this – a runtime with lookup actions in transaction, not IO. But that will require refactoring to
-- the runtime interface in ucm, so we can't use it for now. That's bad: we end up unsafely running separate
-- transactions for inner calls to 'codeLookup' / 'cachedEvalResult', which can lead to deadlock due to a starved
-- connection pool.
codebaseRuntimeTransaction :: Runtime Symbol -> CodebaseEnv -> IO (CodebaseRuntime s (PG.Transaction e))
codebaseRuntimeTransaction unisonRuntime CodebaseEnv {codebaseOwner} = do
  cacheVar <- newTVarIO (CodeLookupCache mempty mempty)
  pure
    CodebaseRuntime
      { codeLookup = codeLookupForUser cacheVar codebaseOwner,
        cachedEvalResult = (fmap . fmap) Term.unannotate . loadCachedEvalResult codebaseOwner,
        unisonRuntime
      }

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
loadTerm :: (QueryM m) => CodebaseEnv -> TermReferenceId -> m (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann))
loadTerm (CodebaseEnv {codebaseOwner}) refId = do
  loadTermForCodeLookup codebaseOwner refId

-- | Load a term and its type.
loadTermForCodeLookup :: (QueryM m) => UserId -> TermReferenceId -> m (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann))
loadTermForCodeLookup codebaseUser refId@(Reference.Id h _) = runMaybeT $ do
  (v2Term, v2Type) <- MaybeT $ DefnQ.loadTerm codebaseUser refId
  convertTerm2to1 h v2Term v2Type

convertTerm2to1 :: (PG.QueryM m) => Hash -> V2.Term.Term V2.Symbol -> V2.Term.Type V2.Symbol -> m (V1.Term Symbol Ann, V1.Type Symbol Ann)
convertTerm2to1 h v2Term v2Type = do
  v1Term <- Cv.term2to1 h expectDeclKind v2Term
  let v1Type = Cv.ttype2to1 v2Type
  pure (v1Term, v1Type)

expectTerm :: (QueryM m) => CodebaseEnv -> TermReferenceId -> m (V1.Term Symbol Ann, V1.Type Symbol Ann)
expectTerm codebase refId = loadTerm codebase refId `whenNothingM` (unrecoverableError (MissingTerm refId))

-- | Load the type of a term.
-- Differs from `loadTerm` in that it can also return the type of builtins.
loadTypeOfTerm :: (QueryM m) => CodebaseEnv -> V2.Reference -> m (Maybe (V1.Type Symbol Ann))
loadTypeOfTerm codebase r = case r of
  Reference.DerivedId h -> fmap snd <$> loadTerm codebase h
  r@Reference.Builtin {} -> do
    let builtinType =
          Map.lookup r Builtin.termRefTypes
            <&> \typ ->
              (typ $> builtinAnnotation)
    pure builtinType
  where
    builtinAnnotation = Ann.Intrinsic

expectTypeOfTerm :: (QueryM m) => CodebaseEnv -> V2.Reference -> m (V1.Type Symbol Ann)
expectTypeOfTerm codebase r = loadTypeOfTerm codebase r `whenNothingM` (unrecoverableError (MissingTypeForTerm r))

expectTypeOfTerms :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Reference (V1.Type Symbol Ann) -> s -> m t
expectTypeOfTerms codebase trav s = do
  s & trav %%~ expectTypeOfTerm codebase

expectTypeOfReferent :: (QueryM m) => CodebaseEnv -> V2.Referent -> m (V1.Type Symbol Ann)
expectTypeOfReferent codebase = \case
  V2.Ref r -> expectTypeOfTerm codebase r
  V2.Con r conId -> expectTypeOfConstructor codebase r conId

expectTypeOfReferents :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Referent (V1.Type Symbol Ann) -> s -> m t
expectTypeOfReferents codebase trav s = do
  s & trav %%~ expectTypeOfReferent codebase

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

loadTypeDeclaration :: (QueryM m) => CodebaseEnv -> Reference.Id -> m (Maybe (V1.Decl Symbol Ann))
loadTypeDeclaration (CodebaseEnv {codebaseOwner}) refId = do
  loadTypeDeclarationForCodeLookup codebaseOwner refId

loadTypeDeclarationForCodeLookup :: (QueryM m) => UserId -> Reference.Id -> m (Maybe (V1.Decl Symbol Ann))
loadTypeDeclarationForCodeLookup codebaseUser refId@(Reference.Id h _) =
  fmap (Cv.decl2to1 h) <$> DefnQ.loadDecl codebaseUser refId

expectTypeDeclaration :: (QueryM m) => CodebaseEnv -> Reference.Id -> m (V1.Decl Symbol Ann)
expectTypeDeclaration codebase refId = loadTypeDeclaration codebase refId `whenNothingM` (unrecoverableError (MissingDecl refId))

-- | Find all the term referents that match the given prefix.
-- Includes decl constructors.
termReferentsByShortHash :: (QueryM m) => ShortHash -> m (Set V1.Referent)
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

typeReferencesByShortHash :: (QueryA m) => ShortHash -> m (Set V1.Reference)
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
  (QueryM m) =>
  CodebaseEnv ->
  V2.Referent ->
  m (Maybe (V1.Type Symbol Ann))
loadTypeOfReferent codebase = \case
  V2.Ref r -> loadTypeOfTerm codebase r
  V2.Con r conId -> loadTypeOfConstructor codebase r conId

-- | Load the type of a constructor.
loadTypeOfConstructor :: (QueryM m) => CodebaseEnv -> V2.TypeReference -> V2.ConstructorId -> m (Maybe (V1.Type Symbol Ann))
loadTypeOfConstructor codebase ref conId = case ref of
  -- No constructors for builtin types.
  Reference.Builtin _txt -> pure Nothing
  Reference.DerivedId refId -> do
    decl <- loadTypeDeclaration codebase refId `whenNothingM` (unrecoverableError (MissingDecl refId))
    pure $ DD.typeOfConstructor (DD.asDataDecl decl) conId

expectTypeOfConstructor :: (QueryM m) => CodebaseEnv -> V2.TypeReference -> V2.ConstructorId -> m (V1.Type Symbol Ann)
expectTypeOfConstructor codebase ref conId =
  loadTypeOfConstructor codebase ref conId >>= \case
    Just typ -> pure typ
    Nothing -> (unrecoverableError (MissingTypeForConstructor ref conId))

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

saveCachedEvalResult :: (QueryM m) => CodebaseEnv -> Reference.Id -> V1.Term Symbol Ann -> m ()
saveCachedEvalResult codebase refId@(Reference.Id h _) term = do
  let v2Term = Cv.term1to2 h term
  DefnQ.saveCachedEvalResult codebase refId v2Term

expectCausalIdByHash :: (QueryM m) => CodebaseEnv -> CausalHash -> m CausalId
expectCausalIdByHash codebase@(CodebaseEnv {codebaseOwner}) causalHash = do
  (CausalQ.loadCausalIdByHash codebase causalHash) `whenNothingM` (unrecoverableError (MissingCausalForHash codebaseOwner causalHash))

-- | Sets the loose code root hash for a user and ensures the appropriate name lookup
-- exists.
setLooseCodeRoot :: (QueryM m) => CodebaseEnv -> UserId -> Maybe Text -> CausalId -> m ()
setLooseCodeRoot codebase caller description causalId = do
  branchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id causalId
  nlReceipt <- NLOps.ensureNameLookupForBranchId branchHashId
  LCQ.setLooseCodeRoot codebase nlReceipt caller description causalId

-- | Squash a causal and add the result to the codebase.
-- Also adds a name lookup for the squashed branch hash.
squashCausalAndAddToCodebase ::
  (QueryM m) =>
  CodebaseEnv ->
  CausalId ->
  -- Returns the new causal hash if successful, or Nothing if the source causal doesn't
  -- exist.
  m (Maybe CausalId)
squashCausalAndAddToCodebase codebase causalId = runMaybeT $ do
  causalBranch <- MaybeT (CausalQ.loadCausalNamespace causalId)
  (squashedCausalId, _squashedCausal) <- lift $ squashCausal codebase causalBranch
  squashedBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id squashedCausalId
  lift $ NLOps.ensureNameLookupForBranchId squashedBranchHashId
  pure squashedCausalId

-- Recursively discards history, resulting in a namespace tree with only single a single
-- Causal node at every level.
squashCausal :: (QueryM m) => CodebaseEnv -> V2.CausalBranch m -> m (CausalId, V2.CausalBranch m)
squashCausal codebase (Causal.Causal {valueHash = unsquashedBranchHash, value}) = do
  mayCachedSquashResult <- runMaybeT $ do
    causalId <- MaybeT (CausalQ.tryGetCachedSquashResult codebase unsquashedBranchHash)
    fmap (causalId,) . MaybeT $ CausalQ.loadCausalNamespace causalId
  case mayCachedSquashResult of
    Just cb -> pure cb
    Nothing -> do
      branch@V2.Branch {children} <- value
      squashedChildren <- traverse (squashCausal codebase) children
      let squashedBranchHead = branch {V2.children = snd <$> squashedChildren}
      (squashedBranchHashId, squashedBranchHash) <- CausalQ.saveV2BranchShallow codebase squashedBranchHead
      let ancestors = mempty
      (squashedCausalId, squashedCausalHash) <- CausalQ.saveCausal codebase Nothing Nothing squashedBranchHashId ancestors
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
