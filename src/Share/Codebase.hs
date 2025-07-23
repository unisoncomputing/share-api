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
    loadTermAndTypeByRefIdsOf,
    expectTermsByRefIdsOf,
    loadTypesOfTermsOf,
    expectTypesOfTermsOf,
    expectTypesOfReferentsOf,
    expectTypesOfConstructorsOf,
    loadTypesOfConstructorsOf,
    loadTypesOfReferentsOf,
    loadTypeDeclarationsByRefIdsOf,
    expectTypeDeclarationsByRefIdsOf,
    loadDeclKindsOf,
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
    convertTerms2to1Of,

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
import Share.Utils.Lens (asListOfDeduped)
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
codebaseRuntimeTransaction unisonRuntime codebase = do
  cacheVar <- newTVarIO (CodeLookupCache mempty mempty)
  pure
    CodebaseRuntime
      { codeLookup = codeLookupForUser cacheVar codebase,
        cachedEvalResult = (fmap . fmap) Term.unannotate . loadCachedEvalResult codebase,
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

-- | Load terms and type.
loadTermAndTypeByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t TermReferenceId (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann)) -> s -> m t
loadTermAndTypeByRefIdsOf codebase trav s = do
  s
    & asListOf trav %%~ \refs -> do
      let hashes = refs <&> \(Reference.Id h _) -> h
      termsAndTypes <- DefnQ.loadTermsByRefIdsOf codebase traversed refs
      let termInfo =
            (zip hashes termsAndTypes) <&> \case
              (_h, Nothing) -> Nothing
              (h, Just (v2Term, v2Type)) -> Just (h, v2Term, v2Type)
      convertTerms2to1Of (traversed . _Just) termInfo

convertTerms2to1Of :: (PG.QueryM m) => Traversal s t (Hash, V2.Term.Term V2.Symbol, V2.Term.Type V2.Symbol) (V1.Term Symbol Ann, V1.Type Symbol Ann) -> s -> m t
convertTerms2to1Of trav s = do
  s
    & asListOf trav %%~ \termInfos -> do
      for termInfos \(h, v2Term, v2Type) -> do
        -- TODO: We need to batchify both term2to1 and have it accept a batched 'expectDeclKind' so we're not making a separate query for every decl!
        v1Term <- Cv.term2to1 h (expectDeclKindsOf id) v2Term
        let v1Type = Cv.ttype2to1 v2Type
        pure (v1Term, v1Type)

expectTermsByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t TermReferenceId (V1.Term Symbol Ann, V1.Type Symbol Ann) -> s -> m t
expectTermsByRefIdsOf codebase trav s = do
  s & asListOfDeduped trav \refIds -> do
    termsAndTypes <- loadTermAndTypeByRefIdsOf codebase traversed refIds
    for (zip refIds termsAndTypes) \case
      (refId, Nothing) -> unrecoverableError (MissingTerm refId)
      (_, Just (term, typ)) -> pure (term, typ)

-- | Load the type of a term.
-- Differs from `loadTerm` in that it can also return the type of builtins.
loadTypesOfTermsOf :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Reference (Maybe (V1.Type Symbol Ann)) -> s -> m t
loadTypesOfTermsOf codebase trav s =
  s
    & asListOf trav %%~ \refs -> do
      let partitionedRefs =
            refs <&> \case
              Reference.DerivedId h -> Right h
              r@Reference.Builtin {} ->
                let builtinType =
                      Map.lookup r Builtin.termRefTypes
                        <&> \typ ->
                          (typ $> builtinAnnotation)
                 in Left builtinType
      results <- loadTermAndTypeByRefIdsOf codebase (traversed . _Right) partitionedRefs
      pure $
        results <&> \case
          Left builtin -> builtin
          Right mayTermAndType -> snd <$> mayTermAndType
  where
    builtinAnnotation = Ann.Intrinsic

expectTypesOfTermsOf :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Reference (V1.Type Symbol Ann) -> s -> m t
expectTypesOfTermsOf codebase trav s = do
  s
    & asListOf trav %%~ \refs -> do
      results <- loadTypesOfTermsOf codebase traversed refs
      for (zip refs results) \case
        (r, Nothing) -> unrecoverableError (MissingTypeForTerm r)
        (_, Just typ) -> pure typ

expectTypesOfReferentsOf :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Referent (V1.Type Symbol Ann) -> s -> m t
expectTypesOfReferentsOf codebase trav s =
  s
    & asListOf trav %%~ \referents -> do
      let partitionedRefs =
            referents <&> \case
              V2.Ref r -> Left r
              V2.Con r conId -> Right (r, conId)
      withTermTypes <- expectTypesOfTermsOf codebase (traversed . _Left) partitionedRefs
      withConstructorTypes <- expectTypesOfConstructorsOf codebase (traversed . _Right) withTermTypes
      pure $ fmap (either id id) withConstructorTypes

expectDeclKindsOf :: (PG.QueryM m) => Traversal s t Reference.TypeReference CT.ConstructorType -> s -> m t
expectDeclKindsOf trav s = do
  s
    & asListOf trav %%~ \refs -> do
      results <- loadDeclKindsOf traversed refs
      for (zip refs results) \case
        (r, Nothing) -> unrecoverableError (DefnQ.missingDeclKindError r)
        (_, Just ct) -> pure ct

loadDeclKindsOf :: (PG.QueryM m) => Traversal s t Reference.TypeReference (Maybe CT.ConstructorType) -> s -> m t
loadDeclKindsOf trav s =
  s
    & asListOf trav %%~ \refs -> do
      xs <-
        refs
          & ( fmap \case
                V2.ReferenceBuiltin t -> Left t
                V2.ReferenceDerived refId -> Right refId
            )
          & traversed . _Left %~ (\t -> Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType)
          & DefnQ.loadDeclKindsOf (traversed . _Right)
      pure (fmap (either id id) xs)

loadTypeDeclarationsByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t Reference.Id (Maybe (V1.Decl Symbol Ann)) -> s -> m t
loadTypeDeclarationsByRefIdsOf codebase trav s =
  s
    & asListOf trav %%~ \refIds -> do
      DefnQ.loadDeclsByRefIdsOf codebase traversed refIds
        <&> \decls ->
          zip refIds decls <&> \case
            (_, Nothing) -> Nothing
            ((Reference.Id h _), Just decl) -> Just (Cv.decl2to1 h decl)

expectTypeDeclarationsByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t Reference.Id (V1.Decl Symbol Ann) -> s -> m t
expectTypeDeclarationsByRefIdsOf codebase trav s = do
  s
    & asListOf trav %%~ \refIds -> do
      results <- loadTypeDeclarationsByRefIdsOf codebase traversed refIds
      for (zip refIds results) \case
        (refId, Nothing) -> unrecoverableError (MissingDecl refId)
        (_, Just decl) -> pure decl

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

-- | Load the types of referents
loadTypesOfReferentsOf :: (QueryM m) => CodebaseEnv -> Traversal s t V2.Referent (Maybe (V1.Type Symbol Ann)) -> s -> m t
loadTypesOfReferentsOf codebase trav s =
  s
    & asListOf trav %%~ \referents -> do
      let partitionedRefs =
            referents <&> \case
              V2.Ref r -> Left r
              V2.Con r conId -> Right (r, conId)
      withTermTypes <- loadTypesOfTermsOf codebase (traversed . _Left) partitionedRefs
      withConstructorTypes <- loadTypesOfConstructorsOf codebase (traversed . _Right) withTermTypes
      pure $ (either id id <$> withConstructorTypes)

-- | Load the type of a constructor.
loadTypesOfConstructorsOf :: (QueryM m) => CodebaseEnv -> Traversal s t (V2.TypeReference, V2.ConstructorId) (Maybe (V1.Type Symbol Ann)) -> s -> m t
loadTypesOfConstructorsOf codebase trav s =
  s
    & asListOf trav %%~ \refs -> do
      let partitionedRefs =
            refs <&> \case
              (Reference.Builtin _t, _conId) -> Left Nothing
              (Reference.DerivedId refId, conId) -> Right ((refId, conId), refId)
      results <- loadTypeDeclarationsByRefIdsOf codebase (traversed . _Right . _2) partitionedRefs
      for results \case
        (Right ((refId, _conId), Nothing)) -> (unrecoverableError (MissingDecl refId))
        (Right ((_refId, conId), (Just decl))) -> do
          pure $ DD.typeOfConstructor (DD.asDataDecl decl) conId
        (Left x) -> pure x

expectTypesOfConstructorsOf :: (QueryM m) => CodebaseEnv -> Traversal s t (V2.TypeReference, V2.ConstructorId) (V1.Type Symbol Ann) -> s -> m t
expectTypesOfConstructorsOf codebase trav s =
  s
    & asListOf trav %%~ \refs -> do
      results <- loadTypesOfConstructorsOf codebase traversed refs
      for (zip refs results) \case
        ((ref, conId), Nothing) -> unrecoverableError (MissingTypeForConstructor ref conId)
        (_, Just r) -> pure r

data CodeLookupCache = CodeLookupCache
  { termCache :: Map Reference.Id (V1.Term Symbol Ann, V1.Type Symbol Ann),
    typeCache :: Map Reference.Id (V1.Decl Symbol Ann)
  }

-- | TODO: The code lookup should either be batchified or we should preload the cache with
-- everything we think we'll need.
codeLookupForUser :: TVar CodeLookupCache -> CodebaseEnv -> CL.CodeLookup Symbol (PG.Transaction e) Ann
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
          maybeTermAndType <- loadTermAndTypeByRefIdsOf codebaseOwner id r
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
          maybeType <- loadTypeDeclarationsByRefIdsOf codebaseOwner id r
          whenJust maybeType \typ ->
            PG.transactionUnsafeIO do
              atomically do
                modifyTVar' cacheVar \CodeLookupCache {typeCache, ..} ->
                  CodeLookupCache {typeCache = Map.insert r typ typeCache, ..}
          pure maybeType

-- | Look up the result of evaluating a term if we have it cached.
--
-- This is intentionally not in CodebaseM because it's used to build the CodebaseEnv.
loadCachedEvalResult :: CodebaseEnv -> Reference.Id -> PG.Transaction e (Maybe (V1.Term Symbol Ann))
loadCachedEvalResult codebase ref@(Reference.Id h _) = runMaybeT do
  v2Term <- MaybeT $ DefnQ.loadCachedEvalResult codebase ref
  -- TODO: Batchify this so we're not making a separate query for every reference in the term!
  lift $ Cv.term2to1 h (expectDeclKindsOf id) v2Term

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
