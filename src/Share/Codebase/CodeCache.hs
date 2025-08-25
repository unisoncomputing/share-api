{-# LANGUAGE RecordWildCards #-}

module Share.Codebase.CodeCache
  ( withCodeCache,
    toCodeLookup,
    termsForRefsOf,
    typesOfReferentsOf,
    getTermsAndTypesByRefIdsOf,
    expectTermsAndTypesByRefIdsOf,
    getTypeDeclsByRefIdsOf,
    expectTypeDeclsByRefIdsOf,
    getTypeDeclsByRefsOf,
    cacheTermAndTypes,
    cacheDecls,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens
import Control.Monad.State.Strict qualified as State
import Data.Map qualified as Map
import Data.Text qualified as Text
import Share.Codebase qualified as Codebase
import Share.Codebase.Types
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Utils.Lens (asListOfDeduped)
import U.Codebase.Reference (Reference)
import U.Codebase.Reference qualified as Reference
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Codebase.CodeLookup qualified as CL
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration.ConstructorId qualified as V1Decl
import Unison.Hash (Hash)
import Unison.Parser.Ann
import Unison.Reference (TermReferenceId, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as V1Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Term qualified as V1
import Unison.Type qualified as Type
import Unison.Type qualified as V1

type TermAndType =
  (V1.Term Symbol Ann, V1.Type Symbol Ann)

withCodeCache :: (QueryM m) => CodebaseEnv -> (forall s. CodeCache s -> m r) -> m r
withCodeCache codeCacheCodebaseEnv action = do
  codeCacheVar <- PG.transactionUnsafeIO (newTVarIO (CodeCacheData Map.empty Map.empty))
  let codeCache = CodeCache {codeCacheVar, codeCacheCodebaseEnv}
  action codeCache

readCodeCache :: (QueryM m) => CodeCache s -> m CodeCacheData
readCodeCache CodeCache {codeCacheVar} = PG.transactionUnsafeIO (readTVarIO codeCacheVar)

cacheTermAndTypes :: (QueryM m) => CodeCache s -> Map TermReferenceId TermAndType -> m ()
cacheTermAndTypes CodeCache {codeCacheVar} termAndTypes = do
  PG.transactionUnsafeIO do
    atomically do
      modifyTVar' codeCacheVar \CodeCacheData {termCache, ..} ->
        let !termCache' = Map.union termCache termAndTypes
         in CodeCacheData {termCache = termCache', ..}

cacheDecls ::
  (QueryM m) =>
  CodeCache s ->
  Map TypeReferenceId (V1.Decl Symbol Ann) ->
  m ()
cacheDecls CodeCache {codeCacheVar} decls = do
  PG.transactionUnsafeIO do
    atomically do
      modifyTVar' codeCacheVar \CodeCacheData {typeCache, ..} ->
        let !typeCache' = Map.union typeCache decls
         in CodeCacheData {typeCache = typeCache', ..}

builtinsCodeLookup :: (Monad m) => CL.CodeLookup Symbol m Ann
builtinsCodeLookup =
  Builtin.codeLookup
    <> IOSource.codeLookupM

-- | Build a Unison 'CodeLookup' which is backed by the given 'CodeCache'.
-- The TVar will be baked in, so it will still share the cache with the CodeCache it's
-- built from.
toCodeLookup :: CodeCache s -> CL.CodeLookup Symbol (PG.Transaction e) Ann
toCodeLookup codeCache = do
  let getTerm refId = fmap fst <$> getTermsAndTypesByRefIdsOf codeCache id refId
  let getTypeOfTerm refId = fmap snd <$> getTermsAndTypesByRefIdsOf codeCache id refId
  let getTypeDeclaration refId = getTypeDeclsByRefIdsOf codeCache id refId
  CL.CodeLookup {getTerm, getTypeOfTerm, getTypeDeclaration}
    <> builtinsCodeLookup

getTermsAndTypesByRefIdsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t TermReferenceId (Maybe TermAndType) ->
  s ->
  m t
getTermsAndTypesByRefIdsOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {termCache} <- readCodeCache codeCache
  s
    & asListOfDeduped trav %%~ \refs -> do
      -- Partition by cache misses
      let partitioned =
            refs
              <&> \r ->
                case findBuiltinTT r <|> Map.lookup r termCache of
                  Just termAndType -> Right termAndType
                  Nothing -> Left (r, r)
      -- Load the missing terms and types
      withUncachedLoaded <- Codebase.loadV1TermAndTypeByRefIdsOf codeCacheCodebaseEnv (traversed . _Left . _2) partitioned
      -- Pull out the new things to cache, and merge newly loaded and cached results.
      let (cacheable, hydrated') =
            withUncachedLoaded
              & traversed %%~ \case
                Left (r, mayTT) ->
                  case mayTT of
                    Just tt -> ([(r, tt)], Just tt)
                    Nothing -> (mempty, Nothing)
                Right tt -> (mempty, Just tt)

      cacheTermAndTypes codeCache (Map.fromList cacheable)
      pure hydrated'

-- | Like 'getTermsAndTypesByRefIdsOf', but throws an unrecoverable error when the term isn't in the database.
expectTermsAndTypesByRefIdsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t TermReferenceId TermAndType ->
  s ->
  m t
expectTermsAndTypesByRefIdsOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {termCache} <- readCodeCache codeCache
  s
    & asListOf trav %%~ \refs -> do
      -- Partition by cache misses
      let terms0 :: [Either (TermReferenceId, TermReferenceId) TermAndType]
          terms0 =
            refs
              <&> \ref ->
                case findBuiltinTT ref <|> Map.lookup ref termCache of
                  Just termAndType -> Right termAndType
                  Nothing -> Left (ref, ref)

      -- Fetch all cache misses from database
      terms1 :: [Either (TermReferenceId, TermAndType) TermAndType] <-
        Codebase.expectTermsByRefIdsOf codeCacheCodebaseEnv (traversed . _Left . _2) terms0

      -- Tease out the just-fetched things to add to the cache
      let terms2 :: [TermAndType]
          justFetched :: Map TermReferenceId TermAndType
          (terms2, justFetched) = teaseOutJustFetched terms1

      cacheTermAndTypes codeCache justFetched
      pure terms2

findBuiltinTT :: TermReferenceId -> Maybe TermAndType
findBuiltinTT refId = do
  tm <- runIdentity $ CL.getTerm builtinsCodeLookup refId
  typ <- runIdentity $ CL.getTypeOfTerm builtinsCodeLookup refId
  pure (tm, typ)

getTypeDeclsByRefIdsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t TypeReferenceId (Maybe (V1.Decl Symbol Ann)) ->
  s ->
  m t
getTypeDeclsByRefIdsOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {typeCache} <- readCodeCache codeCache
  s
    & asListOfDeduped trav %%~ \refs -> do
      -- Partition by cache misses
      let partitioned =
            refs
              <&> \r ->
                case findBuiltinDecl r <|> Map.lookup r typeCache of
                  Just decl -> Right decl
                  Nothing -> Left (r, r)
      -- Load the missing type declarations
      withUncachedLoaded <- Codebase.loadV1TypeDeclarationsByRefIdsOf codeCacheCodebaseEnv (traversed . _Left . _2) partitioned
      -- Pull out the new things to cache, and merge newly loaded and cached results.
      let (cacheable, hydrated') =
            withUncachedLoaded
              & traversed %%~ \case
                Left (r, mayDecl) ->
                  case mayDecl of
                    Just decl -> ([(r, decl)], Just decl)
                    Nothing -> (mempty, Nothing)
                Right decl -> (mempty, Just decl)

      cacheDecls codeCache (Map.fromList cacheable)
      pure hydrated'

expectTypeDeclsByRefIdsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t TypeReferenceId (V1.Decl Symbol Ann) ->
  s ->
  m t
expectTypeDeclsByRefIdsOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {typeCache} <- readCodeCache codeCache
  s
    & asListOf trav %%~ \refs -> do
      -- Partition by cache misses
      let types0 :: [Either (TypeReferenceId, TypeReferenceId) (V1.Decl Symbol Ann)]
          types0 =
            refs
              <&> \ref ->
                case findBuiltinDecl ref <|> Map.lookup ref typeCache of
                  Just typ -> Right typ
                  Nothing -> Left (ref, ref)

      -- Fetch all cache misses from database
      types1 :: [Either (TypeReferenceId, V1.Decl Symbol Ann) (V1.Decl Symbol Ann)] <-
        Codebase.expectTypeDeclarationsByRefIdsOf codeCacheCodebaseEnv (traversed . _Left . _2) types0

      -- Tease out the just-fetched things to add to the cache
      let types2 :: [V1.Decl Symbol Ann]
          justFetched :: Map TypeReferenceId (V1.Decl Symbol Ann)
          (types2, justFetched) = teaseOutJustFetched types1

      cacheDecls codeCache justFetched
      pure types2

findBuiltinDecl :: Reference.Id -> Maybe (V1.Decl Symbol Ann)
findBuiltinDecl refId = do
  runIdentity $ CL.getTypeDeclaration builtinsCodeLookup refId

-- Tease out the just-fetched things to add to the cache
teaseOutJustFetched :: forall a ref. (Ord ref) => [Either (ref, a) a] -> ([a], Map ref a)
teaseOutJustFetched terms1 =
  runState (traverse recordJustFetched terms1) Map.empty
  where
    recordJustFetched :: Either (ref, a) a -> State (Map ref a) a
    recordJustFetched = \case
      Left (ref, term) -> do
        State.modify' (Map.insert ref term)
        pure term
      Right term -> pure term

getTypeDeclsByRefsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t Reference (Maybe (V1.Decl Symbol Ann)) ->
  s ->
  m t
getTypeDeclsByRefsOf codeCache trav s = do
  s
    & asListOf trav %%~ \refs ->
      do
        let derivedIds_ :: Traversal Reference (Maybe (V1.Decl Symbol Ann)) Reference.Id (Maybe (V1.Decl Symbol Ann))
            derivedIds_ f = \case
              -- Builtins don't have decls
              (Reference.Builtin _) -> pure Nothing
              -- Otherwise look up the declaration by its derived id
              Reference.DerivedId refId -> f refId
        getTypeDeclsByRefIdsOf codeCache (traversed . derivedIds_) refs

termsForRefsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t Reference (Maybe (V1.Term Symbol ())) ->
  s ->
  m t
termsForRefsOf codeCache trav s = do
  s
    & asListOf trav %%~ \refs ->
      do
        let trav :: Traversal Reference (Maybe (V1.Term Symbol ())) Reference.Id (Maybe TermAndType)
            trav f = \case
              -- Builtins are their own terms
              ref@(Reference.Builtin _) -> pure (Just (Term.ref () ref))
              -- Otherwise look up the term by its derived id
              Reference.DerivedId refId -> do
                (fmap (Term.amap (const ()) . fst)) <$> f refId
        getTermsAndTypesByRefIdsOf codeCache (traversed . trav) refs

-- | Get the types of terms or constructors.
typesOfReferentsOf ::
  (QueryM m) =>
  (HasCallStack) =>
  CodeCache scope ->
  Traversal s t V1Referent.Referent (Maybe (V1.Type Symbol ())) ->
  s ->
  m t
typesOfReferentsOf codeCache trav s = do
  s
    & asListOf trav %%~ \refs -> do
      -- Partition by builtins, constructors, and term references
      let partitioned ::
            [ Either
                (Type.Type Symbol () {- builtins we knew the types of -})
                ( Either
                    (Reference.Id' Hash {- term references -})
                    (Reference.Id' Hash, V1Decl.ConstructorId {- constructors -})
                )
            ] =
              refs <&> \case
                V1Referent.Ref ref
                  | Just typ <-
                      Map.lookup ref Builtin.termRefTypes ->
                      -- Annoyingly, the builtins Code lookup can only resolve the types of
                      -- Reference.Ids, even though builtins have types too!
                      Left (ABT.amap (const ()) typ)
                V1Referent.Ref (Reference.DerivedId refId) -> Right (Left refId)
                V1Referent.Ref (Reference.Builtin t) -> error $ "typesOfReferentsOf: Builtin without known type: " <> Text.unpack t
                V1Referent.Con (ConstructorReference (Reference.Builtin {}) _) _ct -> error "typesOfReferentsOf: No such thing as a builtin constructor"
                V1Referent.Con (ConstructorReference (Reference.DerivedId refId) conId) _ct -> do
                  Right (Right (refId, conId))
      withTermTypes ::
        [ Either
            (V1.Type Symbol ())
            ( Either
                (Maybe TermAndType)
                (Reference.Id' Hash, V1Decl.ConstructorId)
            )
        ] <-
        getTermsAndTypesByRefIdsOf codeCache (traversed . _Right . _Left) partitioned
      withTypeDecls ::
        [ Either
            (V1.Type Symbol ())
            ( Either
                (Maybe TermAndType)
                (Maybe (V1.Decl Symbol Ann), V1Decl.ConstructorId)
            )
        ] <-
        getTypeDeclsByRefIdsOf codeCache (traversed . _Right . _Right . _1) withTermTypes
      withTypeDecls
        <&> ( \case
                Left typ -> Just typ
                Right (Left mayTT) -> ABT.amap (const ()) . snd <$> mayTT
                Right (Right (mayDecl, conId)) ->
                  case mayDecl of
                    Nothing -> Nothing
                    Just decl ->
                      ABT.amap (const ()) <$> DD.typeOfConstructor (DD.asDataDecl (decl)) conId
            )
        & pure
