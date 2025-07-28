{-# LANGUAGE RecordWildCards #-}

module Share.Codebase.CodeCache (withCodeCache, toCodeLookup) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens
import Data.Map qualified as Map
import Share.Codebase qualified as Codebase
import Share.Codebase.Types
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Utils.Lens (asListOfDeduped)
import Unison.Builtin qualified as Builtin
import Unison.Codebase.CodeLookup qualified as CL
import Unison.DataDeclaration qualified as V1
import Unison.Parser.Ann
import Unison.Reference qualified as Reference
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Symbol (Symbol)
import Unison.Term qualified as V1
import Unison.Type qualified as V1

withCodeCache :: (QueryM m) => CodebaseEnv -> (forall s. CodeCache s -> m r) -> m r
withCodeCache codeCacheCodebaseEnv action = do
  codeCacheVar <- PG.transactionUnsafeIO (newTVarIO (CodeCacheData Map.empty Map.empty))
  let codeCache = CodeCache {codeCacheVar, codeCacheCodebaseEnv}
  action codeCache

readCodeCache :: (QueryM m) => CodeCache s -> m CodeCacheData
readCodeCache CodeCache {codeCacheVar} = PG.transactionUnsafeIO (readTVarIO codeCacheVar)

cacheTermAndTypes ::
  (QueryM m) =>
  CodeCache s ->
  [(Reference.Id, (V1.Term Symbol Ann, V1.Type Symbol Ann))] ->
  m ()
cacheTermAndTypes CodeCache {codeCacheVar} termAndTypes = do
  PG.transactionUnsafeIO do
    atomically do
      modifyTVar' codeCacheVar \CodeCacheData {termCache, ..} ->
        let newTermMap = Map.fromList termAndTypes
            termCache' = Map.union termCache newTermMap
         in CodeCacheData {termCache = termCache', ..}

cacheDecls ::
  (QueryM m) =>
  CodeCache s ->
  [(Reference.Id, V1.Decl Symbol Ann)] ->
  m ()
cacheDecls CodeCache {codeCacheVar} decls = do
  PG.transactionUnsafeIO do
    atomically do
      modifyTVar' codeCacheVar \CodeCacheData {typeCache, ..} ->
        let newDeclsMap = Map.fromList decls
            typeCache' = Map.union typeCache newDeclsMap
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
  let getTerm refId = fmap fst <$> getTermsAndTypesOf codeCache id refId
  let getTypeOfTerm refId = fmap snd <$> getTermsAndTypesOf codeCache id refId
  let getTypeDeclaration refId = getTypeDeclsOf codeCache id refId
  CL.CodeLookup {getTerm, getTypeOfTerm, getTypeDeclaration}
    <> builtinsCodeLookup

getTermsAndTypesOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t Reference.Id (Maybe (V1.Term Symbol Ann, V1.Type Symbol Ann)) ->
  s ->
  m t
getTermsAndTypesOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {termCache} <- readCodeCache codeCache
  s
    & asListOfDeduped trav %%~ \refs -> do
      -- Parition by cache misses
      let partitioned =
            refs
              <&> \r ->
                case Map.lookup r termCache of
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

      cacheTermAndTypes codeCache cacheable
      pure $ hydrated'

getTypeDeclsOf ::
  (QueryM m) =>
  CodeCache scope ->
  Traversal s t Reference.Id (Maybe (V1.Decl Symbol Ann)) ->
  s ->
  m t
getTypeDeclsOf codeCache@(CodeCache {codeCacheCodebaseEnv}) trav s = do
  CodeCacheData {typeCache} <- readCodeCache codeCache
  s
    & asListOfDeduped trav %%~ \refs -> do
      -- Parition by cache misses
      let partitioned =
            refs
              <&> \r ->
                case Map.lookup r typeCache of
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

      cacheDecls codeCache cacheable
      pure $ hydrated'
