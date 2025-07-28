{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Share.Postgres.Definitions.Queries
  ( loadTermsByIdsOf,
    loadTermsByRefIdsOf,
    expectTermsByIdsOf,
    expectTermsByRefIdsOf,
    expectTermIdsByRefIdsOf,
    expectComponentHashIdsByTermIdsOf,
    expectComponentHashIdsByTypeIdsOf,
    saveTermComponent,
    saveEncodedTermComponent,
    termTagsByReferentsOf,
    typeTagsByReferencesOf,
    expectShareTermComponent,
    expectShareTypeComponentsOf,
    loadDeclKindsOf,
    loadDeclsByRefIdsOf,
    expectDeclsByRefIdsOf,
    loadDeclByTypeComponentElementAndTypeIdsOf,
    expectTypeComponentElementsAndTypeIdsOf,
    loadCachedEvalResult,
    saveCachedEvalResult,
    termReferencesByPrefix,
    declReferencesByPrefix,
    constructorReferentsByPrefix,
    ensureTextIds,
    ensureTextIdsOf,
    ensureBytesIds,
    ensureBytesIdsOf,
    expectTextsOf,
    saveTypeComponent,
    termTransitiveDependencies,

    -- * For Migrations
    saveSerializedComponent,

    -- * Errors
    expectedTermError,
    expectedTypeError,
    missingDeclKindError,
  )
where

import Control.Lens
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Servant (err500)
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Types
import Share.Postgres.Definitions.Types qualified as DefnTypes
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.Lens (asListOfDeduped)
import Share.Utils.Logging qualified as Logging
import Share.Utils.Postgres (OrdBy, RawBytes (..), ordered)
import Share.Web.Errors (ErrorID (..), InternalServerError (InternalServerError), ToServerError (..))
import U.Codebase.Decl qualified as Decl
import U.Codebase.Decl qualified as V2 hiding (Type)
import U.Codebase.Decl qualified as V2Decl
import U.Codebase.Reference qualified as Reference
import U.Codebase.Reference qualified as V2
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Queries qualified as Util
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Term qualified as V2
import U.Codebase.Type qualified as V2.Type
import U.Codebase.Type qualified as V2Type
import Unison.Builtin.Decls qualified as Decls
import Unison.ConstructorReference qualified as V1Referent
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Reference qualified as V1Reference
import Unison.Referent qualified as V1Referent
import Unison.Runtime.IOSource qualified as Decls
import Unison.Server.Types qualified as Tags
import Unison.Sync.Common qualified as SyncCommon
import Unison.Sync.Types qualified as Share
import Unison.SyncV2.Types (CBORBytes (..))
import Unison.SyncV2.Types qualified as SyncV2

type ResolvedLocalIds = LocalIds.LocalIds' Text ComponentHash

type PgLocalIds = LocalIds.LocalIds' TextId ComponentHashId

data DefinitionQueryError
  = ExpectedTermNotFound (Either TermId TermReferenceId)
  | ExpectedTermComponentNotFound ComponentHashId
  | ExpectedTypeNotFound (Either TypeId TypeReferenceId)
  | ExpectedTypeComponentNotFound ComponentHashId
  deriving stock (Show)

instance ToServerError DefinitionQueryError where
  toServerError = \case
    ExpectedTermNotFound {} -> (ErrorID "expected-term-not-found", err500)
    ExpectedTermComponentNotFound {} -> (ErrorID "expected-term-component-not-found", err500)
    ExpectedTypeNotFound {} -> (ErrorID "expected-type-not-found", err500)
    ExpectedTypeComponentNotFound {} -> (ErrorID "expected-type-component-not-found", err500)

instance Logging.Loggable DefinitionQueryError where
  toLog = \case
    (ExpectedTermNotFound refId) ->
      Logging.textLog ("Expected term not found: " <> tShow refId)
        & Logging.withSeverity Logging.Error
    (ExpectedTermComponentNotFound componentRef) ->
      Logging.textLog ("Expected term component not found: " <> tShow componentRef)
        & Logging.withSeverity Logging.Error
    (ExpectedTypeNotFound refId) ->
      Logging.textLog ("Expected type not found: " <> tShow refId)
        & Logging.withSeverity Logging.Error
    (ExpectedTypeComponentNotFound componentRef) ->
      Logging.textLog ("Expected type component not found: " <> tShow componentRef)
        & Logging.withSeverity Logging.Error

loadTermIdsByRefIdsOf :: (QueryA m, HasCallStack) => Traversal s t TermReferenceId (Maybe TermId) -> s -> m t
loadTermIdsByRefIdsOf trav s = do
  s
    & asListOf trav %%~ \refIds -> do
      let refsTable :: [(OrdBy, Hash, PgComponentIndex)]
          refsTable =
            ordered refIds
              <&> \(ord, (Reference.Id compHash (pgComponentIndex -> compIndex))) -> (ord, compHash, compIndex)
      queryListCol @(Maybe TermId)
        [sql|
        WITH ref_ids(ord, comp_hash, comp_index) AS(
          SELECT t.ord, t.comp_hash, t.comp_index FROM ^{toTable refsTable} AS t(ord, comp_hash, comp_index)
        )
        SELECT term.id
          FROM ref_ids
            LEFT JOIN component_hashes ch ON ch.base32 = ref_ids.comp_hash
            LEFT JOIN terms term ON (term.component_hash_id = ch.id AND term.component_index = ref_ids.comp_index)
        ORDER BY ref_ids.ord ASC
      |]

expectTermIdsByRefIdsOf :: (QueryA m) => Traversal s t TermReferenceId TermId -> s -> m t
expectTermIdsByRefIdsOf trav s =
  s
    & asListOf trav %%~ \refIds -> do
      loadTermIdsByRefIdsOf traversed refIds
        & unrecoverableEitherMap
          ( \results ->
              for (zip refIds results) \case
                (refId, Nothing) -> Left (expectedTermError $ Right refId)
                (_refId, Just termId) -> Right termId
          )

expectComponentHashIdsByTermIdsOf :: (QueryA m) => Traversal s t TermId ComponentHashId -> s -> m t
expectComponentHashIdsByTermIdsOf trav s = do
  s
    & asListOf trav %%~ \termIds -> do
      queryListCol @ComponentHashId
        [sql| SELECT term.component_hash_id
              FROM ^{toTable (ordered termIds)} AS term_ids(ord, term_id)
              JOIN terms term ON term.id = term_ids.term_id
              ORDER BY term_ids.ord ASC
            |]

expectComponentHashIdsByTypeIdsOf :: (QueryA m) => Traversal s t TypeId ComponentHashId -> s -> m t
expectComponentHashIdsByTypeIdsOf trav s = do
  s
    & asListOf trav %%~ \typeIds -> do
      queryListCol @ComponentHashId
        [sql| SELECT typ.component_hash_id
              FROM ^{toTable (ordered typeIds)} AS type_ids(ord, type_id)
              JOIN types typ ON typ.id = type_ids.type_id
              ORDER BY type_ids.ord ASC
            |]

expectTermsByRefIdsOf :: (HasCallStack, QueryM m) => CodebaseEnv -> Traversal s t TermReferenceId (V2.Term Symbol, V2.Type Symbol) -> s -> m t
expectTermsByRefIdsOf codebase trav s = do
  s & asListOfDeduped trav \termRefs -> do
    termIds <- expectTermIdsByRefIdsOf traversed termRefs
    expectTermsByIdsOf codebase traversed termIds

-- | Helper for loading term components efficiently for sync.
expectShareTermComponent :: (QueryM m) => CodebaseEnv -> ComponentHashId -> m (Share.TermComponent Text Hash32)
expectShareTermComponent (CodebaseEnv {codebaseOwner}) componentHashId = do
  componentElements :: NonEmpty (TermId, LocalTermBytes) <-
    ( queryListRows
        [sql| SELECT term.id, bytes.bytes
           FROM terms term
           LEFT JOIN sandboxed_terms sandboxed ON term.id = sandboxed.term_id
           LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
           WHERE term.component_hash_id = #{componentHashId}
             AND sandboxed.user_id = #{codebaseOwner}
           ORDER BY term.component_index ASC
      |]
        -- Ensure we get at least one index, and that we have bytes saved for each part of the
        -- component.
        <&> checkElements
      )
      `whenNothingM` do
        unrecoverableError $ InternalServerError "expected-term-component" (ExpectedTermComponentNotFound componentHashId)
  second (Hash32.fromHash . unComponentHash) . Share.TermComponent . toList <$> for componentElements \(termId, LocalTermBytes bytes) ->
    (,bytes) <$> termLocalReferencesOf id termId
  where
    checkElements :: [(TermId, Maybe LocalTermBytes)] -> Maybe (NonEmpty (TermId, LocalTermBytes))
    checkElements rows =
      sequenceAOf (traversed . _2) rows
        >>= NonEmpty.nonEmpty

-- | Helper for loading type components efficiently for sync.
expectShareTypeComponentsOf :: forall m s t. (QueryM m) => CodebaseEnv -> Traversal s t ComponentHashId (Share.DeclComponent Text Hash32) -> s -> m t
expectShareTypeComponentsOf CodebaseEnv {codebaseOwner} trav s = do
  s
    & asListOf trav %%~ \componentHashIds -> do
      componentElements <- componentElementsOf traversed componentHashIds
      checkedElements :: [NonEmpty (TypeId, LocalTypeBytes)] <- for (zip componentHashIds componentElements) checkElements
      typeLocalReferencesOf (traversed . traversed . _1) checkedElements
        <&> fmap \elements ->
          elements
            <&> (\(locals, LocalTypeBytes bytes) -> ((Hash32.fromHash . unComponentHash) <$> locals, bytes))
            & NonEmpty.toList
            & Share.DeclComponent
  where
    componentElementsOf :: forall s t. Traversal s t ComponentHashId [(TypeId, Maybe LocalTypeBytes)] -> s -> m t
    componentElementsOf trav s = do
      s & asListOf trav \componentHashIds -> do
        queryListCol @[TupleVal TypeId (Maybe LocalTypeBytes)]
          [sql| WITH component_hashes(ord, component_hash_id) AS (
                SELECT t.ord, t.component_hash_id FROM ^{toTable (ordered componentHashIds)} AS t(ord, component_hash_id)
              )
              SELECT (
                SELECT COALESCE(array_agg((typ.id, bytes.bytes) ORDER BY typ.component_index ASC), '{}') as type_elements
                  FROM types typ
                    LEFT JOIN sandboxed_types sandboxed ON typ.id = sandboxed.type_id
                    LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
                  WHERE ch.component_hash_id = typ.component_hash_id
                        AND sandboxed.user_id = #{codebaseOwner}
              )
              FROM component_hashes ch
              ORDER BY ch.ord ASC
            |]
          <&> (fmap . fmap) \(TupleVal (typeId, bytes)) -> (typeId, bytes)
    checkElements :: (ComponentHashId, [(TypeId, Maybe LocalTypeBytes)]) -> m (NonEmpty (TypeId, LocalTypeBytes))
    checkElements (componentHashId, rows) =
      case NonEmpty.nonEmpty rows of
        Nothing -> unrecoverableError $ InternalServerError "expected-type-component" (ExpectedTypeComponentNotFound componentHashId)
        Just nonEmptyRows ->
          for nonEmptyRows \case
            (_typeId, Nothing) -> unrecoverableError $ InternalServerError "expected-type-component" (ExpectedTypeComponentNotFound componentHashId)
            (typeId, Just bytes) -> pure (typeId, bytes)

-- | Batch load terms by ids.
loadTermsByIdsOf ::
  (QueryA m, HasCallStack) =>
  CodebaseEnv ->
  Traversal s t TermId (Maybe (V2.Term Symbol, V2.Type Symbol)) ->
  s ->
  m t
loadTermsByIdsOf codebase trav s = do
  s & asListOfDeduped trav \termIds -> do
    zipWith combine
      <$> (loadTermComponentElementByTermIdsOf codebase traversed termIds)
      <*> (termLocalReferencesOf traversed termIds)
  where
    combine maybeTermComponentElement (Share.LocalIds {texts, hashes}) =
      maybeTermComponentElement <&> \(TermComponentElement trm typ) ->
        s2cTermWithType
          ( LocalIds.LocalIds
              { textLookup = Vector.fromList texts,
                defnLookup = Vector.fromList hashes
              },
            trm,
            typ
          )

-- | Load a batch of terms by their RefIds.
loadTermsByRefIdsOf ::
  (QueryM m, HasCallStack) =>
  CodebaseEnv ->
  Traversal s t TermReferenceId (Maybe (V2.Term Symbol, V2.Type Symbol)) ->
  s ->
  m t
loadTermsByRefIdsOf codebase trav s = do
  s & asListOfDeduped trav \termRefs -> do
    termIds <- loadTermIdsByRefIdsOf traversed termRefs
    terms <- loadTermsByIdsOf codebase (traversed . _Just) termIds
    -- Flatten the nested maybes.
    pure $ fmap join terms

expectTermsByIdsOf :: (QueryA m) => CodebaseEnv -> Traversal s t TermId (V2.Term Symbol, V2.Type Symbol) -> s -> m t
expectTermsByIdsOf codebase trav s = do
  s & asListOf trav \termIds -> do
    loadTermsByIdsOf codebase traversed termIds
      & unrecoverableEitherMap \results ->
        for (zip termIds results) \case
          (termId, Nothing) -> Left (expectedTermError (Left termId))
          (_termId, Just t) -> Right t

loadTermComponentElementByTermIdsOf ::
  (QueryA m, HasCallStack) =>
  CodebaseEnv ->
  Traversal s t TermId (Maybe TermComponentElement) ->
  s ->
  m t
loadTermComponentElementByTermIdsOf CodebaseEnv {codebaseOwner} trav s = do
  s & asListOf trav \termIds -> do
    let numberedTermIds = zip [0 :: Int32 ..] termIds
    queryListCol
      [sql|
        WITH term_ids(ord, term_id) AS (
              SELECT * FROM ^{toTable numberedTermIds}
        )
        SELECT bytes.bytes
          FROM term_ids
            LEFT JOIN sandboxed_terms sandboxed ON (sandboxed.term_id = term_ids.term_id AND sandboxed.user_id = #{codebaseOwner})
            LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
          ORDER BY term_ids.ord ASC
      |]

termLocalReferencesOf ::
  (QueryA m, HasCallStack) =>
  Traversal s t TermId (Share.LocalIds Text ComponentHash) ->
  s ->
  m t
termLocalReferencesOf trav s = do
  s & asListOf trav \termIds -> do
    zipWith Share.LocalIds
      <$> termLocalTextReferencesOf traversed termIds
      <*> termLocalComponentReferencesOf traversed termIds

termLocalTextReferencesOf :: (QueryA m, HasCallStack) => Traversal s t TermId [Text] -> s -> m t
termLocalTextReferencesOf trav s = do
  s & asListOf trav \termIds -> do
    let numberedTermIds = zip [0 :: Int32 ..] termIds
    queryListCol @[Text]
      [sql|
        WITH term_ids(ord, term_id) AS (
            SELECT * FROM ^{toTable numberedTermIds}
        )
        SELECT (
          -- Need COALESCE because array_agg will return NULL rather than the empty array
          -- if there are no results.
          SELECT COALESCE(array_agg(text.text ORDER BY text_refs.local_index ASC), '{}') as text_array
            FROM term_local_text_references text_refs
              JOIN text ON text_refs.text_id = text.id
            WHERE term_ids.term_id = text_refs.term_id
        ) AS texts
        FROM term_ids
        ORDER BY term_ids.ord ASC
      |]

termLocalComponentReferencesOf :: (QueryA m, HasCallStack) => Traversal s t TermId [ComponentHash] -> s -> m t
termLocalComponentReferencesOf trav s = do
  s & asListOf trav \termIds -> do
    let numberedTermIds = zip [0 :: Int32 ..] termIds
    queryListCol @[ComponentHash]
      [sql|
        WITH term_ids(ord, term_id) AS (
            SELECT * FROM ^{toTable numberedTermIds}
        )
        SELECT (
            SELECT COALESCE(array_agg(component_hashes.base32 ORDER BY local_refs.local_index ASC), '{}') as component_hash_array
            FROM term_local_component_references local_refs
                JOIN component_hashes ON local_refs.component_hash_id = component_hashes.id
            WHERE local_refs.term_id = term_ids.term_id
        ) AS component_hashes
        FROM term_ids
        ORDER BY term_ids.ord ASC
      |]

s2cTermWithType :: (ResolvedLocalIds, TermFormat.Term, TermFormat.Type) -> (V2.Term Symbol, V2.Type Symbol)
s2cTermWithType (localIds, tm, tp) =
  (resolveTermLocalIds localIds tm, resolveTypeLocalIds localIds tp)

resolveTermLocalIds :: ResolvedLocalIds -> TermFormat.Term -> V2.Term Symbol
resolveTermLocalIds (LocalIds.LocalIds {textLookup, defnLookup}) =
  -- substitute the text and hashes back into the term
  V2.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id
  where
    substTermRef = bimap substText (fmap substHash)
    substTypeRef = bimap substText substHash
    substTermLink = bimap substTermRef substTypeRef
    substTypeLink = substTypeRef
    substText i = textLookup ^?! ix (fromIntegral i)
    substHash i = unComponentHash $ (defnLookup ^?! ix (fromIntegral i))

-- | implementation detail of {s,w}2c*Term*
resolveTypeLocalIds :: ResolvedLocalIds -> TermFormat.Type -> V2.Type Symbol
resolveTypeLocalIds (LocalIds.LocalIds {textLookup, defnLookup}) =
  V2Type.rmap (bimap substText substHash)
  where
    substText i = textLookup ^?! ix (fromIntegral i)
    substHash i = unComponentHash $ (defnLookup ^?! ix (fromIntegral i))

-- | Resolves local ids within constructor type signatures so that it can be hashed.
resolveConstructorTypeLocalIds :: ResolvedLocalIds -> DeclFormat.Type Symbol -> V2Type.TypeD Symbol
resolveConstructorTypeLocalIds (LocalIds.LocalIds {textLookup, defnLookup}) =
  V2Type.rmap (bimap substText (fmap substHash))
  where
    substText i = textLookup ^?! ix (fromIntegral i)
    substHash i = unComponentHash $ (defnLookup ^?! ix (fromIntegral i))

loadDeclKindsOf :: (PG.QueryA m, HasCallStack) => Traversal s t TypeReferenceId (Maybe CT.ConstructorType) -> s -> m t
loadDeclKindsOf trav s =
  s
    & asListOf trav %%~ \refIds -> do
      let refTable :: [(OrdBy, Hash, PgComponentIndex)]
          refTable =
            refIds & imap \i (Reference.Id compHash (compIndex)) -> (into @OrdBy i, compHash, pgComponentIndex compIndex)
      queryListCol @(Maybe DeclKindEnum)
        [sql|
          WITH ref_ids(ord, comp_hash, comp_index) AS(
            SELECT t.ord, t.comp_hash, t.comp_index FROM ^{toTable refTable} AS t(ord, comp_hash, comp_index)
          )
          SELECT kind
            FROM ref_ids
              LEFT JOIN component_hashes ch ON ch.base32 = ref_ids.comp_hash
              LEFT JOIN types typ ON typ.component_index = ref_ids.comp_index AND typ.component_hash_id = ch.id
            ORDER BY ref_ids.ord
    |]
        <&> (fmap . fmap) declKindEnumToConstructorType

-- | This isn't in CodebaseM so that we can run it in a normal transaction to build the Code
-- Lookup.
loadDeclsByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t TypeReferenceId (Maybe (V2.Decl Symbol)) -> s -> m t
loadDeclsByRefIdsOf codebase trav s = do
  s
    & asListOf trav %%~ \refs -> do
      mayTypeComponents <- loadTypeComponentElementsAndTypeIdsOf codebase traversed refs
      typeLocalReferencesOf (traversed . _Just . _2) mayTypeComponents
        <&> (fmap . fmap) \(TypeComponentElement decl, Share.LocalIds {texts, hashes}) ->
          let localIds = LocalIds.LocalIds {textLookup = Vector.fromList texts, defnLookup = Vector.fromList hashes}
           in s2cDecl localIds decl

loadDeclByTypeComponentElementAndTypeIdsOf :: (QueryA m) => Traversal s t (TypeComponentElement, TypeId) (V2.Decl Symbol) -> s -> m t
loadDeclByTypeComponentElementAndTypeIdsOf trav s =
  s
    & asListOf trav %%~ \ids -> do
      typeLocalReferencesOf (traversed . _2) ids
        <&> fmap \(TypeComponentElement decl, Share.LocalIds {texts, hashes}) ->
          let localIds = LocalIds.LocalIds {textLookup = Vector.fromList texts, defnLookup = Vector.fromList hashes}
           in s2cDecl localIds decl

loadTypeComponentElementsAndTypeIdsOf :: (QueryA m) => CodebaseEnv -> Traversal s t TypeReferenceId (Maybe (TypeComponentElement, TypeId)) -> s -> m t
loadTypeComponentElementsAndTypeIdsOf (CodebaseEnv codebaseUser) trav s = do
  s
    & asListOf trav %%~ \refs -> do
      let refsTable =
            refs
              & ordered
              <&> \(ord, Reference.Id compHash compIndex) -> (ord, compHash, (pgComponentIndex compIndex))
      queryListRows @(Maybe TypeComponentElement, Maybe TypeId)
        [sql|
        WITH ref_ids(ord, comp_hash, comp_index) AS (
          SELECT t.ord, t.comp_hash, t.comp_index FROM ^{toTable refsTable} AS t(ord, comp_hash, comp_index)
        ) SELECT bytes.bytes, typ.id
            FROM ref_ids
              LEFT JOIN component_hashes ch ON ref_ids.comp_hash = ch.base32
              LEFT JOIN types typ ON (typ.component_index = ref_ids.comp_index AND typ.component_hash_id = ch.id)
              LEFT JOIN sandboxed_types sandboxed ON (typ.id = sandboxed.type_id AND sandboxed.user_id = #{codebaseUser})
              LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
            ORDER BY ref_ids.ord ASC
        |]
        <&> fmap \(element, typeId) -> liftA2 (,) element typeId

expectTypeComponentElementsAndTypeIdsOf :: (QueryA m) => CodebaseEnv -> Traversal s t TypeReferenceId (TypeComponentElement, TypeId) -> s -> m t
expectTypeComponentElementsAndTypeIdsOf codebase trav s =
  s
    & asListOfDeduped trav %%~ \refs -> do
      unrecoverableEitherMap
        ( \elems -> for (zip refs elems) \case
            (refId, Nothing) -> Left (expectedTypeError $ Right refId)
            (_, Just decl) -> Right decl
        )
        (loadTypeComponentElementsAndTypeIdsOf codebase traversed refs)

typeLocalReferencesOf :: (QueryA m) => Traversal s t TypeId (Share.LocalIds Text ComponentHash) -> s -> m t
typeLocalReferencesOf trav s = do
  s
    & asListOf trav %%~ \typeIds -> do
      texts <- typeLocalTextReferencesOf traversed typeIds
      components <- typeLocalComponentReferencesOf traversed typeIds
      pure $ zipWith Share.LocalIds texts components

typeLocalTextReferencesOf :: (QueryA m) => Traversal s t TypeId [Text] -> s -> m t
typeLocalTextReferencesOf trav s =
  s
    & asListOf trav %%~ \typeIds -> do
      queryListCol @[Text]
        [sql|
        WITH type_ids(ord, type_id) AS (
          SELECT t.ord, t.type_id FROM ^{toTable (ordered typeIds)} AS t(ord, type_id)
        ) SELECT (
            -- Need COALESCE because array_agg will return NULL rather than the empty array
            -- if there are no results.
            SELECT COALESCE(array_agg(text.text ORDER BY text_refs.local_index ASC), '{}') as text_array
              FROM type_local_text_references text_refs
                JOIN text ON text_refs.text_id = text.id
              WHERE type_ids.type_id = text_refs.type_id
        ) AS texts
        FROM type_ids
        ORDER BY type_ids.ord ASC
        |]

typeLocalComponentReferencesOf :: (QueryA m) => Traversal s t TypeId [ComponentHash] -> s -> m t
typeLocalComponentReferencesOf trav s = do
  s
    & asListOf trav %%~ \typeIds -> do
      queryListCol @[ComponentHash]
        [sql|
            WITH type_ids(ord, type_id) AS (
              SELECT t.ord, t.type_id FROM ^{toTable (ordered typeIds)} AS t(ord, type_id)
            ) SELECT (
              SELECT COALESCE(array_agg(component_hashes.base32 ORDER BY local_refs.local_index ASC), '{}') as component_hash_array
                FROM type_local_component_references local_refs
                  JOIN component_hashes ON local_refs.component_hash_id = component_hashes.id
                WHERE local_refs.type_id = type_ids.type_id
            ) AS component_hashes
            FROM type_ids
            ORDER BY type_ids.ord ASC
        |]

expectDeclsByRefIdsOf :: (QueryM m) => CodebaseEnv -> Traversal s t Reference.Id (V2.Decl Symbol) -> s -> m t
expectDeclsByRefIdsOf codebase trav s = do
  s
    & asListOf trav %%~ \refs -> do
      result <- loadDeclsByRefIdsOf codebase traversed refs
      for (zip refs result) \case
        (_, Just decl) -> pure decl
        (refId, Nothing) -> unrecoverableError $ InternalServerError "expected-decl" (expectedTermError $ Right refId)

s2cDecl :: ResolvedLocalIds -> DeclFormat.Decl Symbol -> (V2.Decl Symbol)
s2cDecl (LocalIds.LocalIds {textLookup, defnLookup}) V2.DataDeclaration {declType, modifier, bound, constructorTypes} =
  V2.DataDeclaration
    { declType,
      modifier,
      bound,
      constructorTypes = (V2.Type.rmap resolveLocalRef <$> constructorTypes)
    }
  where
    resolveLocalRef :: V2.Reference' LocalIds.LocalTextId (Maybe LocalIds.LocalDefnId) -> V2.Reference' Text (Maybe Hash)
    resolveLocalRef = bimap substText (fmap (unComponentHash . substHash))
    substText i = textLookup ^?! ix (fromIntegral i)
    substHash i = defnLookup ^?! ix (fromIntegral i)

-- | Get the set of user-defined terms whose hash matches the given prefix.
termReferencesByPrefix :: (QueryA m) => Text -> Maybe Word64 -> m (Set V1Reference.Id)
termReferencesByPrefix prefix mayComponentIndex =
  do
    let mayComponentIndex' = pgComponentIndex <$> mayComponentIndex
    queryListRows
      [sql| SELECT component_hashes.base32, term.component_index
        FROM component_hashes
          JOIN terms term ON component_hashes.id = term.component_hash_id
        WHERE component_hashes.base32 ^@ #{prefix}
          AND (#{mayComponentIndex'} IS NULL OR term.component_index = #{mayComponentIndex'})
        ORDER BY term.component_index ASC
    |]
    <&> fmap (\(hash, componentIndex) -> V1Reference.Id hash (unPgComponentIndex componentIndex))
    <&> Set.fromList

-- | All type references whose hash matches the given prefix and optionally the provided component index.
declReferencesByPrefix :: (QueryA m) => Text -> Maybe Word64 -> m (Set V1Reference.Id)
declReferencesByPrefix prefix mayComponentIndex = do
  let mayComponentIndex' = pgComponentIndex <$> mayComponentIndex
  queryListRows
    [sql| SELECT component_hashes.base32, typ.component_index
        FROM component_hashes
          JOIN types typ ON component_hashes.id = typ.component_hash_id
        WHERE component_hashes.base32 ^@ #{prefix}
          AND (#{mayComponentIndex'} IS NULL OR typ.component_index = #{mayComponentIndex'})
        ORDER BY typ.component_index ASC
    |]
    <&> fmap (\(hash, componentIndex) -> V2Reference.Id hash (unPgComponentIndex componentIndex))
    <&> Set.fromList

-- | All referents to type constructors whose hash matches the given prefix and optionally the provided component index and constructor index.
constructorReferentsByPrefix ::
  (QueryA m) =>
  Text ->
  Maybe V2Reference.Pos ->
  Maybe V2Decl.ConstructorId ->
  m (Set V1Referent.Referent)
constructorReferentsByPrefix prefix mayComponentIndex mayConstructorIndex = do
  let mayComponentIndex' = pgComponentIndex <$> mayComponentIndex
  let mayConstructorIndex' = pgConstructorIndex <$> mayConstructorIndex
  queryListRows @(Hash, PgComponentIndex, Text, PgConstructorIndex)
    [sql| SELECT component_hashes.base32, typ.component_index, typ.kind, constr.constructor_index
        FROM component_hashes
          JOIN types typ ON component_hashes.id = typ.component_hash_id
          JOIN constructors constr ON typ.id = constr.type_id
        WHERE component_hashes.base32 ^@ #{prefix}
          AND (#{mayComponentIndex'} IS NULL OR typ.component_index = #{mayComponentIndex'})
          AND (#{mayConstructorIndex'} IS NULL OR constr.constructor_index = #{mayConstructorIndex'})
        ORDER BY typ.component_index ASC, constr.constructor_index ASC
    |]
    <&> fmap
      ( \(hash, componentIndex, declKind, constructorIndex) ->
          let dt = case declKind of
                "data" -> CT.Data
                "ability" -> CT.Effect
                kind -> error $ "declReferentsByPrefix: Unknown decl kind: " <> Text.unpack kind
              conRef = V1Referent.ConstructorReference (V1Reference.Derived hash (unPgComponentIndex componentIndex)) (unPgConstructorIndex constructorIndex)
           in V1Referent.Con conRef dt
      )
    <&> Set.fromList

-- | Look up the result of evaluating a term if we have it cached.
--
-- This is intentionally not in CodebaseM because this method is used to build the
-- CodebaseEnv.
loadCachedEvalResult :: (QueryM m) => CodebaseEnv -> Reference.Id -> m (Maybe (V2.Term Symbol))
loadCachedEvalResult CodebaseEnv {codebaseOwner} (Reference.Id hash compIndex) = runMaybeT do
  let compIndex' = pgComponentIndex compIndex
  (evalResultId :: EvalResultId, EvalResultTerm term) <-
    MaybeT $
      query1Row
        [sql|
    SELECT result.id, bytes.bytes from eval_results result
      JOIN sandboxed_eval_result sandboxed ON result.id = sandboxed.eval_result_id
      JOIN bytes ON sandboxed.result_bytes_id = bytes.id
      JOIN component_hashes hash ON result.component_hash_id = hash.id
      WHERE hash.base32 = #{hash}
        AND result.component_index = #{compIndex'}
        AND sandboxed.user_id = #{codebaseOwner}
    |]
  textLookup <-
    Vector.fromList
      <$> queryListCol
        [sql|
      SELECT text.text
        FROM eval_result_local_text_references mapping
          JOIN text ON mapping.text_id = text.id
        WHERE mapping.eval_result_id = #{evalResultId}
          ORDER BY mapping.local_index ASC
      |]
  defnLookup <-
    Vector.fromList
      <$> queryListCol
        [sql|
      SELECT component_hashes.base32
        FROM eval_result_local_component_references mapping
          JOIN component_hashes ON mapping.component_hash_id = component_hashes.id
        WHERE mapping.eval_result_id = #{evalResultId}
          ORDER BY mapping.local_index ASC
      |]
  let localIds :: ResolvedLocalIds
      localIds = LocalIds.LocalIds {textLookup, defnLookup}
  pure $ resolveTermLocalIds localIds term

-- | Get text ids for all provided texts, inserting any that don't already exist.
ensureTextIds :: (QueryM m) => (Traversable t) => t Text -> m (t TextId)
ensureTextIds = ensureTextIdsOf traversed

-- | Efficiently saves all Text's focused by the provided traversal into the database and
-- replaces them with their corresponding Ids.
ensureTextIdsOf :: (QueryM m, HasCallStack) => Traversal s t Text TextId -> s -> m t
ensureTextIdsOf trav s = do
  s
    & asListOf trav %%~ \texts -> do
      let numberedTexts = zip [1 :: Int32 ..] texts
      results <-
        queryListCol @(TextId)
          [sql|
          WITH new_texts(ord, text, content_hash) AS (
                SELECT t.ord, t.text, text_hash(t.text) FROM ^{toTable numberedTexts} AS t(ord, text)
          ), inserted_texts(content_hash, id) AS (
            INSERT INTO text (text)
              SELECT DISTINCT text FROM new_texts
              ON CONFLICT DO NOTHING
              RETURNING content_hash, id
          )
          SELECT COALESCE(inserted_texts.id, existing_text.id)
            FROM new_texts
              LEFT JOIN text existing_text ON existing_text.content_hash = new_texts.content_hash
              LEFT JOIN inserted_texts ON inserted_texts.content_hash = new_texts.content_hash
            ORDER BY new_texts.ord ASC
          |]
      if length results /= length texts
        then error "ensureTextIdsOf: Missing expected text"
        else pure results

-- | Get text ids for all provided texts, inserting any that don't already exist.
ensureBytesIds :: (QueryM m) => (Traversable t) => t BS.ByteString -> m (t BytesId)
ensureBytesIds = ensureBytesIdsOf traversed

-- | Efficiently saves all bytestrings focused by the provided traversal into the database and
-- replaces them with their corresponding Ids.
ensureBytesIdsOf :: (QueryM m, HasCallStack) => Traversal s t BS.ByteString BytesId -> s -> m t
ensureBytesIdsOf trav s = do
  s
    & asListOf trav %%~ \bytestrings -> do
      let numberedBytestrings = zip [1 :: Int32 ..] (RawBytes <$> bytestrings)
      results <-
        queryListCol @(BytesId)
          [sql|
          WITH new_bytestrings(ord, bytes, content_hash) AS (
                SELECT t.ord, t.bytes, bytes_hash(t.bytes) FROM ^{toTable numberedBytestrings} AS t(ord, bytes)
          ), inserted_bytestrings(content_hash, id) AS (
            INSERT INTO bytes (bytes)
              SELECT DISTINCT bytes FROM new_bytestrings
              ON CONFLICT DO NOTHING
              RETURNING content_hash, id
          )
          SELECT COALESCE(inserted_bytestrings.id, existing_bytestrings.id)
            FROM new_bytestrings
              LEFT JOIN bytes existing_bytestrings ON existing_bytestrings.content_hash = new_bytestrings.content_hash
              LEFT JOIN inserted_bytestrings ON inserted_bytestrings.content_hash = new_bytestrings.content_hash
            ORDER BY new_bytestrings.ord ASC
          |]
      if length results /= length bytestrings
        then error "ensureTextIdsOf: Missing expected bytestring"
        else pure results

-- | Efficiently loads Texts for all TextIds focused by the provided traversal.
expectTextsOf :: (QueryM m, HasCallStack) => Traversal s t TextId Text -> s -> m t
expectTextsOf trav =
  asListOf trav %%~ \textIds -> do
    let numberedTextIds = zip [0 :: Int32 ..] textIds
    results :: [Text] <-
      queryListCol
        [sql|
          WITH new_texts(ord, text_id) AS (
                SELECT * FROM ^{toTable numberedTextIds}
          )
          SELECT text.text
            FROM new_texts
              JOIN text ON text.id = new_texts.text_id
            ORDER BY new_texts.ord ASC
          |]
    if length results /= length textIds
      then error "expectTextsOf: Missing expected text"
      else pure results

-- | Replace all references in a term with local references.
localizeTerm :: (QueryM m) => V2.Term Symbol -> m (PgLocalIds, TermFormat.Term)
localizeTerm tm = do
  let (localIds, tm', _typ) = runIdentity $ Util.c2xTerm Identity (Identity . ComponentHash) tm Nothing
  (localIds' :: PgLocalIds) <- HashQ.ensureComponentHashIdsOf LocalIds.h_ localIds >>= ensureTextIdsOf LocalIds.t_
  pure (localIds', tm')

-- | Replace all references in a term with local references.
_localizeTermAndType ::
  (HasCallStack, QueryM m) =>
  V2.Term Symbol ->
  V2.Type Symbol ->
  m (PgLocalIds, TermFormat.Term, TermFormat.Type)
_localizeTermAndType tm typ = do
  let (localIds, tm', typ') = case runIdentity (Util.c2xTerm Identity (Identity . ComponentHash) tm (Just typ)) of
        (_, _, Nothing) -> error "localizeTermAndType: Type is Nothing"
        (localIds, tm', Just t) -> (localIds, tm', t)
  localIds' :: PgLocalIds <- HashQ.ensureComponentHashIdsOf LocalIds.h_ localIds >>= ensureTextIdsOf LocalIds.t_
  pure (localIds', tm', typ')

-- | Save the result of an evaluation to the database.
-- Note: this is sandboxed by user because the result may be a function containing local
-- variable names.
saveCachedEvalResult :: forall m. (QueryM m) => CodebaseEnv -> Reference.Id -> V2.Term Symbol -> m ()
saveCachedEvalResult (CodebaseEnv {codebaseOwner}) (Reference.Id resultHash compI) term = do
  ensureEvalResult >>= \case
    (True, _) -> pure () -- Already saved.
    (False, evalResultId) -> doSave evalResultId
  where
    doSave :: EvalResultId -> m ()
    doSave evalResultId = do
      (LocalIds.LocalIds {textLookup, defnLookup}, dbTerm) <- localizeTerm term
      resultBytesId <- ensureBytesIdsOf id (evalResultTermToByteString $ EvalResultTerm dbTerm)
      execute_
        [sql|
          INSERT INTO sandboxed_eval_result (user_id, eval_result_id, result_bytes_id)
            VALUES (#{codebaseOwner}, #{evalResultId}, #{resultBytesId})
        |]

      let textLocals = zip [0 :: Int32 ..] (Vector.toList textLookup)
      whenNonEmpty textLocals $
        execute_
          [sql|
          WITH values(local_index, text_id) AS (
            SELECT * FROM ^{toTable textLocals}
          )
          INSERT INTO eval_result_local_text_references(eval_result_id, local_index, text_id)
            SELECT #{evalResultId}, values.local_index, values.text_id
              FROM values
        |]
      let compHashLocals = zip [0 :: Int32 ..] (Vector.toList defnLookup)
      whenNonEmpty compHashLocals $
        execute_
          [sql|
          WITH values(local_index, hash_id) AS (
            SELECT * FROM ^{toTable compHashLocals}
          )
          INSERT INTO eval_result_local_component_references(eval_result_id, local_index, component_hash_id)
            SELECT #{evalResultId}, values.local_index, values.hash_id
              FROM values
        |]
    -- Ensure there's a row for this eval result, returning whether it already exists.
    ensureEvalResult :: m (Bool, EvalResultId)
    ensureEvalResult = do
      resultHashId <- HashQ.ensureComponentHashIdsOf id (ComponentHash resultHash)
      let compIndex = pgComponentIndex compI
      queryExpect1Row @(Bool, EvalResultId)
        [sql|
        WITH values(component_hash_id, component_index) AS (
          SELECT * FROM (VALUES (#{resultHashId}, #{compIndex})) AS t(component_hash_id, component_index)
        ), inserted(component_hash_id, component_index, id) AS (
          INSERT INTO eval_results (component_hash_id, component_index)
            VALUES (#{resultHashId}, #{compIndex})
            ON CONFLICT DO NOTHING
            RETURNING component_hash_id, component_index, id
        ) SELECT result.id IS NOT NULL, COALESCE(inserted.id, result.id)
            FROM values val
              LEFT JOIN eval_results result
                ON val.component_hash_id = result.component_hash_id AND val.component_index = result.component_index
              LEFT JOIN inserted
                ON inserted.component_hash_id = val.component_hash_id AND inserted.component_index = val.component_index
      |]

-- | Encode and save a term component to the database. See 'saveEncodedTermComponent' for a more
-- efficient version if you've already got an encoded term component available.
saveTermComponent :: (QueryM m) => CodebaseEnv -> ComponentHash -> [(PgLocalIds, TermFormat.Term, TermFormat.Type)] -> m ()
saveTermComponent codebase componentHash elements = do
  let encodedElements =
        elements <&> \(localIds, trm, typ) ->
          (localIds, TermComponentElementBytes $ termComponentElementToByteString (TermComponentElement trm typ), typ)
  saveEncodedTermComponent codebase componentHash Nothing encodedElements

-- | Save an already-encoded term component to the database. This is more efficient than
-- 'saveTermComponent' in cases where you've already got a serialized term (like during sync).
saveEncodedTermComponent :: forall m. (QueryM m) => CodebaseEnv -> ComponentHash -> Maybe TempEntity -> [(PgLocalIds, TermComponentElementBytes, TermFormat.Type)] -> m ()
saveEncodedTermComponent codebase@(CodebaseEnv {codebaseOwner}) componentHash maySerialized elements = do
  componentHashId <- HashQ.ensureComponentHashIdsOf id componentHash
  let elementsTable = elements & imap \i _ -> pgComponentIndex $ fromIntegral @Int i
  mayTermIds :: Maybe (NE.NonEmpty TermId) <-
    queryListCol
      [sql|
      WITH elements(component_index) AS (
        SELECT * FROM ^{singleColumnTable elementsTable}
      )
      SELECT term.id FROM elements
        -- Left join so we can verify we get a match for every element.
        LEFT JOIN terms term ON elements.component_index = term.component_index
        WHERE term.component_hash_id = #{componentHashId}
      ORDER BY elements.component_index
    |]
      -- We expect to either have ALL the elements or NONE of them.
      <&> \listOfMayTermIds -> sequenceA listOfMayTermIds >>= NE.nonEmpty

  termIds <- mayTermIds `whenNothing` saveSharedTermAndLocalMappings componentHashId
  let sandboxedTermsTable =
        zip (toList termIds) elements
          & fmap
            ( \(termId, (_localIds, elementBytes, _typ)) ->
                ( termId,
                  elementBytes
                )
            )
  whenNonEmpty sandboxedTermsTable $ do
    sandboxedTermBytesTable <- ensureBytesIdsOf (traversed . traversed) (sandboxedTermsTable <&> second termComponentElementBytes)
    execute_
      [sql|
      WITH elements(term_id, bytes_id) AS (
        SELECT * FROM ^{toTable sandboxedTermBytesTable}
      )
      INSERT INTO sandboxed_terms (user_id, term_id, bytes_id)
        SELECT #{codebaseOwner}, element.term_id, element.bytes_id
          FROM elements element
    |]
  doSaveSerialized componentHashId
  where
    doSaveSerialized chId = do
      componentEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> do
          SyncCommon.entityToTempEntity id . Share.TC <$> expectShareTermComponent codebase chId
      let serializedEntity = SyncV2.serialiseCBORBytes componentEntity
      saveSerializedComponent codebase chId serializedEntity

    saveSharedTermAndLocalMappings :: ComponentHashId -> m (NE.NonEmpty TermId)
    saveSharedTermAndLocalMappings componentHashId = do
      let HashHandle {toReference = hashType} = v2HashHandle
      elementsWithResolvedLocals <- resolveLocalIdsOf (traversed . _1) elements
      -- We need to save the shared terms for the first time.
      let termsTable :: [(PgComponentIndex, Maybe Text, Maybe ComponentHash, Maybe PgComponentIndex)]
          termsTable =
            elementsWithResolvedLocals
              & imap
                ( \componentIndex (localIds, _elementBytes, typeFormat) ->
                    let v2Type = resolveTypeLocalIds localIds typeFormat
                        (typ_builtin, typ_component_hash, typ_component_index) = case hashType v2Type of
                          V2Reference.Derived hash componentIndex -> (Nothing, Just hash, Just $ pgComponentIndex componentIndex)
                          V2.ReferenceBuiltin builtin -> (Just builtin, Nothing, Nothing)
                     in (pgComponentIndex $ fromIntegral @Int componentIndex, typ_builtin, ComponentHash <$> typ_component_hash, typ_component_index)
                )
      -- Save the type hashes.
      elementsWithIds <-
        HashQ.ensureComponentHashIdsOf (traversed . _3 . _Just) termsTable
          >>= ensureTextIdsOf (traversed . _2 . _Just)
      termIds :: (NE.NonEmpty TermId) <-
        queryListCol
          [sql|
          WITH elements(component_index, term_type_builtin, term_type_component_hash_id, term_type_component_index) AS (
            SELECT * FROM ^{toTable elementsWithIds}
          )
          INSERT INTO terms (component_hash_id, component_index, term_type_builtin, term_type_component_hash_id, term_type_component_index)
            SELECT #{componentHashId}, elements.component_index, elements.term_type_builtin, elements.term_type_component_hash_id, elements.term_type_component_index
              FROM elements
            RETURNING terms.id
        |]
          <&> fromMaybe (error "Failed to insert terms") . NE.nonEmpty

      let textLookupTable =
            zip (toList termIds) elements
              & foldMap
                ( \(termId, (LocalIds.LocalIds {textLookup}, _trm, _typ)) ->
                    Vector.toList textLookup
                      & imap (\localIndex textId -> (termId, fromIntegral @Int @Int32 localIndex, textId))
                )

      whenNonEmpty textLookupTable $
        execute_
          [sql|
          WITH text_mappings(term_id, local_index, text_id) AS (
            SELECT * FROM ^{toTable textLookupTable}
          )
          INSERT INTO term_local_text_references(term_id, local_index, text_id)
            SELECT text_mappings.term_id, text_mappings.local_index, text_mappings.text_id
              FROM text_mappings
        |]
      let defnLookupTable =
            zip (toList termIds) elements
              & foldMap
                ( \(termId, (LocalIds.LocalIds {defnLookup}, _trm, _typ)) ->
                    Vector.toList defnLookup
                      & imap (\localIndex defnId -> (termId, fromIntegral @Int @Int32 localIndex, defnId))
                )

      whenNonEmpty defnLookupTable $
        execute_
          [sql|
          WITH defn_mappings(term_id, local_index, component_hash_id) AS (
            SELECT * FROM ^{toTable defnLookupTable}
          )
          INSERT INTO term_local_component_references(term_id, local_index, component_hash_id)
            SELECT defn_mappings.term_id, defn_mappings.local_index, defn_mappings.component_hash_id
              FROM defn_mappings
        |]
      execute_
        [sql|
          SELECT update_component_depth(#{componentHashId})
        |]
      pure termIds

saveTypeComponent :: forall m. (QueryM m) => CodebaseEnv -> ComponentHash -> Maybe TempEntity -> [(PgLocalIds, DeclFormat.Decl Symbol)] -> m ()
saveTypeComponent (codebase@CodebaseEnv {codebaseOwner}) componentHash maySerialized elements = do
  componentHashId <- HashQ.ensureComponentHashIdsOf id componentHash
  let elementsTable = elements & imap \i _ -> fromIntegral @Int @Int32 i
  mayTypeIds :: Maybe (NE.NonEmpty TypeId) <-
    queryListCol
      [sql|
      WITH elements(component_index) AS (
        SELECT * FROM ^{singleColumnTable elementsTable}
      )
      SELECT type.id FROM elements
        -- Left join so we can verify we get a match for every element we expect.
        LEFT JOIN types type ON elements.component_index = type.component_index
        WHERE type.component_hash_id = #{componentHashId}
      ORDER BY elements.component_index
    |]
      -- We expect to either have ALL the elements or NONE of them.
      <&> \listOfMayTypeIds -> sequenceA listOfMayTypeIds >>= NE.nonEmpty
  typeIds <- mayTypeIds `whenNothing` saveSharedTypeAndLocalMappings componentHashId
  let sandboxedTypesTable =
        zip (toList typeIds) elements
          <&> ( \(typeId, (_localIds, decl)) ->
                  ( typeId,
                    TypeComponentElement decl
                  )
              )

  whenNonEmpty sandboxedTypesTable $ do
    sandboxedTypesBytesTable <- ensureBytesIdsOf (traversed . traversed) (sandboxedTypesTable <&> second typeComponentElementToByteString)
    execute_
      [sql|
      WITH elements(type_id, bytes_id) AS (
        SELECT * FROM ^{toTable sandboxedTypesBytesTable}
      )
      INSERT INTO sandboxed_types (user_id, type_id, bytes_id)
        SELECT #{codebaseOwner}, element.type_id, element.bytes_id
          FROM elements element
    |]
  doSaveSerialized componentHashId
  where
    doSaveSerialized chId = do
      componentEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> do
          SyncCommon.entityToTempEntity id . Share.DC <$> expectShareTypeComponentsOf codebase id chId
      let serializedEntity = SyncV2.serialiseCBORBytes componentEntity
      saveSerializedComponent codebase chId serializedEntity

    saveConstructors :: [(TypeId, (PgLocalIds, DeclFormat.Decl Symbol))] -> m ()
    saveConstructors typeElements = do
      typeElementsWithResolvedLocals <- resolveLocalIdsOf (traversed . _2 . _1) typeElements
      let HashHandle {toReferenceDecl = hashConstructorType} = v2HashHandle
      let constructorsTable :: [(TypeId, PgConstructorIndex, Maybe Text {- con type builtin -}, Maybe ComponentHash {- con type component hash -}, Maybe PgComponentIndex {- con type component index -})]
          constructorsTable =
            typeElementsWithResolvedLocals
              & foldMap \(typeId, (resolvedLocalIds, Decl.DataDeclaration {constructorTypes})) ->
                constructorTypes
                  & imap
                    ( \constructorIndex constructorTyp ->
                        let (typeBuiltin, typeComponentHash, typeComponentIndex) = case hashConstructorType (unComponentHash componentHash) (resolveConstructorTypeLocalIds resolvedLocalIds constructorTyp) of
                              V2Reference.Derived hash componentIndex -> (Nothing, Just hash, Just $ pgComponentIndex componentIndex)
                              V2.ReferenceBuiltin builtin -> (Just builtin, Nothing, Nothing)
                         in ( typeId,
                              pgConstructorIndex $ fromIntegral @Int constructorIndex,
                              typeBuiltin,
                              ComponentHash <$> typeComponentHash,
                              typeComponentIndex
                            )
                    )
      constructorsTableWithIds <-
        ( HashQ.ensureComponentHashIdsOf (traversed . _4 . _Just) constructorsTable
            >>= ensureTextIdsOf (traversed . _3 . _Just)
          )

      whenNonEmpty constructorsTableWithIds $
        execute_
          [sql|
          WITH elements(type_id, constructor_index, constructor_type_builtin, constructor_type_component_hash_id, constructor_type_component_index) AS (
            SELECT * FROM ^{toTable constructorsTableWithIds}
          )
          INSERT INTO constructors (type_id, constructor_index, constructor_type_builtin, constructor_type_component_hash_id, constructor_type_component_index)
            SELECT elements.type_id, elements.constructor_index, elements.constructor_type_builtin, elements.constructor_type_component_hash_id, elements.constructor_type_component_index
              FROM elements
        |]

    saveSharedTypeAndLocalMappings :: ComponentHashId -> m (NE.NonEmpty TypeId)
    saveSharedTypeAndLocalMappings componentHashId = do
      -- We need to save the shared types for the first time.
      let typesTable :: [(PgComponentIndex, DeclKindEnum, ModifierEnum)]
          typesTable =
            elements
              & imap
                ( \componentIndex (_localIds, Decl.DataDeclaration {declType, modifier}) ->
                    let modifierEnum = case modifier of
                          Decl.Structural {} -> Structural
                          Decl.Unique {} -> Unique
                     in (pgComponentIndex $ fromIntegral @Int componentIndex, declTypeToDeclKindEnum declType, modifierEnum)
                )
      typeIds :: (NE.NonEmpty TypeId) <-
        queryListCol
          [sql|
          WITH elements(component_index, kind, modifier) AS (
            SELECT * FROM ^{toTable typesTable}
          )
          INSERT INTO types (component_hash_id, component_index, kind, modifier)
            SELECT #{componentHashId}, elements.component_index, elements.kind::decl_kind, elements.modifier::modifier_kind
              FROM elements
            RETURNING types.id
        |]
          <&> fromMaybe (error "Failed to insert types") . NE.nonEmpty

      let textLookupTable =
            zip (toList typeIds) elements
              & foldMap
                ( \(typeId, (LocalIds.LocalIds {textLookup}, _decl)) ->
                    Vector.toList textLookup
                      & imap (\localIndex textId -> (typeId, fromIntegral @Int @Int32 localIndex, textId))
                )

      whenNonEmpty textLookupTable $
        execute_
          [sql|
          WITH text_mappings(type_id, local_index, text_id) AS (
            SELECT * FROM ^{toTable textLookupTable}
          )
          INSERT INTO type_local_text_references(type_id, local_index, text_id)
            SELECT text_mappings.type_id, text_mappings.local_index, text_mappings.text_id
              FROM text_mappings
        |]
      let defnLookupTable =
            zip (toList typeIds) elements
              & foldMap
                ( \(typeId, (LocalIds.LocalIds {defnLookup}, _decl)) ->
                    Vector.toList defnLookup
                      & imap (\localIndex defnId -> (typeId, fromIntegral @Int @Int32 localIndex, defnId))
                )
      whenNonEmpty defnLookupTable $
        execute_
          [sql|
          WITH defn_mappings(type_id, local_index, component_hash_id) AS (
            SELECT * FROM ^{toTable defnLookupTable}
          )
          INSERT INTO type_local_component_references(type_id, local_index, component_hash_id)
            SELECT defn_mappings.type_id, defn_mappings.local_index, defn_mappings.component_hash_id
              FROM defn_mappings
        |]
      saveConstructors (zip (toList typeIds) elements)
      execute_
        [sql|
          SELECT update_component_depth(#{componentHashId})
        |]
      pure typeIds

-- | Efficiently resolve all pg Ids across selected Local Ids.
resolveLocalIdsOf :: (QueryM m, HasCallStack) => Traversal s t PgLocalIds ResolvedLocalIds -> s -> m t
resolveLocalIdsOf trav s = do
  s
    & asListOf trav %%~ \pgLocalIds -> do
      expectTextsOf (traversed . LocalIds.t_) pgLocalIds
        >>= HashQ.expectComponentHashesOf (traversed . LocalIds.h_)

-- | Fetch term tags for all the provided Referents.
termTagsByReferentsOf :: (HasCallStack) => Traversal s t Referent.Referent Tags.TermTag -> s -> Transaction e t
termTagsByReferentsOf trav s = do
  s
    & asListOf trav %%~ \refs -> do
      let partitionedRefs :: [Either Tags.TermTag (ComponentHash, PgComponentIndex, Maybe PgConstructorIndex)]
          partitionedRefs =
            refs <&> \case
              -- If it's a derived term we'll need to look up its type to know the tag.
              Referent.Ref (Reference.Derived hash compInd) -> Right (ComponentHash hash, pgComponentIndex compInd, Nothing)
              -- If it's a builtin term, we can just look up the type and tag directly.
              Referent.Ref r@(Reference.Builtin {}) -> Left $ termTagForBuiltin r
              -- If it's a constructor, the tag is just the tag of the type.
              Referent.Con (Reference.Derived hash compInd) conId -> Right (ComponentHash hash, pgComponentIndex compInd, Just $ pgConstructorIndex conId)
              Referent.Con (Reference.Builtin t) conId -> error $ "Encountered a constructor for a builtin type, which shouldn't be possible: " <> show (t, conId)
      -- We only need to look up the tags for the derived references from the DB
      partitionedRefs
        & asListOf (traversed . _Right)
          %%~ ( \refs -> do
                  let refsTable = refs & imap \i (hash, compInd, conInd) -> (into @OrdBy i, hash, compInd, conInd)
                  results <-
                    queryListCol
                      @Text
                      [sql|
              WITH known_type_hashes(component_hash_id, component_index, tag) AS (
                SELECT component_hashes.id, t.component_index, t.tag FROM ^{toTable knownTypeHashesTable} AS t(component_hash, component_index, tag)
                  JOIN component_hashes ON t.component_hash = component_hashes.base32
              ), refs_table(ord, component_hash_id, component_index, constructor_index) AS (
                SELECT t.ord, component_hashes.id, t.component_index, t.constructor_index
                  FROM ^{toTable refsTable} AS t(ord, component_hash, component_index, constructor_index)
                  JOIN component_hashes ON t.component_hash = component_hashes.base32
              ), constructor_tags(ord, tag) AS (
                -- If we have a special-cased known tag, use that first, then if it's a constructor, build the tag from the kind
                  SELECT refs_table.ord, COALESCE(known_tag_constructor.tag :: text, constructor_type.kind :: text) AS tag
                    FROM refs_table
                      JOIN types constructor_type ON (refs_table.constructor_index IS NOT NULL AND refs_table.component_hash_id = constructor_type.component_hash_id AND refs_table.component_index = constructor_type.component_index)
                      LEFT JOIN known_type_hashes known_tag_constructor
                                  ON  known_tag_constructor.component_hash_id = constructor_type.component_hash_id
                                  AND known_tag_constructor.component_index = constructor_type.component_index
              ), term_tags(ord, tag) AS (
                -- If we have a special-cased known tag, use that first, otherwise it's just a 'plain' term.
                  SELECT refs_table.ord, COALESCE(known_tag_term.tag :: text, 'plain' :: text) AS tag
                    FROM refs_table
                      JOIN terms term ON (refs_table.constructor_index IS NULL AND refs_table.component_hash_id = term.component_hash_id AND refs_table.component_index = term.component_index)
                      LEFT JOIN known_type_hashes known_tag_term
                                  ON  known_tag_term.component_hash_id = term.term_type_component_hash_id
                                  AND known_tag_term.component_index = term.term_type_component_index
              )
                 SELECT tag FROM (
                    SELECT ord, tag FROM constructor_tags
                    UNION ALL
                    SELECT ord, tag FROM term_tags
                 ) AS tags
                    ORDER BY tags.ord ASC
            |]
                  if length results /= length refsTable
                    then error "termTagsByReferentsOf: Missing expected term tag"
                    else pure (results <&> tagFromText)
              )
        <&> fmap unifyEither
  where
    termTagForBuiltin :: Reference -> Tags.TermTag
    termTagForBuiltin _ =
      -- For now all builtins are plain terms.
      Tags.Plain
    refTagRow :: Tags.TermTag -> Reference -> (ComponentHash, PgComponentIndex, Text)
    refTagRow tag = \case
      Reference.Derived hash compInd -> (ComponentHash hash, pgComponentIndex compInd, tagToText tag)
      Reference.Builtin t -> error $ "termTagsByReferentsOf: Unexpected builtin reference: " <> show t
    tagToText :: Tags.TermTag -> Text
    tagToText = \case
      Tags.Plain -> "plain"
      Tags.Doc -> "doc"
      Tags.Test -> "test"
      Tags.Constructor Tags.Ability -> "ability"
      Tags.Constructor Tags.Data -> "data"
    tagFromText :: Text -> Tags.TermTag
    tagFromText = \case
      "plain" -> Tags.Plain
      "doc" -> Tags.Doc
      "test" -> Tags.Test
      "ability" -> Tags.Constructor Tags.Ability
      "data" -> Tags.Constructor Tags.Data
      t -> error $ "termTagsByReferentsOf: Unknown tag: " <> show t
    knownTypeHashesTable :: [(ComponentHash, PgComponentIndex, Text)]
    knownTypeHashesTable =
      [ (refTagRow Tags.Doc Decls.docRef),
        (refTagRow Tags.Doc Decls.doc2Ref),
        (refTagRow Tags.Test Decls.testResultListRef)
      ]

typeTagsByReferencesOf :: (HasCallStack) => Traversal s t TypeReference Tags.TypeTag -> s -> Transaction e t
typeTagsByReferencesOf trav s = do
  s
    & asListOf trav %%~ \refs -> do
      let partitionedRefs :: [Either Tags.TypeTag (ComponentHash, PgComponentIndex)]
          partitionedRefs =
            refs <&> \case
              Reference.Derived hash compInd -> Right (ComponentHash hash, pgComponentIndex compInd)
              r@(Reference.Builtin _) -> Left $ typeTagForBuiltin r
      -- We only need to look up the tags for the derived references from the DB
      partitionedRefs
        & asListOf (traversed . _Right)
          %%~ ( \refs -> do
                  let refsTable = refs & imap \i (hash, compInd) -> (into @OrdBy i, hash, compInd)
                  results <-
                    queryListCol
                      @DeclKindEnum
                      [sql|
              WITH refs_table(ord, component_hash_id, component_index) AS (
                SELECT t.ord, component_hashes.id, t.component_index
                  FROM ^{toTable refsTable} AS t(ord, component_hash, component_index)
                  JOIN component_hashes ON t.component_hash = component_hashes.base32
              ) SELECT type.kind
                  FROM refs_table
                    JOIN types type ON refs_table.component_hash_id = type.component_hash_id AND refs_table.component_index = type.component_index
                    ORDER BY refs_table.ord ASC
            |]
                  if length results /= length refsTable
                    then error "typeTagsByReferencesOf: Missing expected type tag"
                    else pure (results <&> tagFromDeclKind)
              )
        <&> fmap unifyEither
  where
    typeTagForBuiltin :: Reference -> Tags.TypeTag
    typeTagForBuiltin _ =
      -- For now all builtin types are data types.
      Tags.Data
    tagFromDeclKind :: DeclKindEnum -> Tags.TypeTag
    tagFromDeclKind = \case
      DefnTypes.Ability -> Tags.Ability
      DefnTypes.Data -> Tags.Data

saveSerializedComponent :: (QueryM m) => CodebaseEnv -> ComponentHashId -> CBORBytes TempEntity -> m ()
saveSerializedComponent (CodebaseEnv {codebaseOwner}) chId (CBORBytes bytes) = do
  bytesId <- ensureBytesIdsOf id (BL.toStrict bytes)
  execute_
    [sql|
      INSERT INTO serialized_components (user_id, component_hash_id, bytes_id)
        VALUES (#{codebaseOwner}, #{chId}, #{bytesId})
      ON CONFLICT DO NOTHING
    |]

termTransitiveDependencies :: (QueryM m) => Set TermId -> m (Set.Set TermId, Set.Set TypeId)
termTransitiveDependencies termIds = do
  queryExpect1Row @([TermId], [TypeId])
    [sql|
WITH
input_terms(term_id) AS (
  #^{singleColumnTable (toList termIds)} AS t(term_id)
),
-- Get the component hash IDs for the input terms
base_term_components(component_hash_id) AS (
    SELECT DISTINCT term.component_hash_id
    FROM input_terms it
        JOIN terms term ON it.term_id = term.id
),
-- Recursively find all transitive component dependencies
transitive_components(component_hash_id) AS (
    -- Base case: start with the components of our input terms
    SELECT DISTINCT btc.component_hash_id
    FROM base_term_components btc
    UNION
    -- Recursive case: find dependencies of current components
    ( WITH rec AS (
        SELECT component_hash_id
        FROM transitive_components tc
    )
        -- Get term dependencies from current components
        SELECT DISTINCT ref.component_hash_id
        FROM rec atc
            -- Get all terms from the component
            JOIN terms term ON atc.component_hash_id = term.component_hash_id
            -- Get their component references
            JOIN term_local_component_references ref ON term.id = ref.term_id
        UNION
        -- Get type dependencies from current components
        SELECT DISTINCT ref.component_hash_id
        FROM rec atc
            -- Get all types from the component
            JOIN types typ ON atc.component_hash_id = typ.component_hash_id
            -- Get their component references
            JOIN type_local_component_references ref ON typ.id = ref.type_id
    )
)
-- Final result: single row with arrays of term_ids and type_ids
SELECT
    ARRAY(
        SELECT term.id
        FROM transitive_components tc
        JOIN terms term ON tc.component_hash_id = term.component_hash_id
    ) AS term_ids,
    ARRAY(
        SELECT type.id
        FROM transitive_components tc
        JOIN types type ON tc.component_hash_id = type.component_hash_id
    ) AS type_ids;
    |]
    <&> bimap Set.fromList Set.fromList

expectedTermError :: Either TermId TermReferenceId -> InternalServerError DefinitionQueryError
expectedTermError refId =
  InternalServerError "expected-term" (ExpectedTermNotFound refId)

expectedTypeError :: Either TypeId TypeReferenceId -> InternalServerError DefinitionQueryError
expectedTypeError refId =
  InternalServerError "expected-type" (ExpectedTypeNotFound refId)

missingDeclKindError :: TypeReference -> InternalServerError Text
missingDeclKindError r =
  InternalServerError "missing-decl-kind" $ "Couldn't find the decl kind of " <> tShow r
