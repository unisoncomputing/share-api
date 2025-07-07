{-# LANGUAGE DataKinds #-}

module Share.Postgres.Definitions.Queries
  ( loadTerm,
    expectTerm,
    expectTermId,
    expectTermById,
    saveTermComponent,
    saveEncodedTermComponent,
    loadTermComponent,
    expectTermComponent,
    termTagsByReferentsOf,
    typeTagsByReferencesOf,
    loadTypeComponent,
    expectTypeComponent,
    expectShareTermComponent,
    expectShareTypeComponent,
    loadDeclKind,
    loadDeclKindsOf,
    loadDecl,
    expectDecl,
    loadDeclByTypeComponentElementAndTypeId,
    expectTypeComponentElementAndTypeId,
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
import Share.Codebase.Types (CodebaseEnv (..), CodebaseM)
import Share.Codebase.Types qualified as Codebase
import Share.IDs
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Types
import Share.Postgres.Definitions.Types qualified as DefnTypes
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.Postgres (OrdBy, RawBytes (..))
import Share.Web.Errors (ErrorID (..), InternalServerError (InternalServerError), ToServerError (..))
import U.Codebase.Decl qualified as Decl
import U.Codebase.Decl qualified as V2 hiding (Type)
import U.Codebase.Decl qualified as V2Decl
import U.Codebase.Reference qualified as Reference
import U.Codebase.Reference qualified as V2
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as Referent
import U.Codebase.Referent qualified as V2
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

type ComponentRef = These ComponentHash ComponentHashId

data DefinitionQueryError
  = ExpectedTermNotFound TermReferenceId
  | ExpectedTermComponentNotFound ComponentRef
  | ExpectedTypeNotFound TypeReferenceId
  | ExpectedTypeComponentNotFound ComponentRef
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

-- | This isn't in CodebaseM so that we can run it in a normal transaction to build the Code
-- Lookup.
loadTerm :: UserId -> TermReferenceId -> PG.Transaction e (Maybe (V2.Term Symbol, V2.Type Symbol))
loadTerm userId refId = runMaybeT do
  termId <- MaybeT $ loadTermId refId
  MaybeT $ loadTermById userId termId

loadTermId :: (QueryA m) => TermReferenceId -> m (Maybe TermId)
loadTermId (Reference.Id compHash (pgComponentIndex -> compIndex)) =
  query1Col
    [sql|
      SELECT term.id
        FROM terms term
          JOIN component_hashes ON term.component_hash_id = component_hashes.id
          WHERE component_hashes.base32 = #{compHash}
            AND term.component_index = #{compIndex}
      |]

expectTermId :: (QueryA m) => TermReferenceId -> m TermId
expectTermId refId =
  unrecoverableEitherMap
    ( \case
        Nothing -> Left (expectedTermError refId)
        Just termId -> Right termId
    )
    (loadTermId refId)

expectTerm :: UserId -> Reference.Id -> PG.Transaction e (V2.Term Symbol, V2.Type Symbol)
expectTerm userId refId = do
  mayTerm <- loadTerm userId refId
  case mayTerm of
    Just term -> pure term
    Nothing -> unrecoverableError $ expectedTermError refId

resolveComponentHash :: ComponentRef -> CodebaseM e (ComponentHash, ComponentHashId)
resolveComponentHash = \case
  This componentHash -> do
    componentHashId <- HashQ.ensureComponentHashId componentHash
    pure (componentHash, componentHashId)
  That componentHashId -> do
    componentHash <- HashQ.expectComponentHash componentHashId
    pure (componentHash, componentHashId)
  These componentHash componentHashId -> do
    pure (componentHash, componentHashId)

loadTermComponent :: ComponentRef -> CodebaseM e (Maybe (NonEmpty (V2.Term Symbol, V2.Type Symbol)))
loadTermComponent componentRef = runMaybeT $ do
  codebaseOwnerUserId <- asks codebaseOwner
  (componentHash, componentHashId) <- lift $ resolveComponentHash componentRef
  componentIndexes <- MaybeT . lift $ termComponentIndexes componentHashId
  componentElements <- for componentIndexes \compIndex -> do
    MaybeT . lift $ loadTerm codebaseOwnerUserId (Reference.Id (unComponentHash componentHash) compIndex)
  pure componentElements

loadTypeComponent :: ComponentRef -> CodebaseM e (Maybe (NonEmpty (V2Decl.Decl Symbol)))
loadTypeComponent componentRef = runMaybeT $ do
  codebaseOwnerUserId <- asks codebaseOwner
  (componentHash, componentHashId) <- lift $ resolveComponentHash componentRef
  componentIndexes <- MaybeT . lift $ typeComponentIndexes componentHashId
  componentElements <- for componentIndexes \compIndex -> do
    MaybeT . lift $ loadDecl codebaseOwnerUserId (Reference.Id (unComponentHash componentHash) compIndex)
  pure componentElements

termComponentIndexes :: ComponentHashId -> Transaction e (Maybe (NonEmpty V2Reference.Pos))
termComponentIndexes componentHashId = do
  queryListCol
    [sql|
    SELECT component_index from terms term
      WHERE term.component_hash_id = #{componentHashId}
    ORDER BY component_index ASC
   |]
    <&> fmap unPgComponentIndex
    <&> NonEmpty.nonEmpty

typeComponentIndexes :: ComponentHashId -> Transaction e (Maybe (NonEmpty V2Reference.Pos))
typeComponentIndexes componentHashId = do
  queryListCol
    [sql|
    SELECT component_index from types typ
      WHERE typ.component_hash_id = #{componentHashId}
    ORDER BY component_index ASC
   |]
    <&> fmap unPgComponentIndex
    <&> NonEmpty.nonEmpty

expectTermComponent ::
  These ComponentHash ComponentHashId ->
  CodebaseM e (NonEmpty (V2.Term Symbol, V2.Type Symbol))
expectTermComponent componentRef = do
  mayComponent <- loadTermComponent componentRef
  case mayComponent of
    Just component -> pure component
    Nothing -> lift . unrecoverableError $ InternalServerError "expected-term-component" (ExpectedTermComponentNotFound componentRef)

-- | Helper for loading term components efficiently for sync.
expectShareTermComponent :: ComponentHashId -> CodebaseM e (Share.TermComponent Text Hash32)
expectShareTermComponent componentHashId = do
  codebaseOwnerUserId <- asks codebaseOwner
  componentElements :: NonEmpty (TermId, LocalTermBytes) <-
    ( queryListRows
        [sql| SELECT term.id, bytes.bytes
           FROM terms term
           LEFT JOIN sandboxed_terms sandboxed ON term.id = sandboxed.term_id
           LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
           WHERE term.component_hash_id = #{componentHashId}
             AND sandboxed.user_id = #{codebaseOwnerUserId}
           ORDER BY term.component_index ASC
      |]
        -- Ensure we get at least one index, and that we have bytes saved for each part of the
        -- component.
        <&> checkElements
      )
      `whenNothingM` do
        lift . unrecoverableError $ InternalServerError "expected-term-component" (ExpectedTermComponentNotFound (That componentHashId))
  second (Hash32.fromHash . unComponentHash) . Share.TermComponent . toList <$> for componentElements \(termId, LocalTermBytes bytes) ->
    (,bytes) <$> termLocalReferences termId
  where
    checkElements :: [(TermId, Maybe LocalTermBytes)] -> Maybe (NonEmpty (TermId, LocalTermBytes))
    checkElements rows =
      sequenceAOf (traversed . _2) rows
        >>= NonEmpty.nonEmpty

-- | Helper for loading type components efficiently for sync.
expectShareTypeComponent :: ComponentHashId -> CodebaseM e (Share.DeclComponent Text Hash32)
expectShareTypeComponent componentHashId = do
  codebaseOwnerUserId <- asks codebaseOwner
  componentElements :: NonEmpty (TypeId, LocalTypeBytes) <-
    ( queryListRows
        [sql| SELECT typ.id, bytes.bytes
           FROM types typ
           LEFT JOIN sandboxed_types sandboxed ON typ.id = sandboxed.type_id
           LEFT JOIN bytes ON sandboxed.bytes_id = bytes.id
           WHERE typ.component_hash_id = #{componentHashId}
             AND sandboxed.user_id = #{codebaseOwnerUserId}
           ORDER BY typ.component_index ASC
      |]
        -- Ensure we get at least one index, and that we have bytes saved for each part of the
        -- component.
        <&> checkElements
      )
      `whenNothingM` do
        lift . unrecoverableError $ InternalServerError "expected-type-component" (ExpectedTypeComponentNotFound (That componentHashId))
  second (Hash32.fromHash . unComponentHash) . Share.DeclComponent . toList <$> for componentElements \(typeId, LocalTypeBytes bytes) ->
    (,bytes) <$> typeLocalReferences typeId
  where
    checkElements :: [(TypeId, Maybe LocalTypeBytes)] -> Maybe (NonEmpty (TypeId, LocalTypeBytes))
    checkElements rows =
      sequenceAOf (traversed . _2) rows
        >>= NonEmpty.nonEmpty

expectTypeComponent :: ComponentRef -> CodebaseM e (NonEmpty (V2Decl.Decl Symbol))
expectTypeComponent componentRef = do
  mayComponent <- loadTypeComponent componentRef
  case mayComponent of
    Just component -> pure component
    Nothing -> lift . unrecoverableError $ InternalServerError "expected-type-component" (ExpectedTypeComponentNotFound componentRef)

-- | This isn't in CodebaseM so that we can run it in a normal transaction to build the Code
-- Lookup.
loadTermById :: (QueryA m) => UserId -> TermId -> m (Maybe (V2.Term Symbol, V2.Type Symbol))
loadTermById codebaseUser termId = do
  ( \maybeTermComponentElement Share.LocalIds {texts, hashes} ->
      maybeTermComponentElement <&> \(TermComponentElement trm typ) ->
        s2cTermWithType
          ( LocalIds.LocalIds
              { textLookup = Vector.fromList texts,
                defnLookup = Vector.fromList hashes
              },
            trm,
            typ
          )
    )
    <$> loadTermComponentElementByTermId codebaseUser termId
    <*> termLocalReferences termId

expectTermById :: (QueryA m) => UserId -> TermReferenceId -> TermId -> m (V2.Term Symbol, V2.Type Symbol)
expectTermById userId refId termId =
  unrecoverableEitherMap
    ( \case
        Nothing -> Left (expectedTermError refId)
        Just term -> Right term
    )
    (loadTermById userId termId)

loadTermComponentElementByTermId :: (QueryA m) => UserId -> TermId -> m (Maybe TermComponentElement)
loadTermComponentElementByTermId codebaseUser termId =
  query1Col
    [sql|
        SELECT bytes.bytes
          FROM sandboxed_terms sandboxed
            JOIN bytes ON sandboxed.bytes_id = bytes.id
            WHERE sandboxed.user_id = #{codebaseUser}
              AND sandboxed.term_id = #{termId}
      |]

termLocalReferences :: (QueryA m) => TermId -> m (Share.LocalIds Text ComponentHash)
termLocalReferences termId =
  Share.LocalIds
    <$> termLocalTextReferences termId
    <*> termLocalComponentReferences termId

termLocalTextReferences :: (QueryA m) => TermId -> m [Text]
termLocalTextReferences termId =
  queryListCol
    [sql|
      SELECT text
        FROM term_local_text_references
          JOIN text ON term_local_text_references.text_id = text.id
        WHERE term_id = #{termId}
          ORDER BY local_index ASC
      |]

termLocalComponentReferences :: (QueryA m) => TermId -> m [ComponentHash]
termLocalComponentReferences termId =
  queryListCol
    [sql|
      SELECT component_hashes.base32
        FROM term_local_component_references
          JOIN component_hashes ON term_local_component_references.component_hash_id = component_hashes.id
        WHERE term_id = #{termId}
          ORDER BY local_index ASC
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

loadDeclKind :: (PG.QueryA m) => TypeReferenceId -> m (Maybe CT.ConstructorType)
loadDeclKind = loadDeclKindsOf id

loadDeclKindsOf :: (PG.QueryA m) => Traversal s t TypeReferenceId (Maybe CT.ConstructorType) -> s -> m t
loadDeclKindsOf trav s =
  s
    & unsafePartsOf trav %%~ \refIds -> do
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
              LEFT JOIN types typ ON typ.component_index = ref_ids.comp_index
              LEFT JOIN component_hashes ON typ.component_hash_id = component_hashes.id
              WHERE component_hashes.base32 = ref_ids.comp_hash
            ORDER BY ref_ids.ord
    |]
        <&> (fmap . fmap) declKindEnumToConstructorType

-- | This isn't in CodebaseM so that we can run it in a normal transaction to build the Code
-- Lookup.
loadDecl :: (QueryM m) => UserId -> TypeReferenceId -> m (Maybe (V2.Decl Symbol))
loadDecl codebaseUser refId = runMaybeT $ do
  (TypeComponentElement decl, typeId :: TypeId) <- MaybeT (loadTypeComponentElementAndTypeId codebaseUser refId)
  Share.LocalIds {texts, hashes} <- typeLocalReferences typeId
  let localIds = LocalIds.LocalIds {textLookup = Vector.fromList texts, defnLookup = Vector.fromList hashes}
  pure $ s2cDecl localIds decl

loadDeclByTypeComponentElementAndTypeId :: (QueryA m) => (TypeComponentElement, TypeId) -> m (V2.Decl Symbol)
loadDeclByTypeComponentElementAndTypeId (TypeComponentElement decl, typeId) =
  typeLocalReferences typeId <&> \Share.LocalIds {texts, hashes} ->
    let localIds = LocalIds.LocalIds {textLookup = Vector.fromList texts, defnLookup = Vector.fromList hashes}
     in s2cDecl localIds decl

loadTypeComponentElementAndTypeId :: (QueryA m) => UserId -> TypeReferenceId -> m (Maybe (TypeComponentElement, TypeId))
loadTypeComponentElementAndTypeId codebaseUser (Reference.Id compHash (pgComponentIndex -> compIndex)) = do
  query1Row
    [sql|
      SELECT bytes.bytes, typ.id
        FROM types typ
          JOIN component_hashes ON typ.component_hash_id = component_hashes.id
          JOIN sandboxed_types sandboxed ON typ.id = sandboxed.type_id
          JOIN bytes ON sandboxed.bytes_id = bytes.id
          WHERE sandboxed.user_id = #{codebaseUser}
            AND component_hashes.base32 = #{compHash}
            AND typ.component_index = #{compIndex}
      |]

expectTypeComponentElementAndTypeId :: (QueryA m) => UserId -> TermReferenceId -> m (TypeComponentElement, TypeId)
expectTypeComponentElementAndTypeId codebaseUser refId =
  unrecoverableEitherMap
    ( \case
        Nothing -> Left (expectedTypeError refId)
        Just decl -> Right decl
    )
    (loadTypeComponentElementAndTypeId codebaseUser refId)

typeLocalReferences :: (QueryA m) => TypeId -> m (Share.LocalIds Text ComponentHash)
typeLocalReferences typeId =
  Share.LocalIds
    <$> typeLocalTextReferences typeId
    <*> typeLocalComponentReferences typeId

typeLocalTextReferences :: (QueryA m) => TypeId -> m [Text]
typeLocalTextReferences typeId =
  queryListCol
    [sql|
      SELECT text.text
        FROM type_local_text_references
          JOIN text ON type_local_text_references.text_id = text.id
        WHERE type_id = #{typeId}
          ORDER BY local_index ASC
      |]

typeLocalComponentReferences :: (QueryA m) => TypeId -> m [ComponentHash]
typeLocalComponentReferences typeId =
  queryListCol
    [sql|
      SELECT component_hashes.base32
        FROM type_local_component_references
          JOIN component_hashes ON type_local_component_references.component_hash_id = component_hashes.id
        WHERE type_id = #{typeId}
          ORDER BY local_index ASC
      |]

expectDecl :: UserId -> Reference.Id -> PG.Transaction e (V2.Decl Symbol)
expectDecl codebaseUser refId = do
  mayDecl <- loadDecl codebaseUser refId
  case mayDecl of
    Just decl -> pure decl
    Nothing -> unrecoverableError $ InternalServerError "expected-decl" (ExpectedTermNotFound refId)

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
termReferencesByPrefix :: Text -> Maybe Word64 -> Transaction e (Set V1Reference.Id)
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
declReferencesByPrefix :: Text -> Maybe Word64 -> Transaction e (Set V1Reference.Id)
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
  Text ->
  Maybe V2Reference.Pos ->
  Maybe V2Decl.ConstructorId ->
  Transaction e (Set V1Referent.Referent)
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
loadCachedEvalResult :: (QueryM m) => UserId -> Reference.Id -> m (Maybe (V2.Term Symbol))
loadCachedEvalResult codebaseOwnerUserId (Reference.Id hash compIndex) = runMaybeT do
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
        AND sandboxed.user_id = #{codebaseOwnerUserId}
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
ensureTextIdsOf :: (QueryM m) => Traversal s t Text TextId -> s -> m t
ensureTextIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \texts -> do
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
ensureBytesIdsOf :: (QueryM m) => Traversal s t BS.ByteString BytesId -> s -> m t
ensureBytesIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \bytestrings -> do
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
expectTextsOf :: (QueryM m) => Traversal s t TextId Text -> s -> m t
expectTextsOf trav =
  unsafePartsOf trav %%~ \textIds -> do
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
localizeTerm :: V2.Term Symbol -> Transaction e (PgLocalIds, TermFormat.Term)
localizeTerm tm = do
  let (localIds, tm', _typ) = runIdentity $ Util.c2xTerm Identity (Identity . ComponentHash) tm Nothing
  (localIds' :: PgLocalIds) <- HashQ.ensureComponentHashIdsOf LocalIds.h_ localIds >>= ensureTextIdsOf LocalIds.t_
  pure (localIds', tm')

-- | Replace all references in a term with local references.
_localizeTermAndType ::
  (HasCallStack) =>
  V2.Term Symbol ->
  V2.Type Symbol ->
  Transaction e (PgLocalIds, TermFormat.Term, TermFormat.Type)
_localizeTermAndType tm typ = do
  let (localIds, tm', typ') = case runIdentity (Util.c2xTerm Identity (Identity . ComponentHash) tm (Just typ)) of
        (_, _, Nothing) -> error "localizeTermAndType: Type is Nothing"
        (localIds, tm', Just t) -> (localIds, tm', t)
  localIds' :: PgLocalIds <- HashQ.ensureComponentHashIdsOf LocalIds.h_ localIds >>= ensureTextIdsOf LocalIds.t_
  pure (localIds', tm', typ')

-- | Save the result of an evaluation to the database.
-- Note: this is sandboxed by user because the result may be a function containing local
-- variable names.
saveCachedEvalResult :: Reference.Id -> V2.Term Symbol -> CodebaseM e ()
saveCachedEvalResult (Reference.Id resultHash compI) term = do
  ensureEvalResult >>= \case
    (True, _) -> pure () -- Already saved.
    (False, evalResultId) -> doSave evalResultId
  where
    doSave :: EvalResultId -> CodebaseM e ()
    doSave evalResultId = do
      codebaseOwnerUserId <- asks codebaseOwner
      (LocalIds.LocalIds {textLookup, defnLookup}, dbTerm) <- lift $ localizeTerm term
      resultBytesId <- ensureBytesIdsOf id (evalResultTermToByteString $ EvalResultTerm dbTerm)
      execute_
        [sql|
          INSERT INTO sandboxed_eval_result (user_id, eval_result_id, result_bytes_id)
            VALUES (#{codebaseOwnerUserId}, #{evalResultId}, #{resultBytesId})
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
    ensureEvalResult :: CodebaseM e (Bool, EvalResultId)
    ensureEvalResult = do
      resultHashId <- HashQ.ensureComponentHashId (ComponentHash resultHash)
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
saveTermComponent :: ComponentHash -> [(PgLocalIds, TermFormat.Term, TermFormat.Type)] -> CodebaseM e ()
saveTermComponent componentHash elements = do
  let encodedElements =
        elements <&> \(localIds, trm, typ) ->
          (localIds, TermComponentElementBytes $ termComponentElementToByteString (TermComponentElement trm typ), typ)
  saveEncodedTermComponent componentHash Nothing encodedElements

-- | Save an already-encoded term component to the database. This is more efficient than
-- 'saveTermComponent' in cases where you've already got a serialized term (like during sync).
saveEncodedTermComponent :: ComponentHash -> Maybe TempEntity -> [(PgLocalIds, TermComponentElementBytes, TermFormat.Type)] -> CodebaseM e ()
saveEncodedTermComponent componentHash maySerialized elements = do
  codebaseOwnerUserId <- asks codebaseOwner
  componentHashId <- HashQ.ensureComponentHashId componentHash
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
        SELECT #{codebaseOwnerUserId}, element.term_id, element.bytes_id
          FROM elements element
    |]
  doSaveSerialized componentHashId
  where
    doSaveSerialized chId = do
      componentEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> do
          SyncCommon.entityToTempEntity id . Share.TC <$> expectShareTermComponent chId
      let serializedEntity = SyncV2.serialiseCBORBytes componentEntity
      saveSerializedComponent chId serializedEntity

    saveSharedTermAndLocalMappings :: ComponentHashId -> CodebaseM e (NE.NonEmpty TermId)
    saveSharedTermAndLocalMappings componentHashId = do
      let HashHandle {toReference = hashType} = v2HashHandle
      elementsWithResolvedLocals <- lift $ resolveLocalIdsOf (traversed . _1) elements
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

saveTypeComponent :: ComponentHash -> Maybe TempEntity -> [(PgLocalIds, DeclFormat.Decl Symbol)] -> CodebaseM e ()
saveTypeComponent componentHash maySerialized elements = do
  codebaseOwnerUserId <- asks codebaseOwner
  componentHashId <- HashQ.ensureComponentHashId componentHash
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
        SELECT #{codebaseOwnerUserId}, element.type_id, element.bytes_id
          FROM elements element
    |]
  doSaveSerialized componentHashId
  where
    doSaveSerialized chId = do
      componentEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> do
          SyncCommon.entityToTempEntity id . Share.DC <$> expectShareTypeComponent chId
      let serializedEntity = SyncV2.serialiseCBORBytes componentEntity
      saveSerializedComponent chId serializedEntity

    saveConstructors :: [(TypeId, (PgLocalIds, DeclFormat.Decl Symbol))] -> CodebaseM e ()
    saveConstructors typeElements = do
      typeElementsWithResolvedLocals <- lift $ resolveLocalIdsOf (traversed . _2 . _1) typeElements
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

    saveSharedTypeAndLocalMappings :: ComponentHashId -> CodebaseM e (NE.NonEmpty TypeId)
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
resolveLocalIdsOf :: Traversal s t PgLocalIds ResolvedLocalIds -> s -> Transaction e t
resolveLocalIdsOf trav s = do
  s
    & unsafePartsOf trav %%~ \pgLocalIds -> do
      expectTextsOf (traversed . LocalIds.t_) pgLocalIds
        >>= HashQ.expectComponentHashesOf (traversed . LocalIds.h_)

-- | Fetch term tags for all the provided Referents.
termTagsByReferentsOf :: (HasCallStack) => Traversal s t Referent.Referent Tags.TermTag -> s -> Transaction e t
termTagsByReferentsOf trav s = do
  s
    & unsafePartsOf trav %%~ \refs -> do
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
        & unsafePartsOf (traversed . _Right)
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
              ) -- If we have a special-cased known tag, use that first, then if it's a constructor, build the tag from the kind, otherwise it's just a 'plain' term.
                SELECT COALESCE(known_tag.tag :: text, constructor_type.kind :: text, 'plain' :: text)
                  FROM refs_table
                    -- Each ref should match either a term or a type.
                    -- First we try to find a matching term
                    LEFT JOIN terms term ON (refs_table.constructor_index IS NULL AND refs_table.component_hash_id = term.component_hash_id AND refs_table.component_index = term.component_index)
                    -- Now we get data about its type if it was a constructor instead.
                    -- We don't actually need to care which constructor it is, we just need
                    -- the kind of the type.
                    LEFT JOIN types constructor_type ON (refs_table.constructor_index IS NOT NULL AND refs_table.component_hash_id = constructor_type.component_hash_id AND refs_table.component_index = constructor_type.component_index)
                    -- We special case specific types like docs and tests.
                    -- If we have a match in the known component hashes, we can use that tag.
                    LEFT JOIN known_type_hashes known_tag
                                ON  known_tag.component_hash_id = COALESCE(term.term_type_component_hash_id, constructor_type.component_hash_id)
                                    AND known_tag.component_index = COALESCE(term.term_type_component_index, constructor_type.component_index)
                    ORDER BY refs_table.ord ASC
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
    & unsafePartsOf trav %%~ \refs -> do
      let partitionedRefs :: [Either Tags.TypeTag (ComponentHash, PgComponentIndex)]
          partitionedRefs =
            refs <&> \case
              Reference.Derived hash compInd -> Right (ComponentHash hash, pgComponentIndex compInd)
              r@(Reference.Builtin _) -> Left $ typeTagForBuiltin r
      -- We only need to look up the tags for the derived references from the DB
      partitionedRefs
        & unsafePartsOf (traversed . _Right)
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

saveSerializedComponent :: ComponentHashId -> CBORBytes TempEntity -> CodebaseM e ()
saveSerializedComponent chId (CBORBytes bytes) = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  bytesId <- ensureBytesIdsOf id (BL.toStrict bytes)
  execute_
    [sql|
      INSERT INTO serialized_components (user_id, component_hash_id, bytes_id)
        VALUES (#{codebaseOwnerUserId}, #{chId}, #{bytesId})
      ON CONFLICT DO NOTHING
    |]

expectedTermError :: TermReferenceId -> InternalServerError DefinitionQueryError
expectedTermError refId =
  InternalServerError "expected-term" (ExpectedTermNotFound refId)

expectedTypeError :: TypeReferenceId -> InternalServerError DefinitionQueryError
expectedTypeError refId =
  InternalServerError "expected-type" (ExpectedTypeNotFound refId)

missingDeclKindError :: TypeReference -> InternalServerError Text
missingDeclKindError r =
  InternalServerError "missing-decl-kind" $ "Couldn't find the decl kind of " <> tShow r

listTermDependencies :: TermId -> CodebaseM e (Set V2.Reference)
listTermDependencies termId = do
  queryListRows @V2.Referent
    [sql|
      WITH components(component_hash_id) AS (
        SELECT local.component_hash_id
          FROM term_local_component_references local
          WHERE local.term_id = #{termId}
      ) SELECT * from terms t
          JOIN components c ON t.component_hash_id = c.component_hash_id
    |]
    <&> Set.fromList
