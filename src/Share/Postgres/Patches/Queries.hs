module Share.Postgres.Patches.Queries
  ( expectPatch,
    savePatch,

    -- * Sync
    expectPatchEntity,

    -- * Exported for migrations
    saveSerializedPatch,
  )
where

import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Postgres
import Share.Postgres.Definitions.Queries qualified as Defn
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Postgres.Definitions.Types (PgComponentIndex, pgComponentIndex, unPgComponentIndex)
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.Patches.Types
import Share.Postgres.Serialization qualified as S
import Share.Postgres.Sync.Conversions qualified as Cv
import Share.Prelude
import U.Codebase.Branch qualified as V2
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.LocalizeObject qualified as Localize
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull
import U.Codebase.Sqlite.Patch.TermEdit qualified as PatchFullTermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as PatchFullTypeEdit
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.TermEdit qualified as TermEdit
import U.Codebase.TypeEdit qualified as TypeEdit
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H
import Unison.Reference qualified as Reference
import Unison.Sync.Common qualified as SyncCommon
import Unison.Sync.Types qualified as Sync
import Unison.SyncV2.Types (CBORBytes (..))
import Unison.SyncV2.Types qualified as SyncV2
import Unison.Util.Map qualified as Map

loadPatch :: forall m. (QueryM m) => PatchId -> m (Maybe V2.Patch)
loadPatch patchId = runMaybeT do
  termMappings <- lift $ getTermMappings patchId
  constructorMappings <- lift $ getConstructorMappings patchId
  typeMappings <- lift $ getTypeMappings patchId
  pure $
    V2.Patch
      { termEdits = Map.unionWith (Set.union) termMappings constructorMappings,
        typeEdits = typeMappings
      }
  where
    getTermMappings :: PatchId -> m (Map Referent.Referent (Set TermEdit.TermEdit))
    getTermMappings patchId = do
      queryListRows
        [sql|
        SELECT from_term_hash.base32, mapping.from_term_component_index, from_builtin.text, to_term_hash.base32, to_term.component_index, to_builtin.text, mapping.typing, mapping.deprecated
        FROM patch_term_mappings mapping
          LEFT JOIN component_hashes from_term_hash
            ON from_term_hash.id = mapping.from_term_component_hash_id
          LEFT JOIN text from_builtin
            ON from_builtin.id = mapping.from_term_builtin_id
          LEFT JOIN terms to_term
            ON to_term.id = mapping.to_term_id
          LEFT JOIN component_hashes to_term_hash
            ON to_term_hash.id = to_term.component_hash_id
          LEFT JOIN text to_builtin
            ON to_builtin.id = mapping.to_term_builtin_id
        WHERE patch_id = #{patchId}
      |]
        <&> ( fmap \(mayFromComponentHash, mayFromComponentIndex :: Maybe Int64, mayFromBuiltin, mayToComponentHash, mayToComponentIndex :: Maybe Int64, mayToBuiltin, mayTyping, deprecated) ->
                let fromRef = case (mayFromComponentHash, mayFromComponentIndex, mayFromBuiltin) of
                      (Just fromComponentHash, Just fromComponentIndex, Nothing) -> Referent.Ref (Reference.Derived fromComponentHash (fromIntegral @Int64 @Reference.Pos fromComponentIndex))
                      (Nothing, Nothing, Just fromBuiltin) -> Referent.Ref (Reference.Builtin fromBuiltin)
                      _ -> error "Invalid term mapping"
                 in case deprecated of
                      True -> Map.singleton fromRef . Set.singleton $ TermEdit.Deprecate
                      False -> case (mayToComponentHash, mayToComponentIndex, mayToBuiltin, mayTyping) of
                        (Just toCompHash, Just toCompIndex, Nothing, Just typing) ->
                          Map.singleton fromRef $ Set.singleton $ TermEdit.Replace (Referent.Ref (Reference.Derived toCompHash (fromIntegral @Int64 @Reference.Pos toCompIndex))) typing
                        (Nothing, Nothing, Just toBuiltin, Just typing) ->
                          Map.singleton fromRef $ Set.singleton $ TermEdit.Replace (Referent.Ref (Reference.Builtin toBuiltin)) typing
                        _ -> error "Invalid term mapping"
            )
        <&> Map.unionsWith (Set.union)
    getConstructorMappings :: PatchId -> m (Map Referent.Referent (Set TermEdit.TermEdit))
    getConstructorMappings patchId = do
      queryListRows
        [sql|
        SELECT from_constructor_type_hash.base32, mapping.from_constructor_component_index, mapping.from_constructor_constructor_index, to_constructor_type_hash.base32, to_constructor_type.component_index, to_constructor.constructor_index, mapping.typing, mapping.deprecated
        FROM patch_constructor_mappings mapping
          JOIN component_hashes from_constructor_type_hash
            ON from_constructor_type_hash.id = mapping.from_constructor_component_hash_id
          LEFT JOIN constructors to_constructor
            ON to_constructor.id = mapping.to_constructor_id
          LEFT JOIN types to_constructor_type
            ON to_constructor_type.id = to_constructor.type_id
          LEFT JOIN component_hashes to_constructor_type_hash
            ON to_constructor_type_hash.id = to_constructor_type.component_hash_id
        WHERE patch_id = #{patchId}
        |]
        <&> ( fmap \(fromComponentHash, fromComponentIndex :: Int64, fromConstructorIndex :: Int64, mayToComponentHash, mayToComponentIndex :: Maybe Int64, mayToConstructorIndex :: Maybe Int64, mayTyping, deprecated) ->
                let fromRef = Referent.Con (Reference.Derived fromComponentHash (fromIntegral @Int64 @Reference.Pos fromComponentIndex)) (fromIntegral @Int64 @Reference.Pos fromConstructorIndex)
                 in case (mayToComponentHash, mayToComponentIndex, mayToConstructorIndex, mayTyping, deprecated) of
                      (Just toCompHash, Just toCompIndex, Just toConstructorIndex, Just typing, False) ->
                        Map.singleton fromRef $ Set.singleton $ TermEdit.Replace (Referent.Con (Reference.Derived toCompHash (fromIntegral @Int64 @Reference.Pos toCompIndex)) (fromIntegral @Int64 @Reference.Pos toConstructorIndex)) typing
                      (Nothing, Nothing, Nothing, Nothing, True) ->
                        Map.singleton fromRef $ Set.singleton $ TermEdit.Deprecate
                      _ -> error "Invalid constructor mapping"
            )
        <&> Map.unionsWith (Set.union)

    getTypeMappings :: PatchId -> m (Map Reference.Reference (Set TypeEdit.TypeEdit))
    getTypeMappings patchId =
      do
        queryListRows @(Maybe ComponentHash, Maybe PgComponentIndex, Maybe Text, Maybe ComponentHash, Maybe PgComponentIndex, Maybe Text, Bool)
          [sql|
        SELECT from_type_hash.base32, mapping.from_type_component_index, from_builtin.text, to_type_hash.base32, to_type.component_index, to_builtin.text, mapping.deprecated
          FROM patch_type_mappings mapping
            JOIN component_hashes from_type_hash ON from_type_hash.id = mapping.from_type_component_hash_id
            LEFT JOIN types to_type ON to_type.id = mapping.to_type_id
            LEFT JOIN component_hashes to_type_hash ON to_type_hash.id = to_type.component_hash_id
            LEFT JOIN text from_builtin ON from_builtin.id = mapping.from_type_builtin_id
            LEFT JOIN text to_builtin ON to_builtin.id = mapping.to_type_builtin_id
          WHERE patch_id = #{patchId}
        |]
        <&> ( fmap \(mayFromComponentHash, mayFromComponentIndex, mayFromBuiltin, mayToComponentHash, mayToComponentIndex, mayToBuiltin, deprecated) ->
                let fromRef = case (mayFromComponentHash, mayFromComponentIndex, mayFromBuiltin) of
                      (Just (ComponentHash fromComponentHash), Just fromComponentIndex, Nothing) -> Reference.Derived fromComponentHash (unPgComponentIndex fromComponentIndex)
                      (Nothing, Nothing, Just fromBuiltin) -> Reference.Builtin fromBuiltin
                      _ -> error "Invalid type mapping"
                 in case deprecated of
                      True -> Map.singleton fromRef . Set.singleton $ TypeEdit.Deprecate
                      False -> case (mayToComponentHash, mayToComponentIndex, mayToBuiltin) of
                        (Just (ComponentHash toCompHash), Just toCompIndex, Nothing) ->
                          Map.singleton fromRef $ Set.singleton $ TypeEdit.Replace (Reference.Derived toCompHash (unPgComponentIndex toCompIndex))
                        (Nothing, Nothing, Just toBuiltin) ->
                          Map.singleton fromRef $ Set.singleton $ TypeEdit.Replace (Reference.Builtin toBuiltin)
                        _ -> error "Invalid type mapping"
            )
        <&> Map.unionsWith (Set.union)

expectPatch :: (HasCallStack, QueryM m) => PatchId -> m V2.Patch
expectPatch patchId = do
  mayPatch <- loadPatch patchId
  case mayPatch of
    Nothing -> error "expectPatch: Patch not found"
    Just patch -> pure patch

savePatch :: (QueryM m) => CodebaseEnv -> Maybe TempEntity -> PatchHash -> PgPatch -> m PatchId
savePatch (CodebaseEnv {codebaseOwner}) maySerialized patchHash PatchFull.Patch {termEdits, typeEdits} = do
  patchId <-
    query1Col [sql| SELECT id FROM patches WHERE hash = #{patchHash} |] >>= \case
      Just patchId -> pure patchId
      Nothing -> do
        patchId <- doSave patchHash
        doSaveSerialized patchId
        pure patchId
  -- add to patch ownership
  execute_
    [sql|
    INSERT INTO patch_ownership(patch_id, user_id)
      VALUES (#{patchId}, #{codebaseOwner})
      ON CONFLICT DO NOTHING
    |]
  pure patchId
  where
    doSaveSerialized patchId = do
      patchEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> do
          SyncCommon.entityToTempEntity id . Sync.P <$> expectPatchEntity patchId
      let serializedPatch = SyncV2.serialiseCBORBytes patchEntity
      saveSerializedPatch patchId serializedPatch

    doSave patchHash = do
      patchId <-
        queryExpect1Col
          [sql|
          INSERT INTO patches(hash)
            VALUES (#{patchHash})
            RETURNING id
        |]
      whenNonEmpty termsTable $
        execute_
          [sql|
        WITH values(from_term_component_hash_id, from_term_component_index, from_term_builtin_id, to_term_component_hash_id, to_term_component_index, to_term_builtin_id, typing, deprecated) AS (
          SELECT * FROM ^{toTable termsTable}
        )
        INSERT INTO patch_term_mappings(patch_id, from_term_builtin_id, from_term_component_hash_id, from_term_component_index, to_term_id, to_term_builtin_id, typing, deprecated)
          SELECT #{patchId}, from_term_builtin_id, values.from_term_component_hash_id, values.from_term_component_index, to_term.id, to_term_builtin_id, typing::patch_term_typing, deprecated
          FROM values
            LEFT JOIN terms to_term
              ON to_term.component_hash_id = to_term_component_hash_id AND to_term.component_index = to_term_component_index
        |]
      whenNonEmpty constructorsTable $
        execute_
          [sql|
          WITH values(from_constructor_component_hash_id, from_constructor_component_index, from_constructor_constructor_index, to_constructor_type_component_hash_id, to_constructor_type_component_index, to_constructor_constructor_index, typing, deprecated) AS (
            SELECT * FROM ^{toTable constructorsTable}
          )
          INSERT INTO patch_constructor_mappings(patch_id, from_constructor_component_hash_id, from_constructor_component_index, from_constructor_constructor_index, to_constructor_id, typing, deprecated)
            SELECT #{patchId}, from_constructor_type.component_hash_id, from_constructor_type.component_index, from_constructor.constructor_index, to_constructor.id, typing::patch_term_typing, deprecated
            FROM values
              JOIN types from_constructor_type
                ON from_constructor_type.component_hash_id = from_constructor_component_hash_id AND from_constructor_type.component_index = from_constructor_component_index
              JOIN constructors from_constructor
                ON from_constructor.type_id = from_constructor_type.id AND from_constructor.constructor_index = from_constructor_constructor_index
              JOIN types to_constructor_type
                ON to_constructor_type.component_hash_id = to_constructor_type_component_hash_id AND to_constructor_type.component_index = to_constructor_type_component_index
              JOIN constructors to_constructor
                ON to_constructor.type_id = to_constructor_type.id AND to_constructor.constructor_index = to_constructor_constructor_index
        |]

      whenNonEmpty typesTable $
        execute_
          [sql|
          WITH values(from_type_component_hash_id, from_type_component_index, from_type_builtin_id, to_type_component_hash_id, to_type_component_index, to_type_builtin_id, deprecated) AS (
            SELECT * FROM ^{toTable typesTable}
          )
          INSERT INTO patch_type_mappings(patch_id, from_type_component_hash_id, from_type_component_index, from_type_builtin_id, to_type_id, to_type_builtin_id, deprecated)
            SELECT #{patchId}, from_type_component_hash_id, from_type_component_index, from_type_builtin_id, to_type.id, to_type_builtin_id, deprecated
            FROM values
              LEFT JOIN types to_type
                ON to_type.component_hash_id = to_type_component_hash_id AND to_type.component_index = to_type_component_index
          |]
      execute_ [sql| SELECT update_patch_depth(#{patchId}) |]
      pure patchId
    termsTable :: [(Maybe ComponentHashId, Maybe Int64 {- from comp index -}, Maybe TextId, Maybe ComponentHashId, Maybe Int64 {- to comp index -}, Maybe TextId, Maybe PatchFullTermEdit.Typing, Bool)]
    constructorsTable :: [(ComponentHashId, Int64 {- from comp index -}, Int64 {- from constr index -}, Maybe ComponentHashId, Maybe Int64 {- to comp index-}, Maybe Int64 {- to constr index -}, Maybe PatchFullTermEdit.Typing, Bool)]
    (termsTable, constructorsTable) =
      termEdits
        & Map.toList
        & foldMap \(referent, termEdits) ->
          let fromRef = case referent of
                (Referent.Ref termRef) -> case termRef of
                  (Reference.Derived compHash compIndex) -> Left (Just compHash, Just $ fromIntegral @Reference.Pos @Int64 compIndex, Nothing)
                  (Reference.Builtin builtin) -> Left (Nothing, Nothing, Just builtin)
                (Referent.Con typeRef conId) ->
                  case typeRef of
                    (Reference.Derived compHash compIndex) -> Right (compHash, fromIntegral @Reference.Pos @Int64 compIndex, fromIntegral @Reference.Pos @Int64 conId)
                    _ -> error "Invalid constructor reference"
           in termEdits & foldMap \te ->
                let (toCompHash, toCompIndex, toCompBuiltin, toConstrIndex, typing, deprecated) = case te of
                      PatchFullTermEdit.Deprecate -> (Nothing, Nothing, Nothing, Nothing, Nothing, True)
                      PatchFullTermEdit.Replace (Referent.Con typeRef toConId) typing ->
                        case typeRef of
                          (Reference.Derived compHash compIndex) -> (Just compHash, Just $ fromIntegral @Reference.Pos @Int64 compIndex, Nothing, Just $ fromIntegral @Reference.Pos @Int64 toConId, Just typing, False)
                          _ -> error "Invalid constructor reference"
                      PatchFullTermEdit.Replace (Referent.Ref termRef) typing ->
                        case termRef of
                          (Reference.Derived compHash compIndex) -> (Just compHash, Just $ fromIntegral @Reference.Pos @Int64 compIndex, Nothing, Nothing, Just typing, False)
                          (Reference.Builtin builtin) -> (Nothing, Nothing, Just builtin, Nothing, Just typing, False)
                 in case fromRef of
                      -- term mappings, no constructors
                      Left (mayFromCompHash, mayFromCompIndex, mayFromBuiltin) ->
                        ( [(mayFromCompHash, mayFromCompIndex, mayFromBuiltin, toCompHash, toCompIndex, toCompBuiltin, typing, deprecated)],
                          mempty
                        )
                      -- constructor mappings, no builtins
                      Right (fromCompHash, fromCompIndex, fromConstrIndex) ->
                        ( mempty,
                          [(fromCompHash, fromCompIndex, fromConstrIndex, toCompHash, toCompIndex, toConstrIndex, typing, deprecated)]
                        )
    typesTable :: [(Maybe ComponentHashId, Maybe PgComponentIndex, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex, Maybe TextId, Bool)]
    typesTable =
      typeEdits
        & Map.toList
        & foldMap \(ref, typeEdits) ->
          let (fromCompHash, fromCompIndex, fromBuiltin) = case ref of
                Reference.Derived compHash compIndex -> (Just compHash, Just $ pgComponentIndex compIndex, Nothing)
                Reference.Builtin builtin -> (Nothing, Nothing, Just builtin)
           in Set.toList typeEdits <&> \case
                PatchFullTypeEdit.Replace (Reference.Derived compHash compIndex) ->
                  (fromCompHash, fromCompIndex, fromBuiltin, Just compHash, Just $ pgComponentIndex compIndex, Nothing, False)
                PatchFullTypeEdit.Replace (Reference.Builtin builtin) ->
                  (fromCompHash, fromCompIndex, fromBuiltin, Nothing, Nothing, Just builtin, False)
                PatchFullTypeEdit.Deprecate ->
                  (fromCompHash, fromCompIndex, fromBuiltin, Nothing, Nothing, Nothing, True)

expectPatchEntity :: forall m. (HasCallStack, QueryM m) => PatchId -> m (Sync.Patch Text Hash32 Hash32)
expectPatchEntity patchId = do
  v2Patch <- expectPatch patchId
  patchToEntity v2Patch
  where
    patchToEntity :: V2.Patch -> m (Sync.Patch Text Hash32 Hash32)
    patchToEntity patch = do
      let patchFull = Cv.patchV2ToPF patch
      let (PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup}, localPatch) = Localize.localizePatchG patchFull
      let bytes = S.encodePatch localPatch
      Sync.Patch
        { textLookup = Vector.toList patchTextLookup,
          oldHashLookup = Vector.toList patchHashLookup,
          newHashLookup = Vector.toList patchDefnLookup,
          bytes
        }
        & Sync.patchOldHashes_ %~ Hash32.fromHash
        & Sync.patchNewHashes_ %~ Hash32.fromHash
        & pure

saveSerializedPatch :: (QueryM m) => PatchId -> CBORBytes TempEntity -> m ()
saveSerializedPatch patchId (CBORBytes bytes) = do
  bytesId <- DefnQ.ensureBytesIdsOf id (BL.toStrict bytes)
  execute_
    [sql|
      INSERT INTO serialized_patches (patch_id, bytes_id)
        VALUES (#{patchId}, #{bytesId})
      ON CONFLICT DO NOTHING
    |]

-- | Hash a namespace into a BranchHash
_hashPgPatch :: forall m. (QueryM m) => PgPatch -> m PatchHash
_hashPgPatch p = do
  PatchFull.Patch {termEdits, typeEdits} <-
    p
      & ( unsafePartsOf PatchFull.patchT_ doTexts
            >=> unsafePartsOf PatchFull.patchH_ doHashes
            >=> unsafePartsOf PatchFull.patchO_ doHashes
        )
  let patch =
        H.Patch
          { termEdits = doTermEdits termEdits,
            typeEdits = doTypeEdits typeEdits
          }
  let patchHash = PatchHash $ H.contentHash patch
  pure patchHash
  where
    doTexts :: [TextId] -> m [Text]
    doTexts textIds = do
      Defn.expectTextsOf traversed textIds
    doHashes :: [ComponentHashId] -> m [Hash]
    doHashes hashIds = do
      HashQ.expectComponentHashesOf traversed hashIds
        <&> fmap unComponentHash

    v2ToH2Referent :: Referent.Referent -> H.Referent
    v2ToH2Referent = \case
      Referent.Ref r -> H.ReferentRef (v2ToH2Reference r)
      Referent.Con r cid -> H.ReferentCon (v2ToH2Reference r) cid

    v2ToH2Reference :: Reference.Reference -> H.Reference
    v2ToH2Reference = \case
      Reference.ReferenceBuiltin x -> H.ReferenceBuiltin x
      Reference.ReferenceDerived (Reference.Id a b) -> H.ReferenceDerivedId (H.ReferenceId a b)

    doTypeEdits :: Map (Reference.Reference' Text Hash) (Set (PatchFullTypeEdit.TypeEdit' Text Hash)) -> Map H.Reference (Set H.TypeEdit)
    doTypeEdits = Map.bimap v2ToH2Reference (Set.map doTypeEdit)

    doTermEdits :: Map (PatchFull.Referent'' Text Hash) (Set (PatchFullTermEdit.TermEdit' Text Hash)) -> Map H.Referent (Set H.TermEdit)
    doTermEdits = Map.bimap v2ToH2Referent (Set.map doTermEdit)

    doTypeEdit :: PatchFullTypeEdit.TypeEdit' Text Hash -> H.TypeEdit
    doTypeEdit = \case
      PatchFullTypeEdit.Deprecate -> H.TypeEditDeprecate
      PatchFullTypeEdit.Replace r -> H.TypeEditReplace (v2ToH2Reference r)

    doTermEdit :: PatchFullTermEdit.TermEdit' Text Hash -> H.TermEdit
    doTermEdit = \case
      PatchFullTermEdit.Deprecate -> H.TermEditDeprecate
      PatchFullTermEdit.Replace r _t -> H.TermEditReplace (v2ToH2Referent r)
