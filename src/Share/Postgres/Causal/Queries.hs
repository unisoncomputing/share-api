{-# LANGUAGE ApplicativeDo #-}

-- Queries for causals (and namespaces)
module Share.Postgres.Causal.Queries
  ( loadCausalNamespace,
    expectCausalNamespace,
    expectPgCausalNamespace,
    loadCausalNamespaceAtPath,
    HashQ.loadCausalIdByHash,
    HashQ.expectCausalIdByHash,
    HashQ.expectCausalIdsOf,
    HashQ.expectCausalHashesByIdsOf,
    expectNamespace,
    expectPgNamespace,
    savePgNamespace,
    saveCausal,
    tryGetCachedSquashResult,
    saveSquashResult,
    saveV2BranchShallow,
    expectNamespaceStatsOf,
    expectNamespaceHashByCausalHash,
    HashQ.expectNamespaceIdsByCausalIdsOf,
    importAccessibleCausals,
    importCausalIntoCodebase,
    hashCausal,
    bestCommonAncestor,
    isFastForward,

    -- * Sync
    expectCausalEntity,
    expectNamespaceEntity,

    -- * For migrations, can probably remove this export later.
    saveSerializedCausal,
    saveSerializedNamespace,
  )
where

import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Share.Codebase.Types (CodebaseM)
import Share.Codebase.Types qualified as Codebase
import Share.IDs (UserId)
import Share.Postgres
import Share.Postgres.Causal.Types
import Share.Postgres.Definitions.Queries qualified as Defn
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Postgres.Definitions.Types
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.Patches.Queries qualified as PatchQ
import Share.Postgres.Serialization qualified as S
import Share.Postgres.Sync.Conversions qualified as Cv
import Share.Prelude
import Share.Utils.Postgres (OrdBy, ordered)
import Share.Web.Errors (MissingExpectedEntity (MissingExpectedEntity))
import U.Codebase.Branch hiding (NamespaceStats, nonEmptyChildren)
import U.Codebase.Branch qualified as V2 hiding (NamespaceStats)
import U.Codebase.Causal qualified as Causal
import U.Codebase.Causal qualified as U
import U.Codebase.Reference
import U.Codebase.Referent
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Branch.Format (LocalBranchBytes (..))
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.LocalizeObject qualified as Localize
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Codebase.Path qualified as Path
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H
import Unison.NameSegment.Internal as NameSegment
import Unison.Reference qualified as Reference
import Unison.Sync.Common qualified as SyncCommon
import Unison.Sync.Types qualified as Sync
import Unison.SyncV2.Types (CBORBytes (..))
import Unison.SyncV2.Types qualified as SyncV2
import Unison.Util.Map qualified as Map

expectCausalNamespace :: (HasCallStack, QueryM m) => CausalId -> m (CausalNamespace m)
expectCausalNamespace causalId =
  loadCausalNamespace causalId
    `whenNothingM` unrecoverableError (MissingExpectedEntity $ "Expected causal branch for hash:" <> tShow causalId)

loadPgCausalNamespace :: (HasCallStack) => CausalId -> CodebaseM e (Maybe (PgCausalNamespace (CodebaseM e)))
loadPgCausalNamespace causalId = runMaybeT $ do
  branchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id causalId
  let namespace = expectPgNamespace branchHashId
  ancestors <- lift $ ancestorsByCausalId causalId
  pure $
    Causal
      { causalHash = causalId,
        valueHash = branchHashId,
        parents = ancestors,
        value = namespace
      }
  where
    ancestorsByCausalId :: CausalId -> CodebaseM e ((Map CausalId (CodebaseM e (PgCausalNamespace (CodebaseM e)))))
    ancestorsByCausalId causalId = do
      getAncestors
        <&> fmap (\ancestorId -> (ancestorId, expectPgCausalNamespace ancestorId))
        <&> Map.fromList
      where
        getAncestors :: CodebaseM e [CausalId]
        getAncestors = do
          codebaseOwner <- asks Codebase.codebaseOwner
          queryListCol
            [sql| SELECT ca.ancestor_id
                    FROM causal_ancestors ca
                    WHERE ca.causal_id = #{causalId}
                      -- TODO: Just check ownership of the root causal.
                      AND EXISTS (SELECT FROM causal_ownership o WHERE o.causal_id = ca.ancestor_id AND o.user_id = #{codebaseOwner})
            |]

expectPgCausalNamespace :: (HasCallStack) => CausalId -> CodebaseM e (PgCausalNamespace (CodebaseM e))
expectPgCausalNamespace causalId =
  loadPgCausalNamespace causalId
    `whenNothingM` unrecoverableError (MissingExpectedEntity $ "Expected causal branch for causal: " <> tShow causalId)

loadCausalNamespace :: forall m. (QueryM m) => CausalId -> m (Maybe (CausalNamespace m))
loadCausalNamespace causalId = runMaybeT $ do
  causalHash <- HashQ.expectCausalHashesByIdsOf id causalId
  branchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id causalId
  namespaceHash <- HashQ.expectNamespaceHashesByNamespaceHashIdsOf id branchHashId
  let namespace = expectNamespace branchHashId
  ancestors <- lift $ ancestorsByCausalId causalId
  pure $
    Causal
      { causalHash = causalHash,
        valueHash = namespaceHash,
        parents = ancestors,
        value = namespace
      }
  where
    ancestorsByCausalId :: CausalId -> m ((Map CausalHash (m (CausalNamespace m))))
    ancestorsByCausalId causalId = do
      getAncestors
        <&> fmap (\(ancestorId, ancestorHash) -> (ancestorHash, expectCausalNamespace ancestorId))
        <&> Map.fromList
      where
        getAncestors :: m [(CausalId, CausalHash)]
        getAncestors = do
          queryListRows
            [sql| SELECT ancestor_id, ancestor.hash
                    FROM causal_ancestors
                      JOIN causals AS ancestor ON ancestor.id = ancestor_id
                    WHERE causal_id = #{causalId}
            |]

expectNamespaceHashByCausalHash :: CausalHash -> CodebaseM e (BranchHash, BranchHashId)
expectNamespaceHashByCausalHash causalHash = do
  codebaseOwner <- asks Codebase.codebaseOwner
  queryExpect1Row
    [sql| SELECT bh.base32, namespace_hash_id
            FROM causals
              JOIN branch_hashes bh ON causals.namespace_hash_id = bh.id
              WHERE causals.hash = #{causalHash}
                AND EXISTS (SELECT FROM causal_ownership o WHERE o.causal_id = causals.id AND o.user_id = #{codebaseOwner})
    |]

expectNamespace :: forall m. (QueryM m) => BranchHashId -> m (Branch m)
expectNamespace branchHashId = do
  termsAndConstructors <- getTermsAndConstructors branchHashId <&> (traversed . traversed %~ loadTermMetadata)
  types <- getTypes branchHashId <&> (traversed . traversed %~ loadTypeMetadata)
  patches <- getPatches branchHashId
  children <- getChildren branchHashId
  pure $
    Branch
      { -- (<>) is safe here because no referents will overlap between terms and constructors
        terms = termsAndConstructors,
        types,
        patches,
        children
      }
  where
    getTermsAndConstructors :: BranchHashId -> m (Map NameSegment (Map Referent NamespaceTermMappingId))
    getTermsAndConstructors branchHashId = do
      queryListRows @(NameSegment, Maybe Text, Maybe ComponentHash, Maybe PgComponentIndex, Maybe PgConstructorIndex, NamespaceTermMappingId)
        [sql| SELECT name_segment.text AS name_segment, builtin.text AS builtin_text, comp_hash.base32, COALESCE(term.component_index, constr_typ.component_index), constr.constructor_index, mapping.id
                FROM namespace_terms mapping
                JOIN text name_segment ON mapping.name_segment_id = name_segment.id
                LEFT JOIN terms term ON mapping.term_id = term.id
                LEFT JOIN text builtin ON mapping.builtin_id = builtin.id
                LEFT JOIN constructors constr ON mapping.constructor_id = constr.id
                LEFT JOIN types constr_typ ON constr.type_id = constr_typ.id
                LEFT JOIN component_hashes comp_hash ON COALESCE(term.component_hash_id, constr_typ.component_hash_id) = comp_hash.id
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(nameSegment, mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex, mappingId) ->
              case (mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex) of
                (Just builtinId, Nothing, Nothing, Nothing) ->
                  let ref = Referent.Ref (Reference.Builtin builtinId)
                   in (nameSegment, Map.singleton ref mappingId)
                (Nothing, Just (ComponentHash compHash), Just compIndex, Nothing) ->
                  let ref = Referent.Ref $ Reference.DerivedId (Reference.Id compHash (unPgComponentIndex compIndex))
                   in (nameSegment, Map.singleton ref mappingId)
                (Nothing, Just (ComponentHash compHash), Just compIndex, Just constrIndex) ->
                  let ref = Referent.Con (Reference.DerivedId (Reference.Id compHash (unPgComponentIndex compIndex))) (unPgConstructorIndex constrIndex)
                   in (nameSegment, Map.singleton ref mappingId)
                _ -> error $ "Invalid Term reference: " <> show (mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex)
          )
        <&> Map.fromListWith (<>)

    getTypes :: BranchHashId -> m (Map NameSegment (Map Reference NamespaceTypeMappingId))
    getTypes branchHashId = do
      queryListRows
        [sql| SELECT name_segment.text AS name_segment, builtin.text AS builtin_text, component_hashes.base32, typ.component_index, mapping.id
                FROM namespace_types mapping
                JOIN text name_segment ON mapping.name_segment_id = name_segment.id
                LEFT JOIN types typ ON mapping.type_id = typ.id
                LEFT JOIN component_hashes ON typ.component_hash_id = component_hashes.id
                LEFT JOIN text builtin ON mapping.builtin_id = builtin.id
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(name_segment, mayBuiltin, mayCompHash, mayCompIndex, mappingId) ->
              case (mayBuiltin, mayCompHash, mayCompIndex) of
                (Just builtinId, Nothing, Nothing) ->
                  let ref = Reference.Builtin builtinId
                   in (name_segment, Map.singleton ref mappingId)
                (_, Just (ComponentHash compHash), Just compIndex) ->
                  let ref = Reference.DerivedId (Reference.Id compHash (unPgComponentIndex compIndex))
                   in (name_segment, Map.singleton ref mappingId)
                _ -> error "Type reference contains both builtin and component hash"
          )
        <&> Map.fromListWith (<>)

    getPatches :: BranchHashId -> m (Map NameSegment (PatchHash, m Patch))
    getPatches branchHashId = do
      queryListRows
        [sql| SELECT name_segment.text AS name_segment, patch.hash, patch.id
                FROM namespace_patches mapping
                JOIN patches patch ON mapping.patch_id = patch.id
                JOIN text name_segment ON mapping.name_segment_id = name_segment.id
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(name_segment, patchHash, patchId) ->
              (name_segment, (patchHash, PatchQ.expectPatch patchId))
          )
        <&> Map.fromList

    getChildren :: BranchHashId -> m (Map NameSegment (CausalBranch m))
    getChildren branchHashId = do
      childIds <-
        queryListRows
          [sql| SELECT name_segment.text AS name_segment, mapping.child_causal_id
                FROM namespace_children mapping
                JOIN text name_segment ON mapping.name_segment_id = name_segment.id
                WHERE mapping.parent_namespace_hash_id = #{branchHashId}
        |]
      childList <- for childIds \(name_segment, causalId) -> do
        (name_segment,) <$> expectCausalNamespace causalId
      pure $ Map.fromList childList

    loadTermMetadata :: NamespaceTermMappingId -> m MdValues
    loadTermMetadata mappingId = do
      queryListRows @(Maybe Hash, Maybe PgComponentIndex, Maybe Text)
        [sql|
      SELECT metadata_term_hash.base32, metadata_term.component_index, metadata_builtin.text
        FROM namespace_term_metadata meta
          LEFT JOIN terms metadata_term ON meta.metadata_term_id = metadata_term.id
          LEFT JOIN component_hashes metadata_term_hash ON metadata_term.component_hash_id = metadata_term_hash.id
          LEFT JOIN text metadata_builtin ON meta.metadata_builtin_id = metadata_builtin.id
        WHERE meta.named_term = #{mappingId}
      |]
        <&> formatMdValues

    loadTypeMetadata :: NamespaceTypeMappingId -> m MdValues
    loadTypeMetadata mappingId =
      do
        queryListRows @(Maybe Hash, Maybe PgComponentIndex, Maybe Text)
          [sql|
        SELECT metadata_term_hash.base32, metadata_term.component_index, metadata_builtin.text
          FROM namespace_type_metadata meta
            LEFT JOIN terms metadata_term ON meta.metadata_term_id = metadata_term.id
            LEFT JOIN component_hashes metadata_term_hash ON metadata_term.component_hash_id = metadata_term_hash.id
            LEFT JOIN text metadata_builtin ON meta.metadata_builtin_id = metadata_builtin.id
          WHERE meta.named_type = #{mappingId}
      |]
        <&> formatMdValues

    formatMdValues :: [(Maybe Hash, Maybe PgComponentIndex, Maybe Text)] -> MdValues
    formatMdValues =
      MdValues . Set.fromList . fmap \case
        (Just metaValueTermHash, Just metaValueTermIndex, Nothing) ->
          (Reference.Derived metaValueTermHash (unPgComponentIndex metaValueTermIndex))
        (Nothing, Nothing, Just builtin) -> Reference.Builtin builtin
        _ -> error "Unexpected metadata format"

expectPgNamespace :: BranchHashId -> CodebaseM e PgNamespace
expectPgNamespace branchHashId = do
  termsAndConstructors <- getTermsAndConstructors branchHashId >>= (traversed . traversed %%~ loadTermMetadata)
  types <- getTypes branchHashId >>= (traversed . traversed %%~ loadTypeMetadata)
  patches <- getPatches branchHashId
  children <- getChildren branchHashId
  pure $
    BranchFull.Branch
      { terms = termsAndConstructors,
        types = types,
        patches = patches,
        children = children
      }
  where
    loadTermMetadata :: NamespaceTermMappingId -> CodebaseM e (BranchFull.MetadataSetFormat' TextId ComponentHashId)
    loadTermMetadata mappingId = do
      queryListRows @(Reference' TextId ComponentHashId)
        [sql|
      SELECT meta.metadata_builtin_id, metadata_term.component_hash_id, metadata_term.component_index
        FROM namespace_term_metadata meta
          LEFT JOIN terms metadata_term ON meta.metadata_term_id = metadata_term.id
        WHERE meta.named_term = #{mappingId}
      |]
        <&> BranchFull.Inline . Set.fromList

    loadTypeMetadata :: NamespaceTypeMappingId -> CodebaseM e (BranchFull.MetadataSetFormat' TextId ComponentHashId)
    loadTypeMetadata mappingId =
      do
        queryListRows @(Reference' TextId ComponentHashId)
          [sql|
        SELECT meta.metadata_builtin_id, metadata_term.component_hash_id, metadata_term.component_index
          FROM namespace_type_metadata meta
            LEFT JOIN terms metadata_term ON meta.metadata_term_id = metadata_term.id
          WHERE meta.named_type = #{mappingId}
      |]
        <&> BranchFull.Inline . Set.fromList

    getTermsAndConstructors :: BranchHashId -> CodebaseM e (Map TextId (Map (BranchFull.Referent'' TextId ComponentHashId) NamespaceTermMappingId))
    getTermsAndConstructors branchHashId = do
      queryListRows @(NamespaceTermMappingId, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex, Maybe PgConstructorIndex)
        [sql| SELECT mapping.id, mapping.name_segment_id, mapping.builtin_id, COALESCE(term.component_hash_id, constr_typ.component_hash_id), COALESCE(term.component_index, constr_typ.component_index), constr.constructor_index
                FROM namespace_terms mapping
                LEFT JOIN terms term ON mapping.term_id = term.id
                LEFT JOIN constructors constr ON mapping.constructor_id = constr.id
                LEFT JOIN types constr_typ ON constr.type_id = constr_typ.id
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(mappingId, nameSegment, mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex) ->
              case (mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex) of
                (Just builtinId, Nothing, Nothing, Nothing) ->
                  let ref = Referent.Ref (Reference.Builtin builtinId)
                   in (nameSegment, Map.singleton ref mappingId)
                (Nothing, Just compHashId, Just compIndex, Nothing) ->
                  let ref = Referent.Ref $ Reference.DerivedId (Reference.Id compHashId (unPgComponentIndex compIndex))
                   in (nameSegment, Map.singleton ref mappingId)
                (Nothing, Just compHashId, Just compIndex, Just constrIndex) ->
                  let ref = Referent.Con (Reference.DerivedId (Reference.Id compHashId (unPgComponentIndex compIndex))) (unPgConstructorIndex constrIndex)
                   in (nameSegment, Map.singleton ref mappingId)
                _ -> error $ "Invalid term reference: " <> show (mayBuiltin, mayCompHash, mayCompIndex, mayConstrIndex)
          )
        <&> Map.fromListWith (Map.unionWithKey \ref a b -> if a == b then a else error ("expectPGNamespace: getTermsAndConstructors: Encountered different mapping ids for the same name and ref which shouldn't be possible: " <> show (ref, a, b)))

    getTypes :: BranchHashId -> CodebaseM e (Map TextId (Map (TypeReference' TextId ComponentHashId) NamespaceTypeMappingId))
    getTypes branchHashId = do
      queryListRows @(NamespaceTypeMappingId, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex)
        [sql| SELECT mapping.id, name_segment.text, builtin.text, component_hashes.base32, component_index
                FROM namespace_types mapping
                JOIN types typ ON mapping.type_id = typ.id
                JOIN text name_segment ON mapping.name_segment_id = name_segment.id
                LEFT JOIN component_hashes ON typ.component_hash_id = component_hashes.id
                LEFT JOIN text builtin ON mapping.builtin_id = builtin.id
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(mappingId, nameSegment, mayBuiltin, mayCompHash, mayCompIndex) ->
              case (mayBuiltin, mayCompHash, mayCompIndex) of
                (Just builtinId, Nothing, Nothing) ->
                  let ref = Reference.Builtin builtinId
                   in (nameSegment, Map.singleton ref mappingId)
                (_, Just compHash, Just compIndex) ->
                  let ref = Reference.DerivedId (Reference.Id compHash (unPgComponentIndex compIndex))
                   in (nameSegment, Map.singleton ref mappingId)
                _ -> error "Type reference contains both builtin and component hash"
          )
        <&> Map.fromListWith (Map.unionWithKey \ref a b -> if a == b then a else error ("expectPGNamespace: getTypes: Encountered different mapping ids for the same name and ref which shouldn't be possible: " <> show (ref, a, b)))

    getPatches :: BranchHashId -> CodebaseM e (Map TextId PatchId)
    getPatches branchHashId = do
      queryListRows
        [sql| SELECT mapping.name_segment_id, mapping.patch_id
                FROM namespace_patches mapping
                WHERE namespace_hash_id = #{branchHashId}
        |]
        <&> fmap
          ( \(nameSegment, patchHashId) ->
              (nameSegment, patchHashId)
          )
        <&> Map.fromList

    getChildren :: BranchHashId -> CodebaseM e (Map TextId (BranchHashId, CausalId))
    getChildren branchHashId = do
      childList <-
        queryListRows
          [sql| SELECT mapping.name_segment_id, causal.namespace_hash_id, mapping.child_causal_id
                FROM namespace_children mapping
                JOIN causals causal ON mapping.child_causal_id = causal.id
                WHERE mapping.parent_namespace_hash_id = #{branchHashId}
        |]
          <&> fmap \(nameSegmentId, branchHashId, causalId) ->
            (nameSegmentId, (branchHashId, causalId))
      pure $ Map.fromList childList

-- | Crawls the namespace tree to find the causal id mounted at a given path from the provided
-- root causal.
-- Returns Nothing if there's no causal at the provided path (or if the root causal doesn't exist)
loadCausalIdAtPath :: (HasCallStack) => CausalId -> Path.Path -> CodebaseM e (Maybe CausalId)
loadCausalIdAtPath rootCausalId path = runMaybeT $ do
  codebaseOwner <- asks Codebase.codebaseOwner
  let pathArray = Path.toList path
  MaybeT $
    -- Due to the way the function call works we'll always get one row, but the column might be NULL.
    queryExpect1Col @(Maybe CausalId)
      [sql|
      SELECT causal_at_path(#{rootCausalId}, #{pathArray}) AS causal_id
        WHERE EXISTS (SELECT FROM causal_ownership o WHERE o.causal_id = causal_id AND o.user_id = #{codebaseOwner})
    |]

loadCausalNamespaceAtPath :: (HasCallStack) => CausalId -> Path.Path -> CodebaseM e (Maybe (CausalNamespace (CodebaseM e)))
loadCausalNamespaceAtPath causalId path = do
  causalId <- loadCausalIdAtPath causalId path
  traverse expectCausalNamespace causalId

-- | Given a namespace whose dependencies have all been pre-saved, save it to the database under the given hash.
savePgNamespace ::
  (HasCallStack) =>
  -- | The pre-serialized namespace, if available. If Nothing it will be re-generated, which is slower.
  Maybe TempEntity ->
  -- Normally we'd prefer to always hash it ourselves, but there are some bad hashes in the wild
  -- that we need to support saving, if we're passed a hash to save a branch at we will save
  -- it at that hash regardless of what the _actual_ hash is.
  Maybe BranchHash ->
  PgNamespace ->
  CodebaseM e (BranchHashId, BranchHash)
savePgNamespace maySerialized mayBh b@(BranchFull.Branch {terms, types, patches, children}) = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  bh <- whenNothing mayBh $ hashPgNamespace b
  bhId <- HashQ.ensureBranchHashId bh
  queryExpect1Col [sql| SELECT EXISTS (SELECT FROM namespaces WHERE namespace_hash_id = #{bhId}) |] >>= \case
    False -> do
      doSave bhId
      doSaveSerialized bhId
    True -> pure ()
  execute_
    [sql| INSERT INTO namespace_ownership (namespace_hash_id, user_id)
                   VALUES (#{bhId}, #{codebaseOwnerUserId})
                   ON CONFLICT DO NOTHING
    |]
  pure (bhId, bh)
  where
    doSaveSerialized :: BranchHashId -> CodebaseM e ()
    doSaveSerialized bhId = do
      nsEntity <- case maySerialized of
        Just serialized -> pure serialized
        Nothing -> SyncCommon.entityToTempEntity id . Sync.N <$> expectNamespaceEntity bhId
      let serializedNamespace = SyncV2.serialiseCBORBytes nsEntity
      saveSerializedNamespace bhId serializedNamespace

    doSave :: BranchHashId -> CodebaseM e ()
    doSave bhId = do
      -- Expand all term mappings into a list
      let termsWithMeta :: [(OrdBy, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex, Maybe PgConstructorIndex, (BranchFull.MetadataSetFormat' TextId ComponentHashId))]
          termsWithMeta =
            terms
              & itoListOf (itraversed <. folding Map.toList)
              & imap
                ( \ord (nameSegment, (ref, meta)) ->
                    case ref of
                      Referent.Ref (Reference.Builtin builtinTxt) ->
                        (into @OrdBy ord, nameSegment, Just builtinTxt, Nothing, Nothing, Nothing, meta)
                      Referent.Ref (Reference.DerivedId (Reference.Id compHashId compIndex)) ->
                        (into @OrdBy ord, nameSegment, Nothing, Just compHashId, Just (pgComponentIndex compIndex), Nothing, meta)
                      Referent.Con (Reference.Builtin {}) _constrIndex ->
                        error "Builtin constructors not yet supported"
                      Referent.Con (Reference.DerivedId (Reference.Id compHashId compIndex)) constrIndex ->
                        (into @OrdBy ord, nameSegment, Nothing, Just compHashId, Just (pgComponentIndex compIndex), Just (pgConstructorIndex constrIndex), meta)
                )
      let termsTable :: [(OrdBy, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex, Maybe PgConstructorIndex)]
          termsTable =
            termsWithMeta <&> \(ordBy, nameSegment, builtinTxt, compHashId, compIndex, constrIndex, _) ->
              (ordBy, nameSegment, builtinTxt, compHashId, compIndex, constrIndex)
      termMappingIds :: Map OrdBy NamespaceTermMappingId <-
        whenNonEmpty termsTable $ do
          queryListRows @(OrdBy, NamespaceTermMappingId)
            [sql|
          WITH term_mappings(ord, name_segment_id, builtin_id, component_hash_id, component_index, constructor_index) AS (
            SELECT * FROM ^{toTable termsTable}
          ),    dereferenced_terms(ord, name_segment_id, builtin_id, term_id, constructor_id) AS (
            SELECT term_mappings.ord, term_mappings.name_segment_id, term_mappings.builtin_id, term.id, constructor.id
              FROM term_mappings
                LEFT JOIN terms term ON (term_mappings.component_hash_id = term.component_hash_id AND term_mappings.component_index = term.component_index)
                LEFT JOIN types constructor_type ON (term_mappings.component_hash_id = constructor_type.component_hash_id AND term_mappings.component_index = constructor_type.component_index)
                LEFT JOIN constructors constructor ON (constructor.type_id = constructor_type.id AND term_mappings.constructor_index = constructor.constructor_index)
          ),
               inserted_namespace_terms (name_segment_id, builtin_id, term_id, constructor_id, namespace_term_id) AS (
            INSERT INTO namespace_terms (namespace_hash_id, name_segment_id, builtin_id, term_id, constructor_id)
              SELECT #{bhId}, dt.name_segment_id, dt.builtin_id, dt.term_id, dt.constructor_id
                FROM dereferenced_terms dt
            RETURNING namespace_terms.name_segment_id, namespace_terms.builtin_id, namespace_terms.term_id, namespace_terms.constructor_id, namespace_terms.id
          ) SELECT dereferenced_terms.ord, inserted_namespace_terms.namespace_term_id
              FROM dereferenced_terms
                JOIN inserted_namespace_terms
                  ON (dereferenced_terms.name_segment_id = inserted_namespace_terms.name_segment_id
                  AND dereferenced_terms.builtin_id IS NOT DISTINCT FROM inserted_namespace_terms.builtin_id
                  AND dereferenced_terms.term_id  IS NOT DISTINCT FROM inserted_namespace_terms.term_id
                  AND dereferenced_terms.constructor_id IS NOT DISTINCT FROM inserted_namespace_terms.constructor_id)
              ORDER BY dereferenced_terms.ord ASC
          |]
            <&> Map.fromListWith (error "Encountered duplicate OrdBy when saving metadata in termMappingIds")

      let termIdsWithMeta :: [(NamespaceTermMappingId, (BranchFull.MetadataSetFormat' TextId ComponentHashId))]
          termIdsWithMeta =
            termsWithMeta
              & fmap
                ( \(ordBy, _, _, _, _, _, meta) ->
                    let mappingId = (fromMaybe (error "termMappingIds: Expected to find NamespaceTermMappingId for ordby key") $ Map.lookup ordBy termMappingIds)
                     in (mappingId, meta)
                )
      -- Expand all type mappings into a list, paired with their (arbitrary) numerical index which
      -- we'll use to guarantee a stable sort order from PG.
      let typesWithMeta :: [(OrdBy, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex, (BranchFull.MetadataSetFormat' TextId ComponentHashId))]
          typesWithMeta =
            types
              & itoListOf (itraversed <. folding Map.toList)
              & imap
                ( \ord (nameSegment, (ref, meta)) ->
                    case ref of
                      Reference.Builtin builtinTxt ->
                        (into @OrdBy ord, nameSegment, Just builtinTxt, Nothing, Nothing, meta)
                      Reference.DerivedId (Reference.Id compHashId compIndex) ->
                        (into @OrdBy ord, nameSegment, Nothing, Just compHashId, Just (pgComponentIndex compIndex), meta)
                )
      let typesTable :: [(OrdBy, TextId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex)]
          typesTable =
            typesWithMeta <&> \(ordBy, nameSegment, builtinTxt, compHashId, compIndex, _) ->
              (ordBy, nameSegment, builtinTxt, compHashId, compIndex)
      typeMappingIds :: Map OrdBy NamespaceTypeMappingId <-
        whenNonEmpty typesTable $ do
          queryListRows @(OrdBy, NamespaceTypeMappingId)
            [sql|
          WITH type_mappings(ord, name_segment_id, builtin_id, component_hash_id, component_index) AS (
            SELECT * FROM ^{toTable typesTable}
          ), dereferenced_types(ord, name_segment_id, builtin_id, type_id) AS (
            SELECT type_mappings.ord, type_mappings.name_segment_id, type_mappings.builtin_id, type.id
              FROM type_mappings
                LEFT JOIN types type ON (type_mappings.component_hash_id = type.component_hash_id AND type_mappings.component_index = type.component_index)
          ),
               inserted_namespace_types (name_segment_id, builtin_id, type_id, namespace_type_id) AS (
            INSERT INTO namespace_types (namespace_hash_id, name_segment_id, builtin_id, type_id)
              SELECT #{bhId}, dt.name_segment_id, dt.builtin_id, dt.type_id
                FROM dereferenced_types dt
            RETURNING namespace_types.name_segment_id, namespace_types.builtin_id, namespace_types.type_id, namespace_types.id
          ) SELECT dereferenced_types.ord, inserted_namespace_types.namespace_type_id
              FROM dereferenced_types
                JOIN inserted_namespace_types
                  ON (dereferenced_types.name_segment_id = inserted_namespace_types.name_segment_id
                  AND dereferenced_types.builtin_id IS NOT DISTINCT FROM inserted_namespace_types.builtin_id
                  AND dereferenced_types.type_id  IS NOT DISTINCT FROM inserted_namespace_types.type_id)
              ORDER BY dereferenced_types.ord ASC
          |]
            <&> Map.fromListWith (error "Encountered duplicate OrdBy when saving metadata in typeMappingIds")

      let typeIdsWithMeta :: [(NamespaceTypeMappingId, (BranchFull.MetadataSetFormat' TextId ComponentHashId))]
          typeIdsWithMeta =
            typesWithMeta
              & fmap
                ( \(ordBy, _, _, _, _, meta) ->
                    let mappingId = (fromMaybe (error "typeMappingIds: Expected to find NamespaceTypeMappingId for ordby key") $ Map.lookup ordBy typeMappingIds)
                     in (mappingId, meta)
                )

      let termMetaTable :: [(NamespaceTermMappingId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex)]
          termMetaTable =
            termIdsWithMeta
              & foldMap (\(mappingId, BranchFull.Inline refSet) -> (mappingId,) <$> Set.toList refSet)
              & fmap
                ( \(mappingId, ref) -> case ref of
                    Reference.Builtin builtinId -> (mappingId, Just builtinId, Nothing, Nothing)
                    Reference.DerivedId (Reference.Id compHashId compIndex) -> (mappingId, Nothing, Just compHashId, Just (pgComponentIndex compIndex))
                )
      whenNonEmpty termMetaTable $ do
        execute_
          [sql|
            WITH term_meta(mapping_id, builtin_id, component_hash_id, component_index) AS (
              SELECT * FROM ^{toTable termMetaTable}
            )
            INSERT INTO namespace_term_metadata(named_term, metadata_builtin_id, metadata_term_id)
              SELECT mapping_id, builtin_id, term.id
                FROM term_meta
                      LEFT JOIN terms term ON (term_meta.component_hash_id = term.component_hash_id AND term_meta.component_index = term.component_index)
            |]
      let typeMetaTable :: [(NamespaceTypeMappingId, Maybe TextId, Maybe ComponentHashId, Maybe PgComponentIndex)]
          typeMetaTable =
            typeIdsWithMeta
              & foldMap (\(mappingId, BranchFull.Inline refSet) -> (mappingId,) <$> Set.toList refSet)
              & fmap
                ( \(mappingId, ref) -> case ref of
                    Reference.Builtin builtinId -> (mappingId, Just builtinId, Nothing, Nothing)
                    Reference.DerivedId (Reference.Id compHashId compIndex) -> (mappingId, Nothing, Just compHashId, Just (pgComponentIndex compIndex))
                )
      whenNonEmpty typeMetaTable $ do
        execute_
          [sql|
            WITH type_meta(mapping_id, builtin_id, component_hash_id, component_index) AS (
              SELECT * FROM ^{toTable typeMetaTable}
            )
            INSERT INTO namespace_type_metadata(named_type, metadata_builtin_id, metadata_term_id)
              SELECT mapping_id, builtin_id, term.id
                FROM type_meta
                      LEFT JOIN terms term ON (type_meta.component_hash_id = term.component_hash_id AND type_meta.component_index = term.component_index)
            |]

      let patchesTable :: [(TextId, PatchId)]
          patchesTable = Map.toList patches
      whenNonEmpty patchesTable $ do
        execute_
          [sql|
            WITH patch_mappings(name_segment_id, patch_id) AS (
              SELECT * FROM ^{toTable patchesTable}
            )
            INSERT INTO namespace_patches (namespace_hash_id, name_segment_id, patch_id)
              SELECT #{bhId}, patch_mappings.name_segment_id, patch_id
                FROM patch_mappings
            |]
      let childTable :: [(TextId, CausalId)]
          childTable = Map.toList (fmap snd children)
      whenNonEmpty childTable $ do
        execute_
          [sql|
            WITH child_mappings(name_segment_id, causal_id) AS (
              SELECT * FROM ^{toTable childTable}
            )
            INSERT INTO namespace_children (parent_namespace_hash_id, name_segment_id, child_causal_id)
              SELECT #{bhId}, child_mappings.name_segment_id, causal_id
                FROM child_mappings
            |]
      -- Note: this must be run AFTER inserting the namespace and all its children.
      execute_ [sql| SELECT save_namespace(#{bhId}) |]
      execute_ [sql| SELECT update_namespace_depth(#{bhId}) |]

saveSerializedNamespace :: (QueryM m) => BranchHashId -> CBORBytes TempEntity -> m ()
saveSerializedNamespace bhId (CBORBytes bytes) = do
  bytesId <- DefnQ.ensureBytesIdsOf id (BL.toStrict bytes)
  execute_
    [sql|
      INSERT INTO serialized_namespaces (namespace_hash_id, bytes_id)
        VALUES (#{bhId}, #{bytesId})
      ON CONFLICT DO NOTHING
    |]

expectNamespaceEntity :: BranchHashId -> CodebaseM e (Sync.Namespace Text Hash32)
expectNamespaceEntity bhId = do
  v2Branch <- expectNamespace bhId
  second Hash32.fromHash <$> branchToEntity v2Branch
  where
    branchToEntity branch = do
      branchFull <- Cv.branchV2ToBF branch
      let (BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup}, localBranch) = Localize.localizeBranchG branchFull
      let bytes = LocalBranchBytes $ S.encodeNamespace localBranch
      pure $
        Sync.Namespace
          { textLookup = Vector.toList branchTextLookup,
            defnLookup = Vector.toList branchDefnLookup,
            patchLookup = Vector.toList branchPatchLookup,
            childLookup = Vector.toList branchChildLookup,
            bytes = bytes
          }

-- | Hash a namespace into a BranchHash
hashPgNamespace :: forall m. (QueryM m) => PgNamespace -> m BranchHash
hashPgNamespace b = do
  BranchFull.Branch {terms, types, patches, children} <-
    b
      & ( unsafePartsOf BranchFull.t_ doTexts
            >=> unsafePartsOf BranchFull.h_ doHashes
            >=> unsafePartsOf BranchFull.p_ doPatchHashes
            >=> unsafePartsOf BranchFull.c_ doChildren
        )
  let branch =
        H.Branch
          { terms = doTerms terms,
            types = doTypes types,
            patches = Map.mapKeys H.NameSegment patches,
            children = Map.mapKeys H.NameSegment children
          }
  let branchHash = BranchHash $ H.contentHash branch
  pure branchHash
  where
    doTerms :: Map Text (Map Referent (BranchFull.MetadataSetFormat' Text Hash)) -> Map H.NameSegment (Map H.Referent H.MdValues)
    doTerms = Map.bimap H.NameSegment (Map.bimap v2ToH2Referent (\(BranchFull.Inline md) -> H.MdValues $ Set.map v2ToH2Reference md))
    doTypes :: Map Text (Map Reference (BranchFull.MetadataSetFormat' Text Hash)) -> Map H.NameSegment (Map H.Reference H.MdValues)
    doTypes = Map.bimap H.NameSegment (Map.bimap v2ToH2Reference (\(BranchFull.Inline md) -> H.MdValues $ Set.map v2ToH2Reference md))
    doTexts :: [TextId] -> m [Text]
    doTexts textIds = do
      Defn.expectTextsOf traversed textIds
    doHashes :: [ComponentHashId] -> m [Hash]
    doHashes hashIds = do
      HashQ.expectComponentHashesOf traversed hashIds
        <&> fmap unComponentHash
    doPatchHashes :: [PatchId] -> m [Hash]
    doPatchHashes patchIds = do
      HashQ.expectPatchHashesOf traversed patchIds
        <&> fmap unPatchHash

    doChildren :: [(BranchHashId, CausalId)] -> m [Hash]
    doChildren hashes =
      hashes
        & fmap snd
        & HashQ.expectCausalHashesByIdsOf traversed
        <&> fmap unCausalHash

    v2ToH2Reference :: Reference.Reference -> H.Reference
    v2ToH2Reference = \case
      Reference.ReferenceBuiltin x -> H.ReferenceBuiltin x
      Reference.ReferenceDerived (Reference.Id a b) -> H.ReferenceDerivedId (H.ReferenceId a b)

    v2ToH2Referent :: Referent -> H.Referent
    v2ToH2Referent = \case
      Referent.Ref r -> H.ReferentRef (v2ToH2Reference r)
      Referent.Con r cid -> H.ReferentCon (v2ToH2Reference r) cid

hashCausal :: (QueryM m) => BranchHashId -> Set CausalId -> m CausalHash
hashCausal branchHashId ancestorIds = do
  branchHash <- HashQ.expectBranchHash branchHashId
  ancestors <-
    HashQ.expectCausalHashesByIdsOf traversed (Set.toList ancestorIds)
      <&> fmap unCausalHash
      <&> Set.fromList
  let hCausal = H.Causal {branchHash = unBranchHash branchHash, parents = ancestors}
  pure . CausalHash . H.contentHash $ hCausal

saveCausal ::
  -- | The pre-serialized causal, if available. If Nothing it will be re-generated, which is slower.
  Maybe TempEntity ->
  Maybe CausalHash ->
  BranchHashId ->
  Set CausalId ->
  CodebaseM e (CausalId, CausalHash)
saveCausal maySerializedCausal mayCh bhId ancestorIds = do
  ch <- maybe (hashCausal bhId ancestorIds) pure mayCh
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  cId <-
    query1Col [sql| SELECT id FROM causals WHERE hash = #{ch} |] >>= \case
      Just cId -> pure cId
      Nothing -> do
        cId <- doSave ch
        doSaveSerialized cId
        pure cId
  execute_
    [sql|
    INSERT INTO causal_ownership (user_id, causal_id)
      VALUES (#{codebaseOwnerUserId}, #{cId})
      ON CONFLICT DO NOTHING
    |]
  pure (cId, ch)
  where
    doSaveSerialized cId = do
      causalEntity <- case maySerializedCausal of
        Just serializedCausal -> pure serializedCausal
        Nothing -> do
          SyncCommon.entityToTempEntity id . Sync.C <$> expectCausalEntity cId
      let serializedCausal = SyncV2.serialiseCBORBytes causalEntity
      saveSerializedCausal cId serializedCausal

    doSave ch = do
      cId <-
        queryExpect1Col
          [sql|
        INSERT INTO causals(hash, namespace_hash_id)
          VALUES (#{ch}, #{bhId})
          RETURNING id
      |]
      let ancestorsTable = Set.toList ancestorIds
      execute_
        [sql|
      WITH ancestors(ancestor_id) AS (
        SELECT * FROM ^{singleColumnTable ancestorsTable}
      )
      INSERT INTO causal_ancestors (causal_id, ancestor_id)
        SELECT #{cId}, a.ancestor_id
          FROM ancestors a
      |]
      execute_ [sql| SELECT update_causal_depth(#{cId}) |]
      pure cId

saveSerializedCausal :: (QueryM m) => CausalId -> CBORBytes TempEntity -> m ()
saveSerializedCausal causalId (CBORBytes bytes) = do
  bytesId <- DefnQ.ensureBytesIdsOf id (BL.toStrict bytes)
  execute_
    [sql|
      INSERT INTO serialized_causals (causal_id, bytes_id)
        VALUES (#{causalId}, #{bytesId})
      ON CONFLICT DO NOTHING
    |]

expectCausalEntity :: (HasCallStack) => CausalId -> CodebaseM e (Sync.Causal Hash32)
expectCausalEntity causalId = do
  U.Causal {valueHash, parents} <- expectCausalNamespace causalId
  pure $
    ( Sync.Causal
        { namespaceHash = Hash32.fromHash $ unBranchHash valueHash,
          parents = Set.map (Hash32.fromHash . unCausalHash) . Map.keysSet $ parents
        }
    )

-- | Get the ref to the result of squashing if we've squashed that ref in the past.
-- Also adds the squash result to current codebase if we find it.
tryGetCachedSquashResult :: BranchHash -> CodebaseM e (Maybe CausalId)
tryGetCachedSquashResult branchHash = runMaybeT $ do
  codebaseOwner <- asks Codebase.codebaseOwner
  squashedCausalId <-
    MaybeT $
      query1Col
        [sql|
      SELECT sr.squashed_causal_id
        FROM branch_hashes branch_hash
          JOIN squash_results sr ON sr.unsquashed_branch_hash_id = branch_hash.id
        WHERE branch_hash.base32 = #{branchHash}
          AND EXISTS (SELECT FROM namespace_ownership o
                       WHERE o.namespace_hash_id = sr.unsquashed_branch_hash_id
                         AND o.user_id = #{codebaseOwner})
    |]
  execute_ [sql| SELECT inherit_squashed_causal(#{squashedCausalId}, #{codebaseOwner}) |]
  pure squashedCausalId

-- | Caches the result of a squash.
saveSquashResult :: BranchHash -> CausalId -> CodebaseM e ()
saveSquashResult unsquashedBranchHash squashedCausalHashId = do
  execute_
    [sql|
      INSERT INTO squash_results (unsquashed_branch_hash_id, squashed_causal_id)
        SELECT (SELECT id FROM branch_hashes WHERE base32 = #{unsquashedBranchHash}),
               #{squashedCausalHashId}
        ON CONFLICT DO NOTHING
      |]

-- | Saves a namespace to the database, returning its hash and id.
-- NOTE: This does not save the namespace's dependencies, or its children, the caller must
-- ensure those are saved beforehand.
saveV2BranchShallow :: V2.Branch (CodebaseM e) -> CodebaseM e (BranchHashId, BranchHash)
saveV2BranchShallow v2Branch = do
  pgNamespace <- expectV2BranchDependencies v2Branch
  savePgNamespace Nothing Nothing pgNamespace
  where
    expectV2BranchDependencies :: V2.Branch (CodebaseM e) -> CodebaseM e PgNamespace
    expectV2BranchDependencies V2.Branch {terms, types, patches, children} = do
      terms' <-
        terms
          & Map.bitraverse (pure . NameSegment.toUnescapedText) (traverse (fmap mdValuesToMetadataSetFormat))
      types' <-
        types
          & Map.bitraverse (pure . NameSegment.toUnescapedText) (traverse (fmap mdValuesToMetadataSetFormat))
      let patches' =
            patches
              & Map.bimap NameSegment.toUnescapedText fst
      let children' =
            children
              & Map.bimap NameSegment.toUnescapedText Causal.causalHash
      let hashBranchFull :: BranchFull.Branch' Text Hash PatchHash CausalHash =
            BranchFull.Branch
              { terms = terms',
                types = types',
                patches = patches',
                children = children'
              }
      -- resolve to IDs
      hashBranchFull
        & BranchFull.h_ %~ ComponentHash
        & Defn.ensureTextIdsOf BranchFull.t_
        >>= HashQ.ensureComponentHashIdsOf BranchFull.h_
        >>= HashQ.expectPatchIdsOf BranchFull.p_
        >>= HashQ.expectCausalIdsOf BranchFull.c_

    mdValuesToMetadataSetFormat :: V2.MdValues -> BranchFull.MetadataSetFormat' Text Hash
    mdValuesToMetadataSetFormat (V2.MdValues meta) = BranchFull.Inline meta

-- | Get the namespace stats of a namespace.
expectNamespaceStatsOf :: (QueryM m) => Traversal s t BranchHash NamespaceStats -> s -> m t
expectNamespaceStatsOf trav s =
  s
    & unsafePartsOf trav %%~ \branchHashes -> do
      let branchHashTable :: [(OrdBy, BranchHash)] = ordered branchHashes
      results <-
        whenNonEmpty branchHashTable $
          queryListRows
            [sql|
            WITH values(ord, hash) AS (
              SELECT * FROM ^{toTable branchHashTable}
            )
            SELECT contained_terms, deep_contained_terms, contained_types, deep_contained_types, contained_constructors, deep_contained_constructors
              FROM values
              JOIN branch_hashes ON values.hash = branch_hashes.base32
              JOIN namespaces ON branch_hashes.id = namespaces.namespace_hash_id
              ORDER BY values.ord
        |]
            <&> fmap \(containedTerms, deepContainedTerms, containedTypes, deepContainedTypes, containedConstructors, deepContainedConstructors) ->
              NamespaceStats
                { containedTerms = fromIntegral @Int64 @Int containedTerms,
                  deepContainedTerms = fromIntegral @Int64 @Int deepContainedTerms,
                  containedTypes = fromIntegral @Int64 @Int containedTypes,
                  deepContainedTypes = fromIntegral @Int64 @Int deepContainedTypes,
                  containedConstructors = fromIntegral @Int64 @Int containedConstructors,
                  deepContainedConstructors = fromIntegral @Int64 @Int deepContainedConstructors
                }
      if length results /= length branchHashes
        then unrecoverableError $ MissingExpectedEntity ("namespaceStatsOf: Expected namespace stats for all hashes: " <> tShow branchHashes)
        else pure results

-- | Copy a causal and all its dependencies from one codebase to another.
-- Make sure you've done some form of authorization check before calling this.
importCausalIntoCodebase :: UserId -> CausalId -> CodebaseM e ()
importCausalIntoCodebase fromCodebaseUserId causalId = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  execute_
    [sql|
      SELECT copy_causal_into_codebase(#{causalId}, #{fromCodebaseUserId}, #{codebaseOwnerUserId})
    |]

-- | Given a set of causals, import as many as possible from accessible releases.
importAccessibleCausals :: Set (Hash32) -> CodebaseM e [(CausalId, UserId)]
importAccessibleCausals causalHashes = do
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  results <-
    whenNonEmpty causalHashes $
      queryListRows @(CausalId, UserId)
        [sql|
            WITH causals_to_import(hash, causal_id) AS (
              SELECT causal_hashes.hash, causals.id FROM ^{singleColumnTable (Set.toList causalHashes)} AS causal_hashes(hash)
              -- We can only import causals that already exist in the shared codebase.
              JOIN causals ON causal_hashes.hash = causals.hash
              -- Ignore any causals that the codebase owner already has.
              WHERE NOT EXISTS (SELECT FROM causal_ownership co WHERE co.causal_id = causals.id AND co.user_id = #{codebaseOwnerUserId})
            ), all_accessible_projects(owner, project_id) AS (
              (SELECT private_project.owner_user_id AS owner, private_project.project_id
                FROM accessible_private_projects private_project
                  WHERE private_project.user_id = #{codebaseOwnerUserId}
              ) UNION ALL
              (SELECT public_project.owner_user_id AS owner, public_project.id AS project_id
                FROM projects public_project
                  WHERE NOT public_project.private
              )
            ), copyable_causals(causal_id, causal_hash, project_owner, created_at) AS (
                 SELECT cti.causal_id AS causal_id, cti.hash, project.owner, release.created_at
                   FROM causals_to_import cti
                     JOIN project_releases release ON release.squashed_causal_id = cti.causal_id
                     JOIN all_accessible_projects project ON project.project_id = release.project_id
                     -- This extra guarantee is required by the codebase migration from sqlite
                     -- to PG, but doesn't hurt to keep around.
                     JOIN causal_ownership ownership ON ownership.causal_id = cti.causal_id
                     WHERE ownership.user_id = project.owner
               UNION ALL
                 SELECT cti.causal_id AS causal_id, cti.hash, project.owner, branch.created_at
                   FROM causals_to_import cti
                     JOIN project_branches branch ON branch.causal_id = cti.causal_id
                     JOIN all_accessible_projects project ON project.project_id = branch.project_id
                     -- This extra guarantee is required by the codebase migration from sqlite
                     -- to PG, but doesn't hurt to keep around.
                     JOIN causal_ownership ownership ON ownership.causal_id = cti.causal_id
                     WHERE ownership.user_id = project.owner
            )
            -- Get only the first release (by created at) for each causal
            SELECT DISTINCT ON (copyable.causal_id) copyable.causal_id, copyable.project_owner
              FROM copyable_causals copyable
              -- Order by determines which values are returned by DISTINCT ON
              ORDER BY copyable.causal_id ASC, copyable.created_at DESC
        |]
  for_ results \case
    (causalId, owner) -> do
      execute_ [sql| SELECT copy_causal_into_codebase(#{causalId}, #{owner}, #{codebaseOwnerUserId}) |]
  pure results

-- | Find the best common ancestor between two causals for diffs or merges.
bestCommonAncestor :: (QueryM m) => CausalId -> CausalId -> m (Maybe CausalId)
bestCommonAncestor a b = do
  query1Col
    [sql| SELECT best_common_causal_ancestor(#{a}, #{b}) as causal_id
    |]

isFastForward :: (QueryM m) => CausalId -> CausalId -> m Bool
isFastForward fromCausalId toCausalId = do
  queryExpect1Col
    [sql|
  -- It's probably faster in _most_ situations to actually check FORWARDS from the old causal
  -- to find the new one, but in the worst case there are ANY number of descendents from a
  -- given causal so it's a bit riskier. Still probably fine to try if this gets slow
  -- though...
  WITH RECURSIVE causal_history(causal_id) AS (
      SELECT #{toCausalId}
      UNION
      SELECT ca.ancestor_id
      FROM causal_history h
          JOIN causal_ancestors ca ON h.causal_id = ca.causal_id
  ) SELECT EXISTS (
      SELECT FROM causal_history history
        WHERE history.causal_id = #{fromCausalId}
  );
  |]
