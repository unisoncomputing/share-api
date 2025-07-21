{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Share.Postgres.NameLookups.Queries
  ( termNamesForRefsWithinNamespaceOf,
    typeNamesForRefsWithinNamespaceOf,
    ShouldSuffixify (..),
    termRefsForExactNamesOf,
    typeRefsForExactName,
    fuzzySearchTerms,
    fuzzySearchTypes,
    FuzzySearchScore,

    -- * Cursors
    projectTermsWithinRoot,
    projectTermsWithinRootV1,
    projectTypesWithinRoot,

    -- * Name lookup management
    listNameLookupMounts,
    checkBranchHashNameLookupExists,
    deleteNameLookupsExceptFor,
  )
where

import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding (from)
import Data.Foldable qualified as Foldable
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as Text
import Hasql.Decoders (Composite)
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Types
import Share.Postgres.Refs.Types (PGReference, PGReferent, referenceFields, referentFields)
import Share.Prelude
import Share.Utils.Postgres (ordered)
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (ConstructorType)
import U.Codebase.Referent qualified as V2
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Name (Name)
import Unison.Referent qualified as V1
import Unison.Util.Monoid qualified as Monoid

data ShouldSuffixify = Suffixify | NoSuffixify

-- | Get the list of term names and suffixifications for a given Referent within a given namespace.
-- Considers one level of dependencies, but not transitive dependencies.
--
-- If NoSuffixify is provided, the suffixified name will be the same as the fqn.
termNamesForRefsWithinNamespaceOf ::
  (PG.QueryM m) => NameLookupReceipt -> BranchHashId -> PathSegments -> Maybe ReversedName -> ShouldSuffixify -> Traversal s t PGReferent [NameWithSuffix] -> s -> m t
termNamesForRefsWithinNamespaceOf !_nameLookupReceipt bhId namespaceRoot maySuffix shouldSuffixify trav s = do
  s & asListOf trav \refs -> do
    let refsTable :: [(Int32, Maybe Text, Maybe ComponentHashId, Maybe Int64, Maybe Int64)]
        refsTable =
          zip [0 :: Int32 ..] refs <&> \(ord, ref) ->
            let (refBuiltin, refComponentHash, refComponentIndex, refConstructorIndex) = referentFields ref
             in (ord, refBuiltin, refComponentHash, refComponentIndex, refConstructorIndex)
    PG.queryListCol @[NameWithSuffix]
      [PG.sql|
        WITH refs(ord, referent_builtin, referent_component_hash_id, referent_component_index, referent_constructor_index) AS (
          SELECT * FROM ^{PG.toTable refsTable}
        )
        SELECT (
          -- Need COALESCE because array_agg will return NULL rather than the empty array
          -- if there are no results.
          SELECT COALESCE(array_agg((names.reversed_name, names.suffixified_name) ORDER BY length(names.reversed_name) ASC), '{}')
          FROM term_names_for_ref_within_namespace(
            #{bhId},
            #{namespacePrefix},
            #{reversedNamePrefix},
            #{shouldSuffixifyArg},
            refs.referent_builtin,
            refs.referent_component_hash_id,
            refs.referent_component_index,
            refs.referent_constructor_index
          ) AS names(reversed_name, suffixified_name)
        ) AS ref_names
        FROM refs
        ORDER BY refs.ord ASC
      |]
  where
    shouldSuffixifyArg = case shouldSuffixify of
      Suffixify -> True
      NoSuffixify -> False
    namespacePrefix = toNamespacePrefix namespaceRoot
    reversedNamePrefix = case maySuffix of
      Just suffix -> toReversedNamePrefix suffix
      Nothing -> ""

-- | Get the list of type names for a given Reference within a given namespace.
-- Considers one level of dependencies, but not transitive dependencies.
--
-- If NoSuffixify is provided, the suffixified name will be the same as the fqn.
typeNamesForRefsWithinNamespaceOf :: (PG.QueryM m) => NameLookupReceipt -> BranchHashId -> PathSegments -> Maybe ReversedName -> ShouldSuffixify -> Traversal s t PGReference [NameWithSuffix] -> s -> m t
typeNamesForRefsWithinNamespaceOf !_nameLookupReceipt bhId namespaceRoot maySuffix shouldSuffixify trav s = do
  s & asListOf trav \refs -> do
    let refsTable :: [(Int32, Maybe Text, Maybe ComponentHashId, Maybe Int64)]
        refsTable =
          zip [0 :: Int32 ..] refs <&> \(ord, ref) ->
            let (refBuiltin, refComponentHash, refComponentIndex) = referenceFields ref
             in (ord, refBuiltin, refComponentHash, refComponentIndex)
    PG.queryListCol @[NameWithSuffix]
      [PG.sql|
        WITH refs(ord, reference_builtin, reference_component_hash_id, reference_component_index) AS (
          SELECT * FROM ^{PG.toTable refsTable}
        )
        SELECT (
          -- Need COALESCE because array_agg will return NULL rather than the empty array
          -- if there are no results.
          SELECT COALESCE(array_agg((names.reversed_name, names.suffixified_name) ORDER BY length(names.reversed_name) ASC), '{}')
          FROM type_names_for_ref_within_namespace(
            #{bhId},
            #{namespacePrefix},
            #{reversedNamePrefix},
            #{shouldSuffixifyArg},
            refs.reference_builtin,
            refs.reference_component_hash_id,
            refs.reference_component_index
          ) AS names(reversed_name, suffixified_name)
        ) AS ref_names
        FROM refs
        ORDER BY refs.ord ASC
      |]
  where
    shouldSuffixifyArg = case shouldSuffixify of
      Suffixify -> True
      NoSuffixify -> False
    namespacePrefix = toNamespacePrefix namespaceRoot
    reversedNamePrefix = case maySuffix of
      Just suffix -> toReversedNamePrefix suffix
      Nothing -> ""

-- | Brings into scope the transitive_dependency_mounts CTE table, which contains all transitive deps of the given root, but does NOT include the direct dependencies.
-- @transitive_dependency_mounts(root_branch_hash_id, reversed_mount_path)@
-- Where @reversed_mount_path@ is the reversed path from the provided root to the mounted
-- dependency's root.
transitiveDependenciesSql :: BranchHashId -> PG.Sql
transitiveDependenciesSql rootBranchHashId =
  [PG.sql|
        -- Recursive table containing all transitive deps
        WITH RECURSIVE
          transitive_dependency_mounts(root_branch_hash_id, reversed_mount_path) AS (
            -- We've already searched direct deps above, so start with children of direct deps
            SELECT transitive.mounted_root_branch_hash_id, transitive.reversed_mount_path || direct.reversed_mount_path
            FROM name_lookup_mounts direct
                 JOIN name_lookup_mounts transitive on direct.mounted_root_branch_hash_id = transitive.parent_root_branch_hash_id
            WHERE direct.parent_root_branch_hash_id = #{rootBranchHashId}
            UNION ALL
            SELECT mount.mounted_root_branch_hash_id, mount.reversed_mount_path || rec.reversed_mount_path
            FROM name_lookup_mounts mount
              INNER JOIN transitive_dependency_mounts rec ON mount.parent_root_branch_hash_id = rec.root_branch_hash_id
          )
          |]

-- | Get the set of refs for an exact name.
termRefsForExactNamesOf :: (PG.QueryM m) => NamesPerspective m -> Traversal s t ReversedName [NamedRef (PGReferent, Maybe ConstructorType)] -> s -> m t
termRefsForExactNamesOf np trav s = do
  s
    & asListOf trav %%~ \fqns -> do
      relocatedNames <- relocateToMounts np traversed fqns
      let mountPaths = relocatedNames <&> \(_, mountPath, _) -> mountPath
      let scopedNamesTable =
            ordered relocatedNames
              <&> \(ord, (mountRoot, _mountPath, scopedReversedName)) ->
                (ord, mountRoot, scopedReversedName)
      results :: [[CompositeRow (NamedRef (CompositeRow PGReferent, (Maybe ConstructorType)))]] <-
        PG.queryListCol
          [PG.sql|
          WITH scoped_names(ord, mount_branch_hash_id, reversed_name) AS (
            SELECT * FROM ^{PG.toTable scopedNamesTable} AS t(ord, mount_branch_hash_id, reversed_name)
          )
          SELECT (
            SELECT COALESCE(array_agg((stnl.reversed_name, stnl.referent_builtin, stnl.referent_component_hash_id, stnl.referent_component_index, stnl.referent_constructor_index, stnl.referent_constructor_type)), '{}')
            FROM scoped_term_name_lookup stnl
            WHERE stnl.root_branch_hash_id = scoped_names.mount_branch_hash_id
                  AND stnl.reversed_name = scoped_names.reversed_name
          ) FROM scoped_names
          ORDER BY scoped_names.ord ASC
        |]
      results
        & coerce @[[CompositeRow (NamedRef (CompositeRow PGReferent, (Maybe ConstructorType)))]] @[[(NamedRef (PGReferent, (Maybe ConstructorType)))]]
        & zipWith (\mp namedRefs -> prefixNamedRef mp <$> namedRefs) mountPaths
        & pure

-- | Get the set of refs for an exact name.
-- This will only return results which are within the name lookup for the provided branch hash
-- id. It's the caller's job to select the correct name lookup for your exact name.
--
-- See termRefsForExactName in U.Codebase.Sqlite.Operations
typeRefsForExactName :: (PG.QueryM m) => NameLookupReceipt -> BranchHashId -> ReversedName -> m [NamedRef PGReference]
typeRefsForExactName !_nameLookupReceipt bhId reversedName = do
  PG.queryListRows
    [PG.sql|
      SELECT reversed_name, reference_builtin, reference_component_hash_id, reference_component_index
      FROM scoped_type_name_lookup
      WHERE root_branch_hash_id = #{bhId}
            AND reversed_name = #{reversedName}
    |]

-- | Check if we've already got an index for the desired root branch hash.
checkBranchHashNameLookupExists :: (PG.QueryM m) => BranchHashId -> m Bool
checkBranchHashNameLookupExists hashId = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT EXISTS (
        SELECT 1
        FROM name_lookups
        WHERE root_branch_hash_id = #{hashId}
        LIMIT 1
      )
    |]

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: [BranchHashId] -> PG.Transaction e ()
deleteNameLookupsExceptFor hashIds = do
  case hashIds of
    [] -> PG.execute_ [PG.sql| DELETE FROM name_lookups |]
    _ -> do
      let hashIdRows = PG.Only <$> hashIds
      PG.execute_
        [PG.sql|
          WITH RECURSIVE reachable(branch_hash_id) AS (
            SELECT * FROM ^{PG.toTable hashIdRows}
            -- Any name lookup that's mounted on a reachable name lookup is also reachable
            UNION ALL
            SELECT mounted_root_branch_hash_id FROM name_lookup_mounts JOIN reachable ON branch_hash_id = parent_root_branch_hash_id
          )
          DELETE FROM name_lookups
            WHERE root_branch_hash_id NOT IN (SELECT branch_hash_id FROM reachable);
        |]

-- | Fetch the name lookup mounts for a given name lookup index.
listNameLookupMounts :: (PG.QueryM m) => NameLookupReceipt -> BranchHashId -> m [(PathSegments, BranchHashId)]
listNameLookupMounts !_nameLookupReceipt rootBranchHashId =
  do
    PG.queryListRows
      [PG.sql|
        SELECT mount_path, mounted_root_branch_hash_id
        FROM name_lookup_mounts
        WHERE parent_root_branch_hash_id = #{rootBranchHashId}
      |]
    <&> fmap
      \(mountPathText, mountedRootBranchHashId) ->
        let mountPath = textToPathSegments (Text.init mountPathText)
         in (mountPath, mountedRootBranchHashId)

-- | Larger is better.
data FuzzySearchScore
  = FuzzySearchScore
  { exactLastSegmentMatch :: Bool,
    lastSegmentInfixMatch :: Bool,
    lastSegmentMatchPos :: Int64,
    inverseNameLength :: Int64
  }
  deriving (Show, Eq)

instance Ord FuzzySearchScore where
  compare (FuzzySearchScore exact1 infix1 pos1 len1) (FuzzySearchScore exact2 infix2 pos2 len2) =
    exact1 `compare` exact2
      <> infix1 `compare` infix2
      <> pos1 `compare` pos2
      <> len1 `compare` len2

instance DecodeRow FuzzySearchScore where
  decodeRow =
    FuzzySearchScore
      <$> PG.decodeField
      <*> PG.decodeField
      <*> PG.decodeField
      <*> PG.decodeField

-- | Searches for all names within the given name lookup which contain the provided list of segments
-- in order.
-- Search is case insensitive.
fuzzySearchTerms :: (PG.QueryA m) => NameLookupReceipt -> Bool -> BranchHashId -> Int64 -> PathSegments -> NonEmpty Text -> Text -> m [(FuzzySearchScore, NamedRef (PGReferent, Maybe ConstructorType))]
fuzzySearchTerms !_nameLookupReceipt includeDependencies bhId limit namespace querySegments lastSearchTerm = do
  fmap unRow
    <$> PG.queryListRows
      [PG.sql|
      SELECT matches.reversed_name, matches.referent_builtin, matches.referent_component_hash_id, matches.referent_component_index, matches.referent_constructor_index, matches.referent_constructor_type,
      matches.exact_last_segment_match, matches.last_segment_infix_match, matches.last_segment_match_pos, matches.inverse_name_length
      FROM (
        SELECT reversed_name, referent_builtin, referent_component_hash_id, referent_component_index, referent_constructor_index, referent_constructor_type, last_name_segment,
              (last_name_segment = #{lastSearchTerm}) AS exact_last_segment_match,
              (last_name_segment ILIKE ('%' || like_escape(#{lastSearchTerm}) || '%')) AS last_segment_infix_match,
              (-POSITION(#{lastSearchTerm} IN last_name_segment)) AS last_segment_match_pos,
              (-length(reversed_name)) AS inverse_name_length
          FROM scoped_term_name_lookup
        WHERE
          root_branch_hash_id = #{bhId}
          AND namespace LIKE like_escape(#{namespacePrefix}) || '%'
          AND (namespace || last_name_segment) ILIKE #{preparedQuery}
        ^{Monoid.whenM includeDependencies dependenciesSql}
      ) as matches
      -- Exact last-segment matches first, then last-segment infix matches sorting prefix
      -- matches first, then prefer shorter names.
      ORDER BY (last_name_segment = #{lastSearchTerm},
               (#{lastSearchTerm} ILIKE ('%' || like_escape(last_name_segment) || '%')),
               (-POSITION(#{lastSearchTerm} IN last_name_segment)),
               (-length(reversed_name))
               ) DESC
      LIMIT #{limit}
            |]
  where
    namespacePrefix = toNamespacePrefix namespace
    -- Union in the dependencies if required.
    dependenciesSql =
      [PG.sql|
      UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, referent_builtin, referent_component_hash_id, referent_component_index, referent_constructor_index, referent_constructor_type, last_name_segment,
             (last_name_segment = #{lastSearchTerm}) AS exact_last_segment_match,
             (last_name_segment ILIKE ('%' || like_escape(#{lastSearchTerm}) || '%')) AS last_segment_infix_match,
             (-POSITION(#{lastSearchTerm} IN last_name_segment)) AS last_segment_match_pos,
             (-length(reversed_name)) AS inverse_name_length
        FROM name_lookup_mounts mount
          INNER JOIN scoped_term_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE
          mount.parent_root_branch_hash_id = #{bhId}
          -- We have a pre-condition that the namespace must not be within any of the mounts,
          -- so this is sufficient to determine whether the entire sub-index is within the
          -- required namespace prefix.
          AND mount.mount_path LIKE like_escape(#{namespacePrefix}) || '%'
          AND (mount.mount_path || namespace || last_name_segment) ILIKE #{preparedQuery}
          |]
    preparedQuery = prepareFuzzyQuery querySegments
    unRow :: (NamedRef (PGReferent PG.:. PG.Only (Maybe ConstructorType)) PG.:. FuzzySearchScore) -> (FuzzySearchScore, NamedRef (PGReferent, Maybe ConstructorType))
    unRow (namedRef PG.:. score) =
      (score, namedRef <&> \(a PG.:. PG.Only b) -> (a, b))

-- | Searches for all names within the given name lookup which contain the provided list of segments
-- in order.
--
-- Search is case insensitive.
fuzzySearchTypes :: (PG.QueryA m) => NameLookupReceipt -> Bool -> BranchHashId -> Int64 -> PathSegments -> NonEmpty Text -> Text -> m [(FuzzySearchScore, NamedRef PGReference)]
fuzzySearchTypes !_nameLookupReceipt includeDependencies bhId limit namespace querySegments lastSearchTerm = do
  fmap unRow
    <$> PG.queryListRows
      [PG.sql|

      SELECT matches.reversed_name, matches.reference_builtin, matches.reference_component_hash_id, matches.reference_component_index,
      matches.exact_last_segment_match, matches.last_segment_infix_match, matches.last_segment_match_pos, matches.inverse_name_length
      FROM (
        SELECT reversed_name, reference_builtin, reference_component_hash_id, reference_component_index, last_name_segment,
             (last_name_segment = #{lastSearchTerm}) AS exact_last_segment_match,
             (last_name_segment ILIKE ('%' || like_escape(#{lastSearchTerm}) || '%')) AS last_segment_infix_match,
             (-POSITION(#{lastSearchTerm} IN last_name_segment)) AS last_segment_match_pos,
             (-length(reversed_name)) AS inverse_name_length
        FROM scoped_type_name_lookup
      WHERE
        root_branch_hash_id = #{bhId}
        AND namespace LIKE like_escape(#{namespacePrefix}) || '%'
        AND (namespace || last_name_segment) ILIKE #{preparedQuery}
      ^{Monoid.whenM includeDependencies dependenciesSql}
      ) as matches
      -- Exact last-segment matches first, then last-segment prefix matches, then prefer
      -- shorter names.
      ORDER BY (last_name_segment = #{lastSearchTerm},
               (#{lastSearchTerm} ILIKE ('%' || like_escape(last_name_segment) || '%')),
               (-POSITION(#{lastSearchTerm} IN last_name_segment)),
               (-length(reversed_name))
               ) DESC
      LIMIT #{limit}
          |]
  where
    unRow :: (NamedRef PGReference PG.:. FuzzySearchScore) -> (FuzzySearchScore, NamedRef PGReference)
    unRow (namedRef PG.:. score) = (score, namedRef)
    namespacePrefix = toNamespacePrefix namespace
    -- Union in the dependencies if required.
    dependenciesSql =
      [PG.sql|
      UNION ALL
        SELECT (names.reversed_name || mount.reversed_mount_path) AS reversed_name, reference_builtin, reference_component_hash_id, reference_component_index, last_name_segment,
             (last_name_segment = #{lastSearchTerm}) AS exact_last_segment_match,
             (last_name_segment ILIKE ('%' || like_escape('%' || #{lastSearchTerm}) || '%')) AS last_segment_infix_match,
             (-POSITION(#{lastSearchTerm} IN last_name_segment)) AS last_segment_match_pos,
             (-length(reversed_name)) AS inverse_name_length
        FROM name_lookup_mounts mount
          INNER JOIN scoped_type_name_lookup names ON names.root_branch_hash_id = mount.mounted_root_branch_hash_id
        WHERE
          mount.parent_root_branch_hash_id = #{bhId}
          -- We have a pre-condition that the namespace must not be within any of the mounts,
          -- so this is sufficient to determine whether the entire sub-index is within the
          -- required namespace prefix.
          AND mount.mount_path LIKE like_escape(#{namespacePrefix}) || '%'
          AND (mount.mount_path || namespace || last_name_segment) ILIKE #{preparedQuery}
          |]
    preparedQuery = prepareFuzzyQuery querySegments

-- | >>> prepareFuzzyQuery ["foo", "bar"]
-- "%foo%bar%"
--
-- >>> prepareFuzzyQuery ["foo", "", "bar"]
-- "%foo%bar%"
--
-- >>> prepareFuzzyQuery ["foo%", "bar "]
-- "%foo\\%%bar%"
prepareFuzzyQuery :: NonEmpty Text -> Text
prepareFuzzyQuery query =
  query
    & Foldable.toList
    & filter (not . Text.null)
    & map (PG.likeEscape . Text.strip)
    & \q -> "%" <> Text.intercalate "%" q <> "%"

-- | Convert a namespace into the appropriate prefix for searching within that namespace
-- Use this instead of toNamespacePattern if you're using a query or function which
-- builds its own LIKE patterns from a prefix.
--
-- >>> toNamespacePrefix $ PathSegments ["foo", "bar"]
-- "foo.bar."
--
-- >>> toNamespacePrefix $ PathSegments []
-- ""
toNamespacePrefix :: PathSegments -> Text
toNamespacePrefix = \case
  PathSegments [] -> ""
  namespace -> pathSegmentsToText namespace <> "."

-- | Convert reversed name segments into a string suitable for prefix searching.
-- Use this instead of toSuffixPattern if you're using a query or function which
-- builds its own LIKE patterns from a prefix.
--
-- >>> toReversedNamePrefix (ReversedName ("foo" NonEmpty.:| ["bar"]))
-- "foo.bar."
toReversedNamePrefix :: ReversedName -> Text
toReversedNamePrefix suffix = Text.intercalate "." (into @[Text] suffix) <> "."

-- | Get a cursor over all non-lib terms within the given root branch.
projectTermsWithinRootV1 :: (QueryM m) => NameLookupReceipt -> BranchHashId -> m (PGCursor (Name, V1.Referent))
projectTermsWithinRootV1 !_nlReceipt bhId = do
  Cursors.newRowCursor @(NamedRef (V2.Referent PG.:. PG.Only (Maybe ConstructorType)))
    "termsForSearchSyncCursor"
    [sql|
        SELECT reversed_name, referent_builtin, referent_component_hash.base32, referent_component_index, referent_constructor_index, referent_constructor_type
        FROM scoped_term_name_lookup
        LEFT JOIN component_hashes referent_component_hash ON referent_component_hash.id = referent_component_hash_id
        WHERE root_branch_hash_id = #{bhId}
    |]
    <&> fmap
      ( \NamedRef {reversedSegments, ref} -> (reversedNameToName reversedSegments, referent2to1 ref)
      )

-- | Get a cursor over all non-lib terms within the given root branch.
projectTermsWithinRoot :: (QueryM m) => NameLookupReceipt -> BranchHashId -> m (PGCursor (Name, V2.Referent))
projectTermsWithinRoot !nlr bhId = projectTermsWithinRootV1 nlr bhId <&> fmap (over _2 Cv.referent1to2)

referent2to1 :: (HasCallStack) => (V2.Referent PG.:. PG.Only (Maybe V2.ConstructorType)) -> V1.Referent
referent2to1 (r PG.:. PG.Only mayCT) = Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") mayCT) r

-- | Get a cursor over all non-lib types within the given root branch.
projectTypesWithinRoot :: (QueryM m) => NameLookupReceipt -> BranchHashId -> m (PGCursor (Name, Reference))
projectTypesWithinRoot !_nlReceipt bhId = do
  Cursors.newRowCursor @(NamedRef Reference)
    "typesForSearchSyncCursor"
    [sql|
        SELECT reversed_name, reference_builtin, reference_component_hash.base32, reference_component_index
        FROM scoped_type_name_lookup
        LEFT JOIN component_hashes reference_component_hash ON reference_component_hash.id = reference_component_hash_id
        WHERE root_branch_hash_id = #{bhId}
    |]
    <&> fmap (\NamedRef {reversedSegments, ref} -> (reversedNameToName reversedSegments, ref))

-- | Resolve the root branch hash a given name is located within, and the name prefix the
-- mount is located at, as well as the name relative to that mount.
relocateNamesToMountsOf :: (QueryM m) => NamesPerspective m -> Traversal s t ReversedName (BranchHashId, PathSegments, ReversedName) -> s -> m t
relocateNamesToMountsOf NamesPerspective {mounts} trav s =
  s
    & asListOf trav %%~ \reversedNames -> do
      let unreversedNames = reversedNames <&> \(ReversedName rev) -> NonEmpty.reverse rev
      for unreversedNames (resolveNameToMount mounts)
        <&> fmap
          ( \(branchHashId, mountPath, nameRelativeToMount) ->
              ( branchHashId,
                mountPath,
                ReversedName (NonEmpty.reverse nameRelativeToMount)
              )
          )
  where
    resolveNameToMount :: MountTree m -> NonEmpty Text {- the non-reversed name -} -> m (BranchHashId {- branch hash of the mount -}, PathSegments {- Path to the mount -}, NonEmpty Text {- name relative to the returned mount, not-reversed -})
    resolveNameToMount (bhId Cofree.:< Compose mountTreeMap) name = do
      -- Split each name into its possible mount path and the rest of the name.
      -- If the name is in a mount, the mount path will always be the first two segments of
      -- the non-reversed name, e.g. lib.base.data.List -> (lib.base, data.List)
      let (possibleMountPath, remainingName) = first PathSegments $ NonEmpty.splitAt 2 name
      case (NonEmpty.nonEmpty remainingName, Map.lookup possibleMountPath mountTreeMap) of
        (Just unMountedName, Just nextMountsM) -> do
          -- This only does a query the first time we access this mount, then it's cached
          -- automatically in the mount tree.
          nextMount <- nextMountsM
          -- Recursively get the mount for the next segment of the name.
          resolveNameToMount nextMount unMountedName
            <&> \(resultBhId, mountPrefix, nameRelativeToMount) -> do
              (resultBhId, possibleMountPath <> mountPrefix, nameRelativeToMount)
        _ ->
          -- No more mounts to check, the name must be in the current mount.
          pure (bhId, mempty, name)
