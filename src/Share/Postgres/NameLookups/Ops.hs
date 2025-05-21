module Share.Postgres.NameLookups.Ops
  ( namesPerspectiveForRootAndPath,
    relocateToNameRoot,
    fuzzySearchDefinitions,
    termNamesForRefWithinNamespace,
    typeNamesForRefWithinNamespace,
    termRefsForExactName,
    typeRefsForExactName,
    checkBranchHashNameLookupExists,
    deleteNameLookupsExceptFor,
    ensureNameLookupForBranchId,
    Q.projectTermsWithinRoot,
    Q.projectTypesWithinRoot,
    Q.listNameLookupMounts,
    projectNamesWithoutLib,
  )
where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Share.Postgres (QueryA, QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursor
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Conversions qualified as CV
import Share.Postgres.NameLookups.Queries qualified as NameLookupQ
import Share.Postgres.NameLookups.Queries qualified as Q
import Share.Postgres.NameLookups.Types
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Postgres.Refs.Types
import Share.Prelude
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (ConstructorType, Referent)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Reference qualified as V1
import Unison.Referent qualified as V1
import Unison.Util.List qualified as List

-- | Determine which nameLookup is the closest parent of the provided perspective.
--
-- Returns (rootBranchId of the closest parent index, namespace that index is mounted at, location of the perspective within the mounted namespace)
--
-- E.g.
-- If your namespace is "lib.distributed.lib.base.data.List", you'd get back
-- (rootBranchId of the lib.distributed.lib.base name lookup, "lib.distributed.lib.base", "data.List")
--
-- Or if your namespace is "subnamespace.user", you'd get back
-- (the rootBranchId you provided, "", "subnamespace.user")
namesPerspectiveForRootAndPath :: forall m. (PG.QueryM m) => BranchHashId -> PathSegments -> m NamesPerspective
namesPerspectiveForRootAndPath rootBranchHashId namespace = do
  nameLookupReceipt <- ensureNameLookupForBranchId rootBranchHashId
  namesPerspectiveForRootAndPathHelper nameLookupReceipt rootBranchHashId namespace
  where
    namesPerspectiveForRootAndPathHelper :: NameLookupReceipt -> BranchHashId -> PathSegments -> m NamesPerspective
    namesPerspectiveForRootAndPathHelper nameLookupReceipt rootBhId pathSegments = do
      let defaultPerspective =
            NamesPerspective
              { nameLookupBranchHashId = rootBhId,
                pathToMountedNameLookup = (PathSegments []),
                relativePerspective = pathSegments,
                nameLookupReceipt
              }
      fmap (fromMaybe defaultPerspective) . runMaybeT $
        do
          mounts <- lift $ NameLookupQ.listNameLookupMounts nameLookupReceipt rootBhId
          mounts
            & altMap \(mountPathSegments, mountBranchHash) -> do
              case List.splitOnLongestCommonPrefix (into @[Text] pathSegments) (into @[Text] mountPathSegments) of
                -- The path is within this mount:
                (_, remainingPath, []) ->
                  lift $
                    namesPerspectiveForRootAndPathHelper nameLookupReceipt mountBranchHash (into @PathSegments remainingPath)
                      <&> \(NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup = mountLocation, relativePerspective, nameLookupReceipt}) ->
                        NamesPerspective
                          { nameLookupBranchHashId,
                            -- Ensure we return the correct mount location even if the mount is
                            -- several levels deep
                            pathToMountedNameLookup = mountPathSegments <> mountLocation,
                            relativePerspective,
                            nameLookupReceipt
                          }
                -- The path is not within this mount:
                _ -> empty

-- | Given an arbitrary query and perspective, find the name root the query belongs in,
-- then return that root and the query relocated to that root.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ or @.myproject.lib.base.List -> .myproject.lib.base@
relocateToNameRoot :: (PG.QueryM m, Traversable hq) => Path -> hq Name -> BranchHashId -> m (NamesPerspective, hq Name)
relocateToNameRoot perspective query rootBh = do
  -- The namespace containing the name path
  let nameLocation = case getFirst query of
        Just name ->
          name
            & Name.segments
            & NonEmpty.init
            & Path.fromList
        Nothing -> mempty
  let fullPath = perspective <> nameLocation
  namesPerspective@NamesPerspective {relativePerspective} <- namesPerspectiveForRootAndPath rootBh (PathSegments . fmap NameSegment.toUnescapedText . Path.toList $ fullPath)
  let reprefixName name = Name.fromReverseSegments $ (NonEmpty.head $ Name.reverseSegments name) NonEmpty.:| (reverse $ coerce relativePerspective)
  pure (namesPerspective, reprefixName <$> query)

-- | Search for term or type names which contain the provided list of segments in order.
-- Search is case insensitive.
fuzzySearchDefinitions ::
  (PG.QueryM m) =>
  Bool ->
  NamesPerspective ->
  -- | Will return at most n terms and n types; i.e. max number of results is 2n
  Int ->
  NonEmpty Text ->
  Text ->
  m ([(Q.FuzzySearchScore, NameLookups.NamedRef (Referent, Maybe ConstructorType))], [(Q.FuzzySearchScore, NamedRef Reference)])
fuzzySearchDefinitions includeDependencies NamesPerspective {nameLookupBranchHashId, relativePerspective, nameLookupReceipt} limit querySegments lastQuerySegment = do
  pgTermNames <-
    Q.fuzzySearchTerms nameLookupReceipt includeDependencies nameLookupBranchHashId (into @Int64 limit) relativePerspective querySegments lastQuerySegment
      <&> fmap \termName ->
        termName
          & second (stripPrefixFromNamedRef relativePerspective)
  pgTypeNames <-
    Q.fuzzySearchTypes nameLookupReceipt includeDependencies nameLookupBranchHashId (into @Int64 limit) relativePerspective querySegments lastQuerySegment
      <&> fmap \typeName ->
        typeName
          & second (stripPrefixFromNamedRef relativePerspective)

  termNames <- pgTermNames & (traversed . _2 . traversed . _1) %%~ CV.referentPGTo2
  typeNames <- pgTypeNames & (traversed . _2 . traversed) %%~ CV.referencePGTo2
  pure (termNames, typeNames)

-- | Get the list of (fqn, suffixified) names for a given Referent.
termNamesForRefWithinNamespace :: (PG.QueryM m) => NamesPerspective -> PGReferent -> Maybe ReversedName -> m [(ReversedName {- fqn -}, ReversedName {- suffixified -})]
termNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup, nameLookupReceipt} ref maySuffix = do
  NameLookupQ.termNamesForRefWithinNamespace nameLookupReceipt nameLookupBranchHashId mempty ref maySuffix
    <&> fmap (first $ prefixReversedName pathToMountedNameLookup)

-- | Get the list of (fqn, suffixified) names for a given Reference.
typeNamesForRefWithinNamespace :: (PG.QueryM m) => NamesPerspective -> PGReference -> Maybe ReversedName -> m [(ReversedName {- fqn -}, ReversedName {- suffixified -})]
typeNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup, nameLookupReceipt} ref maySuffix = do
  NameLookupQ.typeNamesForRefWithinNamespace nameLookupReceipt nameLookupBranchHashId mempty ref maySuffix
    <&> fmap (first $ prefixReversedName pathToMountedNameLookup)

-- | Helper for findings refs by name within the correct mounted indexes.
refsForExactName ::
  (PG.QueryM m) =>
  (NameLookupReceipt -> BranchHashId -> ReversedName -> m [NamedRef ref]) ->
  NamesPerspective ->
  ReversedName ->
  m [NamedRef ref]
refsForExactName query NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup, nameLookupReceipt} name = do
  namedRefs <- query nameLookupReceipt nameLookupBranchHashId name
  pure $
    namedRefs
      <&> prefixNamedRef pathToMountedNameLookup

termRefsForExactName :: (PG.QueryM m) => NamesPerspective -> ReversedName -> m [NamedRef V1.Referent]
termRefsForExactName namesPerspective reversedName = do
  refsForExactName NameLookupQ.termRefsForExactName namesPerspective reversedName
    >>= traverse (traverse (CV.referentPGTo1UsingCT))

typeRefsForExactName :: (PG.QueryM m) => NamesPerspective -> ReversedName -> m [NamedRef V1.Reference]
typeRefsForExactName namesPerspective reversedName = do
  refsForExactName NameLookupQ.typeRefsForExactName namesPerspective reversedName
    >>= (traverse . traverse) CV.referencePGTo1

-- | Check whether we've already got an index for a given branch hash.
checkBranchHashNameLookupExists :: (PG.QueryM m) => BranchHash -> m Bool
checkBranchHashNameLookupExists bh = do
  bhId <- HashQ.ensureBranchHashId bh
  Q.checkBranchHashNameLookupExists bhId

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: Set BranchHash -> PG.Transaction e ()
deleteNameLookupsExceptFor reachable = do
  bhIds <- for (Set.toList reachable) HashQ.ensureBranchHashId
  Q.deleteNameLookupsExceptFor bhIds

ensureNameLookupForBranchId :: (QueryA m) => BranchHashId -> m NameLookupReceipt
ensureNameLookupForBranchId branchHashId =
  UnsafeNameLookupReceipt
    <$ PG.execute_ [PG.sql| SELECT ensure_name_lookup(#{branchHashId}) |]

-- | Build a 'Names' for all definitions within the given root, without any dependencies.
-- Note: This loads everything into memory at once, so avoid this and prefer streaming when possible.
projectNamesWithoutLib :: (QueryM m) => NameLookupReceipt -> BranchHashId -> m Names
projectNamesWithoutLib !nlr rootBranchHashId = do
  termNamesCursor <- Q.projectTermsWithinRootV1 nlr rootBranchHashId
  allTerms <- Cursor.foldBatched termNamesCursor 1000 (pure . toList)
  typesCursor <- (Q.projectTypesWithinRoot nlr rootBranchHashId)
  allTypes <- Cursor.foldBatched typesCursor 1000 (pure . toList)
  pure $ Names.fromTermsAndTypes allTerms allTypes
