module Share.Postgres.NamesPerspective.Ops
  ( namesPerspectiveForRoot,
    namesPerspectiveForRootAndPath,
    NPQ.relocateNamesToMountsOf,
    NPQ.relocateReversedNamesToMountsOf,
  )
where

import Control.Comonad.Cofree qualified as Cofree
import Data.Map qualified as Map
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Queries qualified as NLQ
import Share.Postgres.NameLookups.Types
import Share.Postgres.NamesPerspective.Queries qualified as NPQ
import Share.Postgres.NamesPerspective.Types (MountTree, NamesPerspective (..))
import Share.Prelude
import UnliftIO.STM

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
namesPerspectiveForRoot :: forall m. (PG.QueryM m) => BranchHashId -> m (NamesPerspective m)
namesPerspectiveForRoot rootBranchHashId = do
  nameLookupReceipt <- NLOps.ensureNameLookupForBranchId rootBranchHashId
  mounts <- buildMountTree nameLookupReceipt rootBranchHashId
  let currentMount = ([], rootBranchHashId)
  let relativePerspective = mempty
  pure $
    NamesPerspective
      { mounts,
        currentMount,
        relativePerspective,
        nameLookupReceipt
      }

namesPerspectiveForRootAndPath :: forall m. (QueryM m) => BranchHashId -> PathSegments -> m (NamesPerspective m)
namesPerspectiveForRootAndPath rootBhId pathSegments = do
  rootNP@NamesPerspective {relativePerspective} <- namesPerspectiveForRoot rootBhId
  NPQ.resolvePathToMount rootNP (relativePerspective <> pathSegments)

-- | Build a 'MountTree' for the given root branch hash ID.
-- The MountTree is a tree of mounted namespace indexes, where each node is a branch hash ID of that namespace,
-- It uses caching to avoid redundant database queries.
buildMountTree :: forall m. (PG.QueryM m) => NameLookupReceipt -> BranchHashId -> m (MountTree m)
buildMountTree nameLookupReceipt rootBranchHashId = do
  cacheVar <- PG.transactionUnsafeIO $ newTVarIO mempty
  go cacheVar rootBranchHashId
  where
    go :: TVar (Map BranchHashId (MountTree m)) -> BranchHashId -> m (MountTree m)
    go cacheVar branchHashId = do
      cachedMounts <- PG.transactionUnsafeIO $ atomically $ do
        readTVar cacheVar
      case Map.lookup branchHashId cachedMounts of
        Just mountTree -> pure mountTree
        Nothing -> do
          mounts <- NLQ.listNameLookupMounts nameLookupReceipt branchHashId
          let mountTree :: Map PathSegments BranchHashId = Map.fromList mounts
          pure (branchHashId Cofree.:< Compose (go cacheVar <$> mountTree))
