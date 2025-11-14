module Share.Postgres.NamesPerspective.Types
  ( NamesPerspective (..),
    MountTree,
    currentMountTree,
    qualifyNameToPerspective,
    perspectiveRootBranchHashId,
    perspectiveCurrentMountPathPrefix,
    perspectiveCurrentMountBranchHashId,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Data.Map qualified as Map
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Types (NameLookupReceipt, PathSegments, ReversedName, prefixReversedName)
import Share.Prelude

-- | A tree of mounted namespaces, where each node is a branch hash ID of that namespace,
-- and the path segments to that namespace from the root.
type MountTree m = Cofree (Compose (Map PathSegments) m) BranchHashId

-- | Any time we need to lookup or search names we need to know what the scope of that search
-- should be. This can be complicated to keep track of, so this is a helper type to make it
-- easy to pass around.
--
-- You should use 'namesPerspectiveForRootAndPath' to construct this type.
--
-- E.g. if we're in loose code, we need to search the correct name lookup for the
-- user's perspective. If their perspective is "myprojects.json.latest.lib.base.data.List",
-- we need to search names using the name index mounted at "myprojects.json.latest.lib.base".
--
-- The NamesPerspective representing this viewpoint would be:
--
-- @@
-- NamesPerspective
--  { nameLookupBranchHashId = #libbasehash
--  , pathToMountedNameLookup = ["myprojects.json", "latest", "lib", "base"]
--  , relativePerspective = ["data", "List"]
--  }
-- @@
data NamesPerspective m = NamesPerspective
  { -- | The branch hash IDs of the root namespace and all the mounted namespaces
    -- within it, recursively.
    --
    -- The root of the MountTree contains the root branch hash id for the perspective.
    mounts :: MountTree m,
    -- | The path to the currently mounted namespace as a sequence of mount paths.
    currentMount :: ([PathSegments], BranchHashId),
    -- | The path to the perspective relative to the current name lookup
    relativePerspective :: PathSegments,
    nameLookupReceipt :: NameLookupReceipt
  }

instance Show (NamesPerspective m) where
  show NamesPerspective {currentMount, relativePerspective} =
    "(NamesPerspective {currentMount = " <> show currentMount <> ", relativePerspective = " <> show (relativePerspective) <> "})"

-- | Get the mount tree we're currently mounted at.
currentMountTree :: forall m. (HasCallStack, Monad m) => NamesPerspective m -> m (MountTree m)
currentMountTree NamesPerspective {mounts = mounts@(rootBranchHashId Cofree.:< _), currentMount = (mountPath, _)} =
  go mountPath mounts
  where
    go :: [PathSegments] -> MountTree m -> m (MountTree m)
    go [] tree = pure tree
    go (x : xs) (_ Cofree.:< (Compose mountMap)) =
      case Map.lookup x mountMap of
        Nothing ->
          error $
            "Mount path "
              <> show x
              <> " not found in root branch hash ID"
              <> show rootBranchHashId
              <> " with current mount "
              <> show mountPath
        Just nextMountsM ->
          nextMountsM >>= go xs

perspectiveRootBranchHashId :: NamesPerspective m -> BranchHashId
perspectiveRootBranchHashId NamesPerspective {mounts = root Cofree.:< _} = root

perspectiveCurrentMountBranchHashId :: NamesPerspective m -> BranchHashId
perspectiveCurrentMountBranchHashId NamesPerspective {currentMount = (_, branchHashId)} = branchHashId

perspectiveCurrentMountPathPrefix :: NamesPerspective m -> PathSegments
perspectiveCurrentMountPathPrefix NamesPerspective {currentMount = (mountPaths, _)} = fold mountPaths

qualifyNameToPerspective :: NamesPerspective m -> ReversedName -> ReversedName
qualifyNameToPerspective np =
  let mountPrefix = perspectiveCurrentMountPathPrefix np
      relativePrefix = relativePerspective np
   in -- This func is often partially applied, so the closure avoids re-computing the prefix over and over.
      \reversedName -> prefixReversedName (mountPrefix <> relativePrefix) reversedName
