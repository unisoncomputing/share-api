module Share.Postgres.NamesPerspective.Queries
  ( relocateReversedNamesToMountsOf,
    relocateNamesToMountsOf,
    resolvePathToMount,
  )
where

import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding (from)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Share.Postgres
import Share.Postgres.NameLookups.Types
import Share.Postgres.NamesPerspective.Types
import Share.Prelude
import Unison.Name (Name)

-- | Resolve the given name within its root branch to it's smallest containing 'mount'
-- perspective.
--
-- If the name is in a mount, the resulting names perspective will be located at that mount
-- with an empty relativePerspective. If the name is in the same mount as the initial
-- perspective then relativePerspective will remain unchanged.
relocateReversedNamesToMountsOf :: forall m s t. (QueryM m) => NamesPerspective m -> Traversal s t ReversedName (NamesPerspective m, ReversedName) -> s -> m t
relocateReversedNamesToMountsOf rootNamesPerspective trav s = do
  s
    & asListOf trav
      %%~ \reversedNames -> do
        for reversedNames \(ReversedName revFqn) ->
          do
            let (lastNameSegment :| revNamePath) = revFqn
            np@NamesPerspective {relativePerspective = PathSegments relativePath} <- resolvePathToMount rootNamesPerspective (PathSegments (reverse revNamePath))
            pure
              ( np {relativePerspective = mempty},
                ReversedName (lastNameSegment NonEmpty.:| reverse relativePath)
              )

relocateNamesToMountsOf :: forall m s t. (QueryM m) => NamesPerspective m -> Traversal s t Name (NamesPerspective m, Name) -> s -> m t
relocateNamesToMountsOf np t s =
  s
    & asListOf t
      %%~ \names -> do
        let reversedNames = names <&> nameToReversedName
        relocated <- relocateReversedNamesToMountsOf np traversed reversedNames
        pure $ relocated <&> \(np', rev) -> (np', reversedNameToName rev)

-- | Relocate a given perspective to be at the provided path, which is relative to the
-- provided perspective.
--
-- >>> import Share.Postgres.IDs
-- >>> import Control.Comonad.Cofree qualified as Cofree
-- >>> mountTree = BranchHashId 1 Cofree.:< (Compose $ Map.fromList [ (PathSegments ["lib", "base"], Identity (BranchHashId 2 Cofree.:< (Compose $ Map.fromList [(PathSegments ["data", "List"], Identity (BranchHashId 3 Cofree.:< Compose mempty))])))])
-- >>> resolvePathToMount (NamesPerspective{mounts=mountTree, currentMount=(mempty, BranchHashId 1), relativePerspective=mempty, nameLookupReceipt=undefined}) (PathSegments ["lib", "base", "data", "List"])
-- Identity (NamesPerspective {currentMount = ([PathSegments ["lib","base"],PathSegments ["data","List"]],BranchHashId {unBranchHashId = 3}), relativePerspective = PathSegments []})
resolvePathToMount :: forall m. (Monad m) => NamesPerspective m -> PathSegments -> m (NamesPerspective m)
resolvePathToMount rootNP path = do
  mountTree <- currentMountTree rootNP
  let (mountPrefix, _bhId) = currentMount rootNP
  let pathPrefix = relativePerspective rootNP
  go mountTree (reverse mountPrefix) (pathPrefix <> path)
  where
    go :: MountTree m -> [PathSegments] -> PathSegments -> m (NamesPerspective m)
    go (bhId Cofree.:< Compose mountTreeMap) reversedMountPrefix (PathSegments path) = do
      case NonEmpty.nonEmpty path of
        Nothing ->
          -- If we have no more segments to check, we can return the current mount.
          pure (rootNP {currentMount = (reverse $ reversedMountPrefix, bhId), relativePerspective = mempty})
        Just nePath -> do
          -- Split the path into the first two segments and the remaining path.
          -- If the path is in a mount, the mount path will always be the first two segments
          -- e.g. lib.base.data.List -> (lib.base, data.List)
          let (possibleMountPath, remainingPath) = bothMap PathSegments $ NonEmpty.splitAt 2 nePath
          case Map.lookup possibleMountPath mountTreeMap of
            Just nextMountsM -> do
              -- This only does a query the first time we access this mount, then it's cached
              -- automatically in the mount tree.
              nextMount <- nextMountsM
              -- Recursively get the mount for the next segment of the name.
              go nextMount (possibleMountPath : reversedMountPrefix) remainingPath
            Nothing -> do
              -- No mount, the path must be in the current mount.
              pure (rootNP {currentMount = (reverse $ reversedMountPrefix, bhId), relativePerspective = PathSegments path})
