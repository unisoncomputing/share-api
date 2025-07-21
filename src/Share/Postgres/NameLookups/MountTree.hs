-- | Algorithms related to names perspectives.
module Share.Postgres.NameLookups.MountTree (relocateNamesToMountsOf) where

import Control.Comonad.Cofree qualified as Cofree
import Control.Lens
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Share.Postgres.NameLookups.Types
import Share.Prelude

-- | Resolve the root branch hash a given name is located within, and the name prefix the
-- mount is located at, as well as the name relative to that mount.
relocateNamesToMountsOf :: forall m s t. (Monad m) => NamesPerspective m -> Traversal s t ReversedName (NamesPerspective m, ReversedName) -> s -> m t
relocateNamesToMountsOf rootNamesPerspective@NamesPerspective {mounts} trav s =
  s
    & asListOf trav
      %%~ \reversedNames -> do
        let unreversedNames = reversedNames <&> \(ReversedName rev) -> NonEmpty.reverse rev
        for unreversedNames (resolveNameToMount mounts [])
          <&> fmap
            ( \(np, nameRelativeToMount) ->
                ( np,
                  ReversedName (NonEmpty.reverse nameRelativeToMount)
                )
            )
  where
    resolveNameToMount :: MountTree m -> [PathSegments] -> NonEmpty Text {- the non-reversed name -} -> m (NamesPerspective m, NonEmpty Text {- name relative to the returned mount, not-reversed -})
    resolveNameToMount (bhId Cofree.:< Compose mountTreeMap) reversedMountPrefix name = do
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
          resolveNameToMount nextMount (possibleMountPath : reversedMountPrefix) unMountedName
        _ ->
          -- No more mounts to check, the name must be in the current mount.
          pure (rootNamesPerspective {currentMount = (reverse $ reversedMountPrefix, bhId)}, name)
