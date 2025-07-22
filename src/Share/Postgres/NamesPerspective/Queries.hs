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

-- | Resolve the root branch hash a given name is located within, and the name prefix the
-- mount is located at, as well as the name relative to that mount.
relocateReversedNamesToMountsOf :: forall m s t. (QueryM m) => NamesPerspective m -> Traversal s t ReversedName (NamesPerspective m, ReversedName) -> s -> m t
relocateReversedNamesToMountsOf rootNamesPerspective@NamesPerspective {relativePerspective = namespacePrefix} trav s = do
  mountTree <- currentMountTree rootNamesPerspective
  s
    & asListOf trav
      %%~ \reversedNames -> do
        for reversedNames \(ReversedName revFqn) ->
          do
            let (lastNameSegment :| revNamePath) = revFqn
            np@NamesPerspective {relativePerspective} <- resolvePathToMount rootNamesPerspective mountTree [] (namespacePrefix <> PathSegments (reverse revNamePath))
            let (PathSegments relativePath) = relativePerspective
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

resolvePathToMount :: (QueryM m) => NamesPerspective m -> MountTree m -> [PathSegments] -> PathSegments -> m (NamesPerspective m)
resolvePathToMount rootNP (bhId Cofree.:< Compose mountTreeMap) reversedMountPrefix (PathSegments path) = do
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
          resolvePathToMount rootNP nextMount (possibleMountPath : reversedMountPrefix) remainingPath
        Nothing -> do
          -- No mount, the path must be in the current mount.
          pure (rootNP {currentMount = (reverse $ reversedMountPrefix, bhId), relativePerspective = PathSegments path})
