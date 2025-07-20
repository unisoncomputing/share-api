-- | Algorithms related to names perspectives.
module Share.Postgres.NameLookups.NamesPerspective (relocateToMounts) where

import Share.Postgres.NameLookups.Types

-- | Resolve the root branch hash a given name is located within, and the name prefix the
-- mount is located at, as well as the name relative to that mount.
relocateToMounts :: (QueryM m) => MountTree m -> Traversal s t ReversedName (BranchHashId, ReversedName, PathSegments) -> s -> m t
relocateToMounts = _
