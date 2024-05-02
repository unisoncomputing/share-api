module Enlil.Postgres.Patches.Types (PgPatch) where

import Enlil.Postgres.IDs
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull

type PgPatch = PatchFull.Patch' TextId ComponentHashId ComponentHashId
