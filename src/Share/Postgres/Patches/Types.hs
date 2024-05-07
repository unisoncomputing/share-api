module Share.Postgres.Patches.Types (PgPatch) where

import Share.Postgres.IDs
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull

type PgPatch = PatchFull.Patch' TextId ComponentHashId ComponentHashId
