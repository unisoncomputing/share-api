module Share.Web.UCM.HistoryComments.Queries () where

import Data.Time (UTCTime)
import Share.Codebase.Types (CodebaseEnv (..))
import Share.IDs
import Share.Postgres
import Share.Postgres.Cursors (PGCursor)
import Share.Postgres.Cursors qualified as PGCursor
import Share.Postgres.IDs
import Share.Prelude
import Share.Web.UCM.SyncV2.Types (IsCausalSpine (..), IsLibRoot (..))
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.SyncV2.Types (CBORBytes)

fetchNewComments :: CodebaseEnv -> ProjectId -> CausalId -> UTCTime -> PGCursor HistoryCommentChunk
fetchNewComments codebase projectId causalId sinceTime = _
