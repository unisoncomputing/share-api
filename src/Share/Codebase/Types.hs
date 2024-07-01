module Share.Codebase.Types
  ( CodebaseM,
    CodebaseEnv (..),
    CodebaseRuntime (..),
    CodebaseLocation (..),
    codebaseLocationForUserCodebase,
    codebaseLocationForProjectBranchCodebase,
    codebaseLocationForProjectRelease,
    publicRoot,
  )
where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Parser.Ann (Ann)
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.NameSegment.Internal (NameSegment (..))

publicRoot :: Path.Path
publicRoot = Path.singleton $ NameSegment "public"

-- | The scope of a given codebase transaction.
data CodebaseEnv = CodebaseEnv
  { codebaseOwner :: UserId
  }

data CodebaseRuntime = CodebaseRuntime
  { codeLookup :: CL.CodeLookup Symbol IO Ann,
    -- Function to look up cached evaluation results for the runtime.
    cachedEvalResult :: Reference.Id -> IO (Maybe (Rt.Term Symbol)),
    unisonRuntime :: Rt.Runtime Symbol
  }

-- | A PG Transaction scoped to a specific user codebase.
type CodebaseM e = ReaderT CodebaseEnv (PG.Transaction e)

newtype CodebaseLocation = CodebaseLocation {codebaseOwnerUserId :: UserId}
  deriving stock (Eq, Ord, Show)

-- | Builds the codebase directory for the provided user's codebase.
codebaseLocationForUserCodebase :: UserId -> CodebaseLocation
codebaseLocationForUserCodebase = CodebaseLocation

codebaseLocationForProjectBranchCodebase ::
  -- | Project Owner
  UserId ->
  -- | Optional branch contributor user
  Maybe UserId ->
  CodebaseLocation
codebaseLocationForProjectBranchCodebase projOwner branchContributor =
  case branchContributor of
    Just contributorUser -> CodebaseLocation contributorUser
    Nothing -> CodebaseLocation projOwner

codebaseLocationForProjectRelease ::
  -- | Project Owner
  UserId ->
  CodebaseLocation
codebaseLocationForProjectRelease = codebaseLocationForUserCodebase
