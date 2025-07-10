module Share.Codebase.Types
  ( CodebaseM,
    CodebaseEnv (..),
    CodebaseRuntime (..),
    CodebaseLocation (..),
    hoistCodebaseRuntime,
    codebaseLocationForUserCodebase,
    codebaseLocationForProjectBranchCodebase,
    codebaseLocationForProjectRelease,
    publicRoot,
  )
where

import Control.Monad.Morph (MFunctor (..))
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)

publicRoot :: Path.Path
publicRoot = Path.singleton (NameSegment "public")

-- | The scope of a given codebase transaction.
data CodebaseEnv = CodebaseEnv
  { codebaseOwner :: UserId
  }

-- | The runtime environment for a codebase transaction.
-- Includes a skolem type var to prevent the runtime from escaping the transaction its
-- runtime was initialized with.
data CodebaseRuntime s m = CodebaseRuntime
  { codeLookup :: CL.CodeLookup Symbol m Ann,
    -- Function to look up cached evaluation results for the runtime.
    cachedEvalResult :: Reference.Id -> m (Maybe (Rt.Term Symbol)),
    unisonRuntime :: Rt.Runtime Symbol
  }

hoistCodebaseRuntime :: (Monad m) => (forall x. m x -> n x) -> CodebaseRuntime s m -> CodebaseRuntime s n
hoistCodebaseRuntime f (CodebaseRuntime lookup eval runtime) =
  CodebaseRuntime
    { codeLookup = hoist f lookup,
      cachedEvalResult = \id -> f (eval id),
      unisonRuntime = runtime
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
