module Share.Codebase.Types
  ( CodebaseEnv (..),
    CodebaseRuntime (..),
    CodebaseLocation (..),
    CodeCache (..),
    CodeCacheData (..),
    hoistCodebaseRuntime,
    codebaseLocationForUserCodebase,
    codebaseLocationForProjectBranchCodebase,
    codebaseLocationForProjectRelease,
    publicRoot,
  )
where

import Control.Monad.Morph (MFunctor (..))
import Data.Map (Map)
import Share.IDs
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.DataDeclaration qualified as V1
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as V1
import Unison.Type qualified as V1
import UnliftIO.STM (TVar)

publicRoot :: Path.Path
publicRoot = Path.singleton (NameSegment "public")

-- | The scope of a given codebase transaction.
data CodebaseEnv = CodebaseEnv
  { codebaseOwner :: UserId
  }

data CodeCache scope = CodeCache
  { codeCacheCodebaseEnv :: CodebaseEnv,
    codeCacheVar :: TVar CodeCacheData
  }

data CodeCacheData = CodeCacheData
  { termCache :: Map Reference.Id (V1.Term Symbol Ann, V1.Type Symbol Ann),
    typeCache :: Map Reference.Id (V1.Decl Symbol Ann)
  }

-- | The runtime environment for a codebase transaction.
-- Includes a skolem scope type var to prevent the runtime from escaping the transaction its
-- runtime was initialized with.
data CodebaseRuntime scope m = CodebaseRuntime
  { codeLookup :: CL.CodeLookup Symbol m Ann,
    codeCache :: CodeCache scope,
    -- Function to look up cached evaluation results for the runtime.
    cachedEvalResult :: Reference.Id -> m (Maybe (Rt.Term Symbol)),
    unisonRuntime :: Rt.Runtime Symbol
  }

hoistCodebaseRuntime :: (Monad m) => (forall x. m x -> n x) -> CodebaseRuntime s m -> CodebaseRuntime s n
hoistCodebaseRuntime f (CodebaseRuntime lookup codeCache eval runtime) =
  CodebaseRuntime
    { codeLookup = hoist f lookup,
      codeCache,
      cachedEvalResult = \id -> f (eval id),
      unisonRuntime = runtime
    }

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
