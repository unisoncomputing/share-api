{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Share.Utils.Logging.Types
  ( LogMsg (..),
    Severity (..),
    GetSeverity (..),
  )
where

import GHC.Stack (CallStack)
import Share.Prelude
import Prelude hiding (log)

data LogMsg = LogMsg
  { severity :: Severity,
    callstack :: Maybe CallStack,
    msg :: Text,
    tags :: Map Text Text
  }

data Severity
  = Debug
  | Info
  | -- Distinct from Error in that the caller made a mistake, it's not a server error.
    UserFault
  | Error
  deriving (Show, Eq, Ord)

-- | Severity Singleton class for deriving via instances.
class GetSeverity (s :: Severity) where
  getSeverity :: Proxy s -> Severity

instance GetSeverity 'Debug where
  getSeverity _ = Debug

instance GetSeverity 'Info where
  getSeverity _ = Info

instance GetSeverity 'UserFault where
  getSeverity _ = UserFault

instance GetSeverity 'Error where
  getSeverity _ = Error
