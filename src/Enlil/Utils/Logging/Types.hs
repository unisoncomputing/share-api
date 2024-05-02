module Enlil.Utils.Logging.Types where

import Enlil.Prelude
import GHC.Stack (CallStack)
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
