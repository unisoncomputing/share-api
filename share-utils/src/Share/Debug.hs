{-# LANGUAGE OverloadedStrings #-}
-- pTrace is marked deprecated so you get warnings when you use it.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Share.Debug
  ( debug,
    debugM,
    whenDebug,
    debugLog,
    debugLogM,
    shouldDebug,
    DebugFlag (..),
  )
where

import Control.Monad
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Debug.Pretty.Simple (pTrace, pTraceM)
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pShow)
import UnliftIO.Environment (lookupEnv)
import Witch (into)

data DebugFlag
  = Timing
  | Queries
  deriving (Eq, Ord, Show, Bounded, Enum)

debugFlags :: Set DebugFlag
debugFlags = case (unsafePerformIO (lookupEnv "SHARE_DEBUG")) of
  Nothing -> Set.empty
  Just "" -> Set.fromList [minBound .. maxBound]
  Just s -> Set.fromList $ do
    w <- (Text.splitOn "," . Text.pack $ s)
    case Text.toUpper . Text.strip $ w of
      "TIMING" -> pure Timing
      "QUERIES" -> pure Queries
      _ -> mempty
{-# NOINLINE debugFlags #-}

debugTiming :: Bool
debugTiming = Timing `Set.member` debugFlags
{-# NOINLINE debugTiming #-}

debugQueries :: Bool
debugQueries = Queries `Set.member` debugFlags
{-# NOINLINE debugQueries #-}

-- | Use for trace-style selective debugging.
-- E.g. 1 + (debug Sync "The second number" 2)
--
-- Or, use in pattern matching to view arguments.
-- E.g.
-- myFunc (debug Sync "argA" -> argA) = ...
debug :: (Show a) => DebugFlag -> String -> a -> a
debug flag msg a =
  if shouldDebug flag
    then (trace (msg <> ":\n" <> into @String (pShow a)) a)
    else a

-- | Use for selective debug logging in monadic contexts.
-- E.g.
-- do
--   debugM Sync "source repo" srcRepo
--   ...
debugM :: (Show a, Monad m) => DebugFlag -> String -> a -> m ()
debugM flag msg a =
  whenDebug flag do
    traceM (msg <> ":\n" <> into @String (pShow a))

debugLog :: DebugFlag -> String -> a -> a
debugLog flag msg =
  if shouldDebug flag
    then pTrace msg
    else id

debugLogM :: (Monad m) => DebugFlag -> String -> m ()
debugLogM flag msg =
  whenDebug flag $ pTraceM msg

-- | A 'when' block which is triggered if the given flag is being debugged.
whenDebug :: (Monad m) => DebugFlag -> m () -> m ()
whenDebug flag action = do
  when (shouldDebug flag) action

shouldDebug :: DebugFlag -> Bool
shouldDebug = \case
  Timing -> debugTiming
  Queries -> debugQueries
