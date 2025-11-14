module Main where

import Share.BackgroundJobs.Monad
import Share.Env (withEnv)
import Share.Tasks.AmbiguousComponentCheck qualified as AmbiguousComponentCheck

main :: IO ()
main = do
  withEnv \env -> runBackground env "share-task-runner" AmbiguousComponentCheck.run
