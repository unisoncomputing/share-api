module Main where

import Share.Env (withEnv)
import Share.BackgroundJobs.Monad
import UnliftIO

main :: IO ()
main = do
  withEnv \env -> runBackground env "share-task-runner" task

