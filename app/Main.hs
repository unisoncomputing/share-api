module Main where

import Share
import Env (withEnv)

main :: IO ()
main = do
  withEnv startApp
