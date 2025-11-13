module Main where

import Share
import Share.Env (withEnv)

main :: IO ()
main = do
  withEnv startApp
