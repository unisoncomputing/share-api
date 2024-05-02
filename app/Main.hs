module Main where

import Enlil
import Env (withEnv)

main :: IO ()
main = do
  withEnv startApp
