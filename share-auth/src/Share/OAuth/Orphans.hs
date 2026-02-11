{-# OPTIONS_GHC -Wno-orphans #-}

module Share.OAuth.Orphans () where

import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.JWT qualified as JWT

instance JWT.MonadRandom (ExceptT e IO) where
  getRandomBytes = liftIO . JWT.getRandomBytes
