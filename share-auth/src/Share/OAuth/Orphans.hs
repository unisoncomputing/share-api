{-# OPTIONS_GHC -Wno-orphans #-}

module Share.OAuth.Orphans () where

import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.JWT qualified as JWT
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Network.URI (URI, parseURI)

instance JWT.MonadRandom (ExceptT e IO) where
  getRandomBytes = liftIO . JWT.getRandomBytes

instance ToJSON URI where
  toJSON = Aeson.String . Text.pack . show @URI

instance FromJSON URI where
  parseJSON = Aeson.withText "URI" $ \t -> case parseURI (Text.unpack t) of
    Just uri -> pure uri
    Nothing -> fail "Invalid URI"
