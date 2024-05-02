module Enlil.Utils.SecureTokens (newSecureToken) where

import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding qualified as BE
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import UnliftIO (MonadIO (liftIO))

-- | Generate a cryptographically secure random token.
-- https://neilmadden.blog/2018/08/30/moving-away-from-uuids/
--
--  E.g.
-- >>> newSecureToken
-- "uxf85C7Y0B6om47SxHNRJarKa5-EnRVkTJ7mWOwC"
newSecureToken :: (MonadIO m) => m Text
newSecureToken = do
  liftIO $ do
    Text.decodeUtf8 . BE.convertToBase @ByteString BE.Base64URLUnpadded <$> getRandomBytes numRandomBytes
  where
    numRandomBytes = 30
