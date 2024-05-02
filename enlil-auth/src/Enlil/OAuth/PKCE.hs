-- | Helper for easily generating PKCE challenges and verifiers according to:
-- https://datatracker.ietf.org/doc/html/rfc7636
--
-- Currently only supports the S256 method.
module Enlil.OAuth.PKCE (generatePkce, verifyPkce) where

import Control.Monad.Except
import Crypto.Hash qualified as Crypto
import Data.ByteArray.Encoding qualified as BE
import Data.Text.Encoding qualified as Text
import Enlil.OAuth.Types
import Enlil.Utils.SecureTokens (newSecureToken)

-- | Generate a PKCE verifier and challenge using the S256 method.
generatePkce :: MonadIO m => m (PKCEVerifier, PKCEChallenge, PKCEChallengeMethod)
generatePkce = do
  verifier <- newSecureToken
  let digest = Crypto.hashWith Crypto.SHA256 $ Text.encodeUtf8 verifier
  let challenge = Text.decodeUtf8 $ BE.convertToBase BE.Base64URLUnpadded digest
  pure (PKCEVerifier verifier, PKCEChallenge challenge, SHA256Method)

-- | Verify a PKCE challenge, returning True if the challenge is valid.
verifyPkce :: PKCEVerifier -> PKCEChallenge -> PKCEChallengeMethod -> Bool
verifyPkce (PKCEVerifier verifier) (PKCEChallenge challenge) method =
  case method of
    -- We don't support plain text PKCE.
    PlainTextMethod -> False
    SHA256Method ->
      let digest = Crypto.hashWith Crypto.SHA256 $ Text.encodeUtf8 verifier
          b64Digest = BE.convertToBase BE.Base64URLUnpadded digest
       in b64Digest == Text.encodeUtf8 challenge
