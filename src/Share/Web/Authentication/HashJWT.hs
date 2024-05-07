module Share.Web.Authentication.HashJWT where

import Crypto.JWT
import Share.Env qualified as Env
import Share.JWT qualified as JWT
import Share.Prelude
import Share.Web.App
import Share.Web.Errors
import Unison.Share.API.Hash (HashJWTClaims)

signHashJWT :: HashJWTClaims -> WebApp SignedJWT
signHashJWT claims = do
  jSettings <- asks Env.jwtSettings
  JWT.signJWT jSettings claims >>= \case
    Left err -> respondError (InternalServerError "jwt:signing-error" err)
    Right a -> pure a
