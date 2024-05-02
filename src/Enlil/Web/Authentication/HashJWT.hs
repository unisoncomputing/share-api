module Enlil.Web.Authentication.HashJWT where

import Crypto.JWT
import Enlil.Env qualified as Env
import Enlil.JWT qualified as JWT
import Enlil.Prelude
import Enlil.Web.App
import Enlil.Web.Errors
import Unison.Share.API.Hash (HashJWTClaims)

signHashJWT :: HashJWTClaims -> WebApp SignedJWT
signHashJWT claims = do
  jSettings <- asks Env.jwtSettings
  JWT.signJWT jSettings claims >>= \case
    Left err -> respondError (InternalServerError "jwt:signing-error" err)
    Right a -> pure a
