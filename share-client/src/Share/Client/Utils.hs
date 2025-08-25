{-# LANGUAGE TypeOperators #-}

module Share.Client.Utils (jwtToAuthenticatedRequest, maybeJwtToAuthenticatedRequest) where

import Crypto.JWT qualified as JWT
import Data.Foldable
import Data.Function ((&))
import Data.List qualified as List
import Servant.Client.Core
import Servant.Client.Core qualified as Client
import Share.JWT qualified as ShareJWT

jwtToAuthenticatedRequest :: (AuthClientData a ~ JWT.SignedJWT) => JWT.SignedJWT -> AuthenticatedRequest a
jwtToAuthenticatedRequest jwt =
  mkAuthenticatedRequest jwt addJWTHeader

maybeJwtToAuthenticatedRequest :: (AuthClientData a ~ Maybe JWT.SignedJWT) => Maybe JWT.SignedJWT -> AuthenticatedRequest a
maybeJwtToAuthenticatedRequest mayJWT =
  mkAuthenticatedRequest mayJWT (maybe id addJWTHeader)

addJWTHeader :: JWT.SignedJWT -> Request -> Request
addJWTHeader jwt req =
  case List.lookup "Authorization" (toList $ Client.requestHeaders req) of
    Nothing ->
      req
        & Client.addHeader "Authorization" ("Bearer " <> ShareJWT.signedJWTToText jwt)
    Just _ -> req
