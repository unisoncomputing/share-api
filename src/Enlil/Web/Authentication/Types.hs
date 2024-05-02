module Enlil.Web.Authentication.Types where

import Crypto.JWT
import Crypto.JWT qualified as JWT
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Enlil.OAuth.Scopes
import Enlil.Prelude
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.Errors
import Enlil.Web.Errors qualified as Errors
import Servant

data AuthenticationErr
  = JWTErr JWT.JWTError
  | TextIsNotAJWT
  | CustomError Text
  | MissingScopes Scopes
  deriving stock (Show)

instance Logging.Loggable AuthenticationErr where
  toLog = Logging.withSeverity Logging.UserFault . Logging.textLog . tShow

instance Errors.ToServerError AuthenticationErr where
  toServerError err =
    let (errID, serverErr) = case err of
          TextIsNotAJWT -> (ErrorID "authn:not-a-jwt", err401)
          CustomError {} -> (ErrorID "authn:custom", err401)
          MissingScopes {} -> (ErrorID "authn:missing-scopes", err403)
          JWTErr e -> case e of
            JWSError _ -> (ErrorID "authn:jwt:jws-error", err401)
            JWTClaimsSetDecodeError {} -> (ErrorID "authn:jwt:claims-set-decode-error", err401)
            JWTExpired -> (ErrorID "authn:jwt:expired", err401)
            JWTNotYetValid -> (ErrorID "authn:jwt:not-yet-valid", err401)
            JWTNotInIssuer -> (ErrorID "authn:jwt:not-in-issuer", err401)
            JWTNotInAudience -> (ErrorID "authn:jwt:not-in-audience", err401)
            JWTIssuedAtFuture -> (ErrorID "authn:jwt:issued-at-future", err401)
     in (errID, serverErr {errBody = BL.fromStrict . Text.encodeUtf8 $ authErrMsg err})

authErrMsg :: AuthenticationErr -> Text
authErrMsg = \case
  TextIsNotAJWT -> "Token is not a valid JWT"
  CustomError txt -> txt
  MissingScopes scopes -> "The following scopes are required: " <> Text.pack (show scopes)
  JWTErr e -> case e of
    JWSError _ -> "Invalid Token Signature"
    JWTClaimsSetDecodeError e -> "Invalid Token Claims, " <> Text.pack e
    JWTExpired -> "Token Expired"
    JWTNotYetValid -> "Token used before valid"
    JWTNotInIssuer -> "Invalid Issuer"
    JWTNotInAudience -> "Invalid Audience"
    JWTIssuedAtFuture -> "Token used before issued"
