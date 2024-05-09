{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Web.Errors
  ( respondError,
    reportError,
    ToServerError (..),
    ErrorRedirect (..),
    InternalServerError (..),
    EntityMissing (..),
    MissingExpectedEntity (..),
    Unimplemented (..),
    BadRequest (..),
    InvalidParam (..),
    NotAuthorized (..),
    ErrorID (..),
    internalServerError,
    invalidName,
    missingParameter,
    or404,
    or403,
    errorRedirect,
    ShareUIError (..),
    SomeServerError (..),
    someServerError,
    withCallstack,
    throwSomeServerError,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Hash (SHA256)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.String (IsString)
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack qualified as GHC
import Servant
import Servant.Client
import Share.Env qualified as Env
import Share.Monitoring qualified as Monitoring
import Share.OAuth.Errors (OAuth2Error (..), OAuth2ErrorCode (..), OAuth2ErrorRedirect (..))
import Share.OAuth.Types (AuthenticationRequest (..), RedirectReceiverErr (..))
import Share.Prelude
import Share.Utils.Logging
import Share.Utils.URI (URIParam (..), addQueryParam)
import Share.Web.App
import Unison.Server.Backend qualified as Backend
import Unison.Server.Errors qualified as Backend
import Unison.Sync.Types qualified as Sync
import UnliftIO qualified

newtype ErrorID = ErrorID Text
  deriving stock (Show)
  deriving (IsString) via Text

class ToServerError e where
  toServerError :: e -> (ErrorID, ServerError)

-- Helpful for cases where an error is specialized to Void.
instance ToServerError Void where
  toServerError = absurd

instance ToServerError ClientError where
  toServerError _err =
    (ErrorID "unknown-client-error", internalServerError)

instance ToServerError RedirectReceiverErr where
  toServerError = \case
    MismatchedState {} -> (ErrorID "oauth:mismatched-state", err400 {errBody = "Mismatched state parameter"})
    MissingOrExpiredPendingSession {} -> (ErrorID "oauth:no-pending-session", err404 {errBody = "Auth session has expired or is missing. Please try again."})
    MissingCode {} -> (ErrorID "oauth:missing-code", err404 {errBody = "'code' parameter is required"})
    MissingState {} -> (ErrorID "oauth:missing-code", err404 {errBody = "'state' parameter is required"})
    ErrorFromIdentityProvider {} -> (ErrorID "oauth:identity-provider-error", err400 {errBody = "Error from identity provider"})
    FailedToCreateSession {} -> (ErrorID "oauth:failed-to-create-session", err500 {errBody = "Failed to create session"})
    InvalidJWTFromIDP {} -> (ErrorID "oauth:invalid-jwt-from-idp", err400 {errBody = "Invalid JWT from identity provider"})

instance ToServerError OAuth2Error where
  toServerError err = case err of
    CodeMissingOrExpired {} -> (ErrorID "oauth:code-missing", err404 {errBody = "Session for code is missing or expired."})
    MismatchedClientId {} -> (ErrorID "oauth:mismatched-client-id", err400 {errBody = "Mismatched client_id"})
    MismatchedRedirectURI {} -> (ErrorID "oauth:mismatched-redirect-uri", err400 {errBody = "Mismatched redirect_uri"})
    UnregisteredRedirectURI {} -> (ErrorID "oauth:unregistered-redirect-uri", err400 {errBody = "Unregistered redirect_uri"})
    MismatchedClientSecret {} -> (ErrorID "oauth:mismatched-client-secret", err400 {errBody = "Mismatched client_secret"})
    UnknownClient {} -> (ErrorID "oauth:unknown-client", err400 {errBody = "Unregistered client_id"})
    OpenIDScopeRequired (AuthenticationRequest {..}) ->
      toServerError
        ( OAuth2ErrorRedirect
            { errCode = InvalidScope,
              errDescription = "openid must be a requested scope",
              state = Just state,
              redirectURI = Just redirectURI
            }
        )
    PKCEChallengeFailure -> (ErrorID "oauth:pkce-challenge-failure", err400 {errBody = "PKCE challenge failure."})

instance ToServerError OAuth2ErrorRedirect where
  toServerError (OAuth2ErrorRedirect {redirectURI = mayRedirectURI, errCode, errDescription, state = mayState}) =
    case mayRedirectURI of
      Nothing ->
        (ErrorID $ "oauth:" <> tShow errCode, err400 {errBody = BL.fromStrict $ Text.encodeUtf8 errDescription})
      Just (URIParam redirectURI) ->
        let errURI =
              redirectURI
                & addQueryParam "error" errCode
                & addQueryParam "error_description" errDescription
                & maybe id (addQueryParam "state") mayState
         in (ErrorID $ "oauth:" <> tShow errCode, err302 {errHeaders = [("Location", BSC.pack $ show errURI)]})

-- | Logs the error with a call stack then aborts the request and renders the corresponding ServerError to the client.
respondError :: (HasCallStack, ToServerError e, Loggable e) => e -> WebApp a
respondError e = do
  let (_, serverErr) = toServerError e
  reportError e
  UnliftIO.throwIO serverErr

-- | Logs the error with a call stack, but doesn't abort the request or render an error to the client.
reportError :: (HasCallStack, ToServerError e, Loggable e) => e -> WebApp ()
reportError e = do
  let (ErrorID errID, serverErr) = toServerError e
  env <- ask
  RequestCtx {pathInfo, rawURI} <- asks Env.ctx
  reqTags <- getTags
  let errLog@LogMsg {msg} = withTag ("error-id", errID) $ toLog e
  -- We emit a separate log message for each error, but it's also
  -- handy to have that data on the main log for the request.
  addRequestTag "error-id" errID
  addRequestTag "error-msg" msg
  let coreTags = HM.singleton "path" (Text.unpack $ "/" <> Text.intercalate "/" pathInfo)
  let extraTags =
        HM.fromList . fmap (bimap Text.unpack Aeson.String) . Map.toList $
          reqTags <> maybe mempty (\uri -> Map.singleton "url" (tShow @URI uri)) rawURI
  case (errHTTPCode serverErr) of
    status
      | status >= 500 -> do
          Monitoring.reportError env coreTags extraTags errID e
          logMsg (withSeverity Error $ errLog)
      | status >= 400 ->
          logMsg (withSeverity UserFault $ errLog)
      | otherwise -> logMsg (withSeverity Info $ errLog)
  pure ()

data InternalServerError err = InternalServerError
  { errorId :: Text,
    err :: err
  }
  deriving stock (Show)

instance (Show err) => Loggable (InternalServerError err) where
  toLog = withSeverity Error . showLog

internalServerError :: ServerError
internalServerError = err500 {errBody = "Something went wrong, please try again later"}

instance ToServerError (InternalServerError a) where
  toServerError InternalServerError {errorId} = (ErrorID errorId, internalServerError)

data EntityMissing = EntityMissing {entityMissingErrorID :: ErrorID, errorMsg :: Text}
  deriving stock (Show)

instance Loggable EntityMissing where
  toLog EntityMissing {errorMsg} = withSeverity UserFault $ textLog errorMsg

instance ToServerError EntityMissing where
  toServerError (EntityMissing {entityMissingErrorID}) = (entityMissingErrorID, err404 {errBody = "Not Found"})

data HashMismatch = HashMismatch {expected :: SHA256, actual :: SHA256}
  deriving stock (Show)

instance Loggable HashMismatch where
  toLog HashMismatch {expected, actual} =
    withSeverity UserFault $
      textLog $
        pack $
          "Hash mismatch: expected " <> show expected <> " but got " <> show actual

instance ToServerError HashMismatch where
  toServerError _ = (ErrorID "Hash Mismatch", err400 {errBody = "Hash Mismatch"})

data NotAuthorized = NotAuthorized deriving (Show)

instance Loggable NotAuthorized where
  toLog _ = withSeverity UserFault $ textLog "Not Authorized"

instance ToServerError NotAuthorized where
  toServerError _ = (ErrorID "Not Authorized", err403)

data ErrorRedirect = ErrorRedirect Text URI
  deriving stock (Show)

instance Loggable ErrorRedirect where
  toLog (ErrorRedirect msg _) = withSeverity Error $ textLog msg

instance ToServerError ErrorRedirect where
  toServerError (ErrorRedirect errorID redirectURI) =
    ( ErrorID errorID,
      err302
        { errHeaders = [("Location", BS.pack $ show redirectURI)]
        }
    )

missingParameter :: Text -> WebApp a
missingParameter param = respondError $ EntityMissing (ErrorID "missing-parameter") $ "Missing parameter: " <> param

invalidName :: Text -> WebApp a
invalidName name = respondError $ BadRequest $ "Invalid name: " <> name <> ". Names start with a a letter and contain only letters numbers and hyphens."

or403 :: WebApp (Bool) -> WebApp ()
or403 m =
  m >>= (\b -> if b then pure () else respondError NotAuthorized)

or404 :: WebApp (Maybe a) -> EntityMissing -> WebApp a
or404 m err =
  m >>= \case
    Nothing -> respondError err
    Just b -> pure b

--- Instances from Unison

instance ToServerError Backend.BackendError where
  toServerError err =
    let errID = case err of
          Backend.NoSuchNamespace {} -> "backend:no-such-namespace"
          Backend.BadNamespace {} -> "backend:bad-namespace"
          Backend.NoBranchForHash {} -> "backend:no-branch-for-hash"
          Backend.CouldntLoadBranch {} -> "backend:couldnt-load-branch"
          Backend.CouldntExpandBranchHash {} -> "backend:couldnt-expand-branch-hash"
          Backend.AmbiguousBranchHash {} -> "backend:ambiguous-branch-hash"
          Backend.MissingSignatureForTerm {} -> "backend:missing-signature-for-term"
          Backend.NoSuchDefinition {} -> "backend:no-such-definition"
          Backend.AmbiguousHashForDefinition {} -> "backend:ambiguous-hash-for-definition"
          Backend.DisjointProjectAndPerspective {} -> "backend:disjoint-project-and-perspective"
          Backend.ExpectedNameLookup {} -> "backend:missing-name-lookup"
          Backend.ProjectBranchNameNotFound {} -> "backend:project-branch-name-not-found"
     in (ErrorID errID, Backend.backendError err)

-- | Redirect the user to the Share UI and show an error message.
errorRedirect :: ShareUIError -> WebApp a
errorRedirect shareUIError = do
  errURI <- shareUIPathQ ["error"] (Map.fromList [("appError", shareUIErrorToUIText shareUIError)])
  respondError $ ErrorRedirect (shareUIErrorToUIText shareUIError) errURI

-- | Various Error types that the Share UI knows how to interpret
data ShareUIError
  = UnspecifiedError
  | AccountCreationGitHubPermissionsRejected
  | AccountCreationHandleAlreadyTaken
  | AccountCreationInvalidHandle
  deriving (Show)

-- | Convert ShareUIError to the Text that the UI expects
shareUIErrorToUIText :: ShareUIError -> Text
shareUIErrorToUIText e =
  case e of
    UnspecifiedError ->
      "UnspecifiedError"
    AccountCreationGitHubPermissionsRejected ->
      "AccountCreationGitHubPermissionsRejected"
    AccountCreationHandleAlreadyTaken ->
      "AccountCreationHandleAlreadyTaken"
    AccountCreationInvalidHandle ->
      "AccountCreationInvalidHandle"

data Unimplemented = Unimplemented
  deriving (Eq, Show)

instance ToServerError Unimplemented where
  toServerError Unimplemented = (ErrorID "unimplemented", err501 {errBody = "Not Implemented"})

instance Loggable Unimplemented where
  toLog Unimplemented = withSeverity Error . textLog $ "Unimplemented"

newtype BadRequest = BadRequest Text
  deriving stock (Show)

instance ToServerError BadRequest where
  toServerError (BadRequest msg) = (ErrorID "bad-request", err400 {errBody = BL.fromStrict . Text.encodeUtf8 $ msg})

instance Loggable BadRequest where
  toLog (BadRequest msg) = withSeverity UserFault . textLog $ msg

data InvalidParam = InvalidParam {paramName :: Text, param :: Text, parseError :: Text}

instance ToServerError InvalidParam where
  toServerError (InvalidParam {paramName, parseError}) =
    ( ErrorID $ "invalid-param:" <> paramName,
      err400
        { errReasonPhrase = "Invalid Parameter",
          errBody = BL.fromStrict . Text.encodeUtf8 $ "Unable to parse parameter " <> paramName <> ", " <> parseError
        }
    )

instance Loggable InvalidParam where
  toLog (InvalidParam {paramName, param, parseError}) =
    withSeverity UserFault . textLog $
      "Invalid Parameter: " <> paramName <> ". value: " <> tShow param <> ", error: " <> parseError

-- | Box up server errors of any type into the same type.
-- This is handy when "throwing" into ExceptT in things like PG transactions
-- where there are many possible errors, but we're just going to throw them in the App
-- monad outside of the transaction anyways.
data SomeServerError where
  SomeServerError :: (ToServerError e, Loggable e, Show e) => e -> SomeServerError

instance Show SomeServerError where
  show (SomeServerError e) = show e

instance ToServerError SomeServerError where
  toServerError (SomeServerError e) = toServerError e

instance Loggable SomeServerError where
  toLog (SomeServerError e) = toLog e

-- | Error for when a database entry is expected to exist but is missing.
data MissingExpectedEntity = MissingExpectedEntity Text
  deriving stock (Show)

instance ToServerError MissingExpectedEntity where
  toServerError (MissingExpectedEntity msg) = (ErrorID "missing-expected-entity", err404 {errBody = BL.fromStrict $ Text.encodeUtf8 msg})

instance Loggable MissingExpectedEntity where
  toLog (MissingExpectedEntity msg) = withSeverity Error $ textLog ("missing-expected-entity: " <> msg)

-- | Allows throwing server errors of any type.
-- Also adds a callstack from the call-site.
someServerError :: (ToServerError e, Loggable e, Show e, HasCallStack) => e -> SomeServerError
someServerError = SomeServerError . withCallstack

instance Exception SomeServerError

data WithCallStack e = WithCallStack GHC.CallStack e
  deriving stock (Show)

instance ToServerError e => ToServerError (WithCallStack e) where
  toServerError (WithCallStack _ e) = toServerError e

instance Loggable e => Loggable (WithCallStack e) where
  toLog (WithCallStack cs e) =
    toLog e
      & withCallstackIfUnset cs

-- | Wraps a server error to include a callstack.
-- 'HasCallStack' implements 'Loggable' and 'ToServerError' so you
-- can use this in most error situations.
withCallstack :: (HasCallStack, Loggable e) => e -> WithCallStack e
withCallstack = WithCallStack GHC.callStack

throwSomeServerError :: (Show e, ToServerError e, Loggable e, MonadError SomeServerError m, HasCallStack) => e -> m a
throwSomeServerError = throwError . SomeServerError . withCallstack

-- Instances from unison types.

instance ToServerError Sync.EntityValidationError where
  toServerError = \case
    Sync.EntityHashMismatch {} -> ("entity-hash-mismatch", err500)
    Sync.UnsupportedEntityType {} -> ("unsupported-entity-type", err500)
    Sync.InvalidByteEncoding {} -> ("invalid-byte-encoding", err500)
    Sync.HashResolutionFailure {} -> ("hash-resolution-failure", err500)
