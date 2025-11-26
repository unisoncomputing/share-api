{-# LANGUAGE DataKinds #-}

module Share.Web.UCM.SyncCommon.Types (CodebaseLoadingError (..)) where

import Data.Text.Encoding qualified as Text
import Servant
import Share.IDs
import Share.IDs qualified as IDs
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Unison.SyncCommon.Types

data CodebaseLoadingError
  = CodebaseLoadingErrorProjectNotFound ProjectShortHand
  | CodebaseLoadingErrorUserNotFound UserHandle
  | CodebaseLoadingErrorNoReadPermission BranchRef
  | CodebaseLoadingErrorInvalidBranchRef Text BranchRef
  deriving stock (Show)
  deriving (Logging.Loggable) via Logging.ShowLoggable Logging.UserFault CodebaseLoadingError

instance ToServerError CodebaseLoadingError where
  toServerError = \case
    CodebaseLoadingErrorProjectNotFound projectShortHand -> (ErrorID "codebase-loading:project-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "Project not found: " <> (IDs.toText projectShortHand)})
    CodebaseLoadingErrorUserNotFound userHandle -> (ErrorID "codebase-loading:user-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "User not found: " <> (IDs.toText userHandle)})
    CodebaseLoadingErrorNoReadPermission branchRef -> (ErrorID "codebase-loading:no-read-permission", Servant.err403 {errBody = from . Text.encodeUtf8 $ "No read permission for branch ref: " <> (unBranchRef branchRef)})
    CodebaseLoadingErrorInvalidBranchRef err branchRef -> (ErrorID "codebase-loading:invalid-branch-ref", Servant.err400 {errBody = from . Text.encodeUtf8 $ "Invalid branch ref: " <> err <> " " <> (unBranchRef branchRef)})
