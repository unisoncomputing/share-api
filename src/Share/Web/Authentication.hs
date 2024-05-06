{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Share.Web.Authentication
  ( cookieSessionTTL,
    requireAuthenticatedUser,
    UnauthenticatedError (..),
    pattern MaybeAuthedUserID,
    pattern AuthenticatedUser,
    pattern Unauthenticated,
  )
where

import Data.Time
import Share.IDs
import Share.OAuth.Session (Session (..), pattern AuthenticatedUser, pattern MaybeAuthedUserID, pattern Unauthenticated)
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Errors
import Share.Web.Errors qualified as Errors
import Servant

cookieSessionTTL :: NominalDiffTime
cookieSessionTTL =
  30 * nominalDay

data UnauthenticatedError = UnauthenticatedError
  deriving stock (Show)

instance Logging.Loggable UnauthenticatedError where
  toLog = Logging.withSeverity Logging.UserFault . Logging.showLog

instance ToServerError UnauthenticatedError where
  toServerError _ = (ErrorID "unauthenticated", err401 {errBody = "Unauthenticated"})

-- | Note: prefer AuthenticatedUserId instead.
requireAuthenticatedUser :: Maybe Session -> WebApp UserId
requireAuthenticatedUser (AuthenticatedUser uid) = pure uid
requireAuthenticatedUser _ = Errors.respondError UnauthenticatedError
