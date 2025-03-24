-- | Postgres operations composed of individual queries
module Share.Postgres.Ops where

import Control.Monad.Except
import Servant
import Share.IDs (ProjectId, ProjectSlug (..), UserHandle (..), UserId (..))
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Queries as Q
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project
import Share.User (User (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Errors

data Errors
  = ProjectAlreadyExists UserId ProjectSlug ProjectId

instance Logging.Loggable Errors where
  toLog (ProjectAlreadyExists uid slug projId) =
    Logging.textLog "Project already exists"
      & Logging.withSeverity Logging.UserFault
      & Logging.withTag ("user-id", IDs.toText uid)
      & Logging.withTag ("project-slug", IDs.toText slug)
      & Logging.withTag ("project-id", IDs.toText projId)

instance ToServerError Errors where
  toServerError = \case
    ProjectAlreadyExists {} -> (ErrorID "project:already-exists", err409 {errBody = "Project already exists for this user and slug"})

expectUserByHandle :: UserHandle -> WebApp User
expectUserByHandle userHandle = PG.runTransaction (UserQ.userByHandle userHandle) `or404` (EntityMissing (ErrorID "user-not-found-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)

expectUserById :: UserId -> WebApp User
expectUserById uid = PG.runTransaction (UserQ.userByUserId uid) `or404` (EntityMissing (ErrorID "user-not-found-for-id") $ "User not found for id: " <> IDs.toText uid)

projectIdByUserIdAndSlug :: UserId -> ProjectSlug -> WebApp ProjectId
projectIdByUserIdAndSlug userId slug = do
  PG.runTransactionOrRespondError $ do
    user <- UserQ.userByUserId userId `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-id") $ "No found for id: " <> IDs.toText userId)
    Q.projectIDFromHandleAndSlug (handle user) slug `whenNothingM` throwError (EntityMissing (ErrorID "no-project-for-handle-and-slug") $ "Project not found: " <> IDs.toText (handle user) <> "/" <> IDs.toText slug)

projectIdByUserHandleAndSlug :: UserHandle -> ProjectSlug -> WebApp ProjectId
projectIdByUserHandleAndSlug userHandle projectSlug = do
  PG.runTransaction (Q.projectIDFromHandleAndSlug userHandle projectSlug) `or404` (EntityMissing (ErrorID "no-project-for-handle-and-slug") $ "Project not found: " <> IDs.toText userHandle <> "/" <> IDs.toText projectSlug)

createProject :: UserId -> ProjectSlug -> Maybe Text -> Set ProjectTag -> ProjectVisibility -> WebApp ProjectId
createProject ownerUserId slug summary tags visibility = do
  PG.runTransactionOrRespondError do
    Q.projectIDFromUserIdAndSlug ownerUserId slug >>= \case
      Just projectId -> throwError (ProjectAlreadyExists ownerUserId slug projectId)
      Nothing -> Q.createProject ownerUserId slug summary tags visibility
