module Share.Web.UCM.HistoryComments.Impl (server) where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Either (partitionEithers)
import Network.WebSockets.Connection
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Queries qualified as PGQ
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Web.App (WebApp, WebAppServer)
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (Unimplemented (Unimplemented), reportError, respondError)
import Share.Web.UCM.HistoryComments.Queries qualified as Q
import Unison.Server.HistoryComments.API qualified as HistoryComments
import Unison.Server.HistoryComments.Types (DownloadCommentsRequest (DownloadCommentsRequest), HistoryCommentChunk (..), UploadCommentsResponse (..))
import Unison.Server.Types
import Unison.Util.Websockets
import UnliftIO

server :: Maybe UserId -> HistoryComments.Routes WebAppServer
server mayCaller =
  HistoryComments.Routes
    { downloadHistoryComments = downloadHistoryCommentsStreamImpl mayCaller,
      uploadHistoryComments = uploadHistoryCommentsStreamImpl mayCaller
    }

wsMessageBufferSize :: Int
wsMessageBufferSize = 100

downloadHistoryCommentsStreamImpl :: Maybe UserId -> Connection -> WebApp ()
downloadHistoryCommentsStreamImpl mayUserId (DownloadCommentsRequest {}) = do
  _ <- error "AUTH CHECK HERE"
  respondError Unimplemented

-- Re-run the given STM action at most n times, collecting the results into a list.
-- If the action returns Nothing, stop early and return what has been collected so far, along with a Bool indicating whether the action was exhausted.
fetchChunk :: Int -> STM (Maybe a) -> STM ([a], Bool)
fetchChunk size action = do
  let go 0 = pure ([], False)
      go n = do
        optional action >>= \case
          Nothing -> do
            -- No more values available at the moment
            pure ([], False)
          Just Nothing -> do
            -- Queue is closed
            pure ([], True)
          Just (Just val) -> do
            (rest, exhausted) <- go (n - 1)
            pure (val : rest, exhausted)
  go size

uploadHistoryCommentsStreamImpl :: Maybe UserId -> BranchRef -> Connection -> WebApp ()
uploadHistoryCommentsStreamImpl mayCallerUserId br@(BranchRef branchRef) conn = do
  callerUserId <- AuthN.requireAuthenticatedUser' mayCallerUserId
  result <- withQueues @UploadCommentsResponse @HistoryCommentChunk wsMessageBufferSize wsMessageBufferSize conn \q@(Queues {receive}) -> runExceptT $ do
    projectBranchSH@ProjectBranchShortHand {userHandle, projectSlug, contributorHandle} <- case IDs.fromText @ProjectBranchShortHand branchRef of
      Left err -> handleErrInQueue q (UploadCommentsGenericFailure $ IDs.toText err)
      Right pbsh -> pure pbsh
    let projectSH = ProjectShortHand {userHandle, projectSlug}
    mayInfo <- runMaybeT $ mapMaybeT PG.runTransaction $ do
      project <- MaybeT $ PGQ.projectByShortHand projectSH
      branch <- MaybeT $ PGQ.branchByProjectBranchShortHand projectBranchSH
      contributorUser <- MaybeT $ for contributorHandle UserQ.userByHandle
      pure (project, branch, contributorUser)
    (project, _branch, contributorUser) <- maybe (handleErrInQueue q $ UploadCommentsProjectBranchNotFound br) pure $ mayInfo
    authZ <-
      lift (AuthZ.checkUploadToProjectBranchCodebase callerUserId project.projectId contributorUser.user_id) >>= \case
        Left _authErr -> handleErrInQueue q (UploadCommentsNotAuthorized br)
        Right authZ -> pure authZ
    projectId <- error "Process Branch Ref"
    let loop :: ExceptT UploadCommentsResponse WebApp ()
        loop = do
          (chunk, closed) <- atomically $ fetchChunk insertCommentBatchSize do
            receive <&> fmap \case
              HistoryCommentErrorChunk err -> (Left $ UploadCommentsGenericFailure err)
              chunk -> (Right chunk)
          let (errs, chunks) = partitionEithers chunk
          PG.runTransaction $ Q.insertHistoryComments authZ projectId chunks
          for errs $ \err -> handleErrInQueue q err
          when (not closed) loop
    loop
  case result of
    Left err -> reportError err
    Right (Left err) -> reportError err
    Right (Right ()) -> pure ()
  where
    insertCommentBatchSize = 100
    handleErrInQueue :: forall o x. Queues UploadCommentsResponse o -> UploadCommentsResponse -> ExceptT UploadCommentsResponse WebApp x
    handleErrInQueue Queues {send} e = do
      _ <- atomically $ send e
      throwError e
