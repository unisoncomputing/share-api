module Share.Web.UCM.HistoryComments.Impl (server) where

import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty qualified as NEL
import Data.Monoid (All (..))
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Ki.Unlifted qualified as Ki
import Network.WebSockets.Connection
import Share.Branch
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursor
import Share.Postgres.Queries qualified as PGQ
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project
import Share.User
import Share.Web.App (WebApp, WebAppServer)
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (reportError)
import Share.Web.UCM.HistoryComments.Queries qualified as Q
import Unison.Debug qualified as Debug
import Unison.Server.HistoryComments.API qualified as HistoryComments
import Unison.Server.HistoryComments.Types (HistoryCommentDownloaderChunk (..), HistoryCommentUploaderChunk (..), UploadCommentsResponse (..))
import Unison.Server.HistoryComments.Types qualified as Sync
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

downloadHistoryCommentsStreamImpl :: Maybe UserId -> BranchRef -> Connection -> WebApp ()
downloadHistoryCommentsStreamImpl mayCallerUserId br@(BranchRef branchRef) conn = do
  result <- withQueues @(MsgOrError Sync.DownloadCommentsResponse HistoryCommentUploaderChunk) @(MsgOrError Void HistoryCommentDownloaderChunk) wsMessageBufferSize wsMessageBufferSize conn \q@(Queues {receive, send}) -> Ki.scoped \scope -> runExceptT $ do
    projectBranchSH@ProjectBranchShortHand {userHandle, projectSlug, contributorHandle} <- case IDs.fromText @ProjectBranchShortHand branchRef of
      Left err -> handleErrInQueue q (Sync.DownloadCommentsGenericFailure $ IDs.toText err)
      Right pbsh -> pure pbsh
    let projectSH = ProjectShortHand {userHandle, projectSlug}
    mayInfo <- lift . runMaybeT $ mapMaybeT PG.runTransaction $ do
      project <- MaybeT $ PGQ.projectByShortHand projectSH
      branch <- MaybeT $ PGQ.branchByProjectBranchShortHand projectBranchSH
      contributorUser <- for contributorHandle (MaybeT . UserQ.userByHandle)
      pure (project, branch, contributorUser)
    (project, branch, _contributorUser) <- maybe (handleErrInQueue q $ Sync.DownloadCommentsProjectBranchNotFound br) pure $ mayInfo
    !authZ <-
      lift (AuthZ.checkDownloadFromProjectBranchCodebase mayCallerUserId project.projectId) >>= \case
        Left _authErr -> handleErrInQueue q (Sync.DownloadCommentsNotAuthorized br)
        Right authZ -> pure authZ

    downloadableCommentsVar <-
      liftIO $ newTVarIO @_ @(Set (Either Sync.HistoryCommentHash32 Sync.HistoryCommentRevisionHash32)) Set.empty
    commentHashesToSendQ <- liftIO $ newTBMQueueIO @(Sync.HistoryCommentHash32, [Sync.HistoryCommentRevisionHash32]) 100
    commentsToSendQ <- liftIO $ newTBMQueueIO @(Either Sync.HistoryCommentHash32 Sync.HistoryCommentRevisionHash32) 100
    errMVar <- newEmptyTMVarIO
    _ <- lift $ Ki.fork scope (hashNotifyWorker send commentHashesToSendQ)
    senderThread <- lift $ Ki.fork scope (senderWorker send commentsToSendQ)
    _ <- lift $ Ki.fork scope (receiverWorker receive commentsToSendQ errMVar downloadableCommentsVar)
    lift $ PG.runTransaction $ do
      cursor <- Q.projectBranchCommentsCursor authZ branch.causal
      Cursor.foldBatched cursor 100 \hashes -> do
        let (newHashes, chunks) =
              hashes
                & foldMap
                  ( \(commentHash, revisionHash) ->
                      ([Left commentHash, Right revisionHash], [(commentHash, [revisionHash])])
                  )
                & first Set.fromList
        PG.transactionUnsafeIO $ atomically $ do
          modifyTVar downloadableCommentsVar (Set.union newHashes)
          for chunks \chunk -> writeTBMQueue commentHashesToSendQ (chunk)
    -- Close the hashes queue to signal we don't have any more, then wait for the notifier to finish
    atomically $ closeTBMQueue commentHashesToSendQ
    -- Now we just have to wait for the sender to finish sending all the comments we have queued up.
    -- Once we've uploaded everything we can safely exit and the connection will be closed.
    atomically $ Ki.await senderThread
  case result of
    Left err -> do
      reportError err
    Right (Left err, _leftovers {- Messages sent by server after we finished. -}) -> do
      reportError err
    Right (Right (), _leftovers {- Messages sent by server after we finished. -}) -> do
      pure ()
  where
    senderWorker ::
      ( MsgOrError err HistoryCommentUploaderChunk ->
        STM Bool
      ) ->
      TBMQueue (Either Sync.HistoryCommentHash32 Sync.HistoryCommentRevisionHash32) ->
      WebApp ()
    senderWorker send commentsToSendQ = do
      let loop = do
            (hashesToSend, isClosed) <- atomically $ flushTBMQueue commentsToSendQ
            withCommentsAndRevisions <- lift . PG.runTransaction $ do
              -- Send comments first, then revisions
              withComments <- Q.historyCommentsByHashOf (traversed . _Left) hashesToSend
              Q.historyCommentRevisionsByHashOf (traversed . _Right) withComments
            for_ withCommentsAndRevisions \commentOrRevision -> atomically . send . Msg $ intoChunk commentOrRevision
            guard (not isClosed)
            loop
      void . runMaybeT $ loop

    receiverWorker ::
      STM (Maybe (MsgOrError Void HistoryCommentDownloaderChunk)) ->
      TBMQueue
        ( Either
            Sync.HistoryCommentHash32
            Sync.HistoryCommentRevisionHash32
        ) ->
      TMVar Text ->
      (TVar (Set (Either Sync.HistoryCommentHash32 Sync.HistoryCommentRevisionHash32))) ->
      WebApp ()
    receiverWorker receive commentsToSendQ errMVar downloadableCommentsVar = do
      let loop = do
            msgOrError <- atomically receive
            case msgOrError of
              -- Channel closed, shut down
              Nothing -> pure ()
              Just (Msg msg) -> case msg of
                DoneCheckingHashesChunk -> do
                  -- The downloader is done checking hashes, and has issued all requests for
                  -- comments.
                  -- We can close the relevant queues now, we won't get any more requests.
                  atomically $ closeTBMQueue commentsToSendQ
                  loop
                RequestCommentsChunk comments -> do
                  atomically $ do
                    downloadableComments <- readTVar downloadableCommentsVar
                    let validComments = Set.intersection (NESet.toSet comments) downloadableComments
                    for_ validComments $ writeTBMQueue commentsToSendQ
                  loop
              Just (DeserialiseFailure msg) -> do
                atomically $ putTMVar errMVar $ "uploadHistoryComments: deserialisation failure: " <> msg
      loop

    hashNotifyWorker ::
      (MsgOrError Sync.DownloadCommentsResponse HistoryCommentUploaderChunk -> STM Bool) ->
      TBMQueue (Sync.HistoryCommentHash32, [Sync.HistoryCommentRevisionHash32]) ->
      WebApp ()
    hashNotifyWorker send q = do
      let loop = do
            isClosed <- atomically $ do
              (hashesToCheck, isClosed) <- flushTBMQueue q
              All sendSuccess <-
                NEL.nonEmpty hashesToCheck & foldMapM \possiblyNewHashes -> do
                  All <$> (send $ Msg $ PossiblyNewHashesChunk possiblyNewHashes)
              pure (isClosed || not sendSuccess)
            if isClosed
              then do
                -- If the queue is closed, send a DoneCheckingHashesChunk to notify the server we're done.
                void . atomically $ send (Msg DoneSendingHashesChunk)
              else loop
      loop
    intoChunk = either HistoryCommentChunk HistoryCommentRevisionChunk

-- Re-run the given STM action at most n times, collecting the results into a list.
-- If the action returns Nothing, stop and return what has been collected so far, along with a Bool indicating whether the action was exhausted.
fetchChunk :: (Show a) => Int -> STM (Maybe a) -> STM ([a], Bool)
fetchChunk size action = do
  let go 0 = pure ([], False)
      go n = do
        optional action >>= \case
          Nothing -> do
            -- No more values available at the moment
            empty
          Just Nothing -> do
            -- Queue is closed
            pure ([], True)
          Just (Just val) -> do
            (rest, exhausted) <- go (n - 1) <|> pure ([], False)
            pure (val : rest, exhausted)
  go size

uploadHistoryCommentsStreamImpl :: Maybe UserId -> BranchRef -> Connection -> WebApp ()
uploadHistoryCommentsStreamImpl mayCallerUserId br@(BranchRef branchRef) conn = do
  callerUserId <- AuthN.requireAuthenticatedUser' mayCallerUserId
  result <- withQueues @(MsgOrError UploadCommentsResponse HistoryCommentDownloaderChunk) @(MsgOrError Void HistoryCommentUploaderChunk) wsMessageBufferSize wsMessageBufferSize conn \q@(Queues {receive, send}) -> Ki.scoped \scope -> runExceptT $ do
    projectBranchSH@ProjectBranchShortHand {userHandle, projectSlug, contributorHandle} <- case IDs.fromText @ProjectBranchShortHand branchRef of
      Left err -> handleErrInQueue q (UploadCommentsGenericFailure $ IDs.toText err)
      Right pbsh -> pure pbsh
    let projectSH = ProjectShortHand {userHandle, projectSlug}
    mayInfo <- lift . runMaybeT $ mapMaybeT PG.runTransaction $ do
      project <- MaybeT $ PGQ.projectByShortHand projectSH
      branch <- MaybeT $ PGQ.branchByProjectBranchShortHand projectBranchSH
      contributorUser <- for contributorHandle (MaybeT . UserQ.userByHandle)
      pure (project, branch, contributorUser)
    (project, _branch, contributorUser) <- maybe (handleErrInQueue q $ UploadCommentsProjectBranchNotFound br) pure $ mayInfo
    !authZ <-
      lift (AuthZ.checkUploadToProjectBranchCodebase callerUserId project.projectId (user_id <$> contributorUser)) >>= \case
        Left _authErr -> handleErrInQueue q (UploadCommentsNotAuthorized br)
        Right authZ -> pure authZ
    hashesToCheckQ <- liftIO $ newTBMQueueIO @(Sync.HistoryCommentHash32, [Sync.HistoryCommentRevisionHash32]) 100
    commentsQ <- liftIO $ newTBMQueueIO 100
    errMVar <- liftIO newEmptyTMVarIO
    _receiverThread <- lift $ Ki.fork scope $ receiverWorker receive errMVar hashesToCheckQ commentsQ
    inserterThread <- lift $ Ki.fork scope $ inserterWorker authZ commentsQ project.projectId
    _hashCheckingThread <- lift $ Ki.fork scope $ hashCheckingWorker project.projectId send hashesToCheckQ
    Debug.debugLogM Debug.HistoryComments "Upload history comments: waiting for inserter thread to finish"
    -- The inserter thread will finish when the client closes the connection.
    atomically $ Ki.await inserterThread
    Debug.debugLogM Debug.HistoryComments "Done. Closing connection."
  case result of
    Left err -> reportError err
    Right (Left err, _leftovers) -> reportError err
    Right (Right (), _leftovers) -> pure ()
  where
    inserterWorker ::
      AuthZ.AuthZReceipt ->
      TBMQueue (Either Sync.HistoryComment Sync.HistoryCommentRevision) ->
      ProjectId ->
      WebApp ()
    inserterWorker authZ commentsQ projectId = do
      let loop = do
            (chunk, closed) <- atomically (fetchChunk insertCommentBatchSize (readTBMQueue commentsQ))
            PG.whenNonEmpty chunk do
              Debug.debugM Debug.HistoryComments "Inserting comments chunk of size" (length chunk)
              PG.runTransaction $ Q.insertHistoryComments authZ projectId chunk
            when closed $ Debug.debugLogM Debug.HistoryComments "Inserter worker: comments queue closed"
            when (not closed) loop
      loop
      Debug.debugLogM Debug.HistoryComments "Inserter worker finished"

    hashCheckingWorker ::
      ProjectId ->
      (MsgOrError err HistoryCommentDownloaderChunk -> STM Bool) ->
      TBMQueue (Sync.HistoryCommentHash32, [Sync.HistoryCommentRevisionHash32]) ->
      WebApp ()
    hashCheckingWorker projectId send hashesToCheckQ = do
      let loop = do
            (hashes, closed) <- atomically (fetchChunk insertCommentBatchSize (readTBMQueue hashesToCheckQ))
            Debug.debugM Debug.HistoryComments "Checking hashes chunk of size" (length hashes)
            PG.whenNonEmpty hashes $ do
              unknownCommentHashes <- fmap Set.fromList $ PG.runTransaction $ do
                Q.filterForUnknownHistoryCommentHashes (Sync.unHistoryCommentHash32 . fst <$> hashes)
              let (revisionHashesWeDefinitelyNeed, revisionHashesToCheck) =
                    hashes
                      -- Only check revisions for comments that are unknown
                      & foldMap \case
                        (commentHash, revisionHashes)
                          -- If the comment hash is unknown, we need _all_ its revisions, we
                          -- don't need to check them.
                          -- Otherwise, we need to check which revisions are unknown.
                          | Set.member (Sync.unHistoryCommentHash32 commentHash) unknownCommentHashes -> (revisionHashes, [])
                          | otherwise -> ([], revisionHashes)
              unknownRevsFiltered <- PG.runTransaction $ Q.filterForUnknownHistoryCommentRevisionHashes projectId (Sync.unHistoryCommentRevisionHash32 <$> revisionHashesToCheck)
              let allNeededHashes =
                    (Set.map (Left . Sync.HistoryCommentHash32) unknownCommentHashes)
                      <> (Set.fromList . fmap Right $ revisionHashesWeDefinitelyNeed)
                      <> (Set.fromList (Right . Sync.HistoryCommentRevisionHash32 <$> unknownRevsFiltered))
              case NESet.nonEmptySet allNeededHashes of
                Nothing -> pure ()
                Just unknownHashesSet -> do
                  Debug.debugM Debug.HistoryComments "Requesting unknown hashes" unknownHashesSet
                  void . atomically $ send $ Msg $ RequestCommentsChunk unknownHashesSet
            when closed $ Debug.debugLogM Debug.HistoryComments "Hash checking worker: hashes queue closed"
            when (not closed) loop
      loop
      void . atomically $ send $ Msg $ DoneCheckingHashesChunk
      Debug.debugLogM Debug.HistoryComments "Hash checking worker finished"
    receiverWorker :: STM (Maybe (MsgOrError Void HistoryCommentUploaderChunk)) -> TMVar Text -> TBMQueue (Sync.HistoryCommentHash32, [Sync.HistoryCommentRevisionHash32]) -> TBMQueue (Either Sync.HistoryComment Sync.HistoryCommentRevision) -> WebApp ()
    receiverWorker recv errMVar hashesToCheckQ commentsQ = do
      let loop = do
            next <- atomically do
              recv >>= \case
                Nothing -> do
                  Debug.debugLogM Debug.HistoryComments "Receiver worker: connection closed"
                  closeTBMQueue hashesToCheckQ
                  closeTBMQueue commentsQ
                  pure (pure ())
                Just (DeserialiseFailure err) -> do
                  Debug.debugM Debug.HistoryComments "Receiver worker: deserialisation failure" err
                  putTMVar errMVar err
                  pure (pure ())
                Just (Msg msg) -> do
                  case msg of
                    PossiblyNewHashesChunk hashesToCheck -> do
                      for_ hashesToCheck $ \h -> writeTBMQueue hashesToCheckQ h
                    DoneSendingHashesChunk -> do
                      closeTBMQueue hashesToCheckQ
                    HistoryCommentChunk comment -> do
                      writeTBMQueue commentsQ (Left comment)
                    HistoryCommentRevisionChunk revision -> do
                      writeTBMQueue commentsQ (Right revision)
                  pure loop
            next
      loop
      Debug.debugLogM Debug.HistoryComments "Receiver worker finished"
    insertCommentBatchSize = 100

handleErrInQueue :: forall o x e a. Queues (MsgOrError e a) o -> e -> ExceptT e WebApp x
handleErrInQueue Queues {send} e = do
  _ <- atomically $ send $ UserErr e
  throwError e

-- Read all available values from a TBMQueue, returning them and whether the queue is closed.
flushTBMQueue :: TBMQueue a -> STM ([a], Bool)
flushTBMQueue q = do
  optional (readTBMQueue q) >>= \case
    -- No values available
    Nothing -> empty
    Just Nothing -> do
      -- Queue closed
      pure ([], True)
    Just (Just v) -> do
      (vs, closed) <- flushTBMQueue q <|> pure ([], False)
      pure (v : vs, closed)
