{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Postgres helpers
module Share.Postgres
  ( -- * Types
    Transaction,
    Pipeline,
    T,
    Session,
    Mode (..),
    IsolationLevel (..),
    Interp.EncodeValue (..),
    Interp.EncodeRow (..),
    Interp.DecodeValue (..),
    Interp.DecodeRow (..),
    Interp.DecodeField,
    RawBytes (..),
    Only (..),
    QueryA (..),
    QueryM (..),
    unrecoverableError,
    throwErr,
    decodeField,
    (:.) (..),

    -- * Running Transactions and Sessions
    readTransaction,
    writeTransaction,
    runTransaction,
    runTransactionMode,
    tryRunTransaction,
    tryRunTransactionMode,
    unliftTransaction,
    runTransactionOrRespondError,
    runSession,
    tryRunSession,
    runSessionOrRespondError,
    runSessionWithPool,
    tryRunSessionWithPool,
    unliftSession,
    defaultIsolationLevel,
    pFor,
    pFor_,

    -- * query Helpers
    rollback,
    queryListRows,
    query1Row,
    queryExpect1Row,
    queryListCol,
    query1Col,
    queryExpect1Col,
    execute_,
    likeEscape,
    cachedFor,
    cachedForOf,
    whenNonEmpty,

    -- * Interpolation
    Interp.sql,
    Interp.toTable,
    Interp.Sql,
    singleColumnTable,

    -- * Debugging
    timeTransaction,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Compose (Compose (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Void
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Interp
import Hasql.Pipeline qualified as Hasql.Pipeline
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Share.App
import Share.Debug qualified as Debug
import Share.Env qualified as Env
import Share.Postgres.Orphans ()
import Share.Prelude
import Share.Utils.Logging (Loggable (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.App
import Share.Web.Errors (ErrorID (..), SomeServerError, ToServerError (..), internalServerError, respondError, someServerError)
import System.CPUTime (getCPUTime)

data TransactionError e
  = Unrecoverable SomeServerError
  | Err e

-- | A transaction that may fail with an error 'e' (or throw an unrecoverable error)
newtype Transaction e a = Transaction {unTransaction :: Hasql.Session (Either (TransactionError e) a)}
  deriving (Functor, Applicative, Monad) via (ExceptT (TransactionError e) Hasql.Session)

instance MonadError e (Transaction e) where
  throwError = Transaction . pure . Left . Err
  catchError (Transaction t) f = Transaction do
    t >>= \case
      Left (Err e) -> unTransaction (f e)
      Left (Unrecoverable err) -> pure (Left (Unrecoverable err))
      Right a -> pure (Right a)

-- | Applicative pipelining transaction
newtype Pipeline e a = Pipeline {_unPipeline :: Hasql.Pipeline.Pipeline (Either (TransactionError e) a)}
  deriving (Functor, Applicative) via (Compose Hasql.Pipeline.Pipeline (Either (TransactionError e)))

pFor :: (Traversable f) => f a -> (a -> Pipeline e b) -> Transaction e (f b)
pFor f p = pipelined $ for f p

pFor_ :: (Foldable f) => f a -> (a -> Pipeline e b) -> Transaction e ()
pFor_ f p = pipelined $ for_ f p

-- | A transaction that has no errors. Prefer using a fully polymorphic 'e' when possible,
-- but this is very helpful when dealing with data type which include fields which are
-- loaded on-demand from the DB.
type T = Transaction Void

-- | A session that may fail with an error 'e'
type Session e = ExceptT (TransactionError e) Hasql.Session

data PostgresError
  = PostgresError (Pool.UsageError)
  deriving stock (Show)
  deriving anyclass (Exception)

instance ToServerError PostgresError where
  toServerError (PostgresError err) =
    let errId = case err of
          Pool.ConnectionUsageError {} -> "connection-usage-error"
          Pool.SessionUsageError {} -> "session-usage-error"
          Pool.AcquisitionTimeoutUsageError {} -> "acquisition-timeout-usage-error"
     in (ErrorID $ "postgres:pool:" <> errId, internalServerError)

instance Logging.Loggable PostgresError where
  toLog (PostgresError err) =
    Logging.showLog err
      & Logging.withSeverity Logging.Error

-- TODO: I think we want to vary this per transaction.
defaultIsolationLevel :: IsolationLevel
defaultIsolationLevel = Serializable

data Mode = Read | ReadWrite deriving stock (Eq, Show)

data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving stock (Show, Eq)

-- | Run a transaction in a session
transaction :: forall e a. IsolationLevel -> Mode -> Transaction e a -> Session e a
transaction isoLevel mode (Transaction t) = do
  let loop :: Session.Session (Either (TransactionError e) a)
      loop = do
        beginTransaction isoLevel mode
        res <- catchError (Just <$> mayCommit t) \case
          Session.QueryError
            _
            _
            ( Session.ResultError
                (Session.ServerError errCode _ _ _ _)
              )
              -- retry on serialization failure or deadlock
              -- https://www.postgresql.org/docs/current/errcodes-appendix.html
              | errCode == "40001" || errCode == "40P01" -> pure Nothing
          err -> do
            -- Try rolling back, but this will most likely fail since the connection has
            -- already failed. If this fails we just rethrow the original exception.
            catchError rollbackSession (const $ throwError err)
            -- It's very important to rethrow all QueryErrors so Hasql can remove the
            -- connection from the pool.
            throwError err
        case res of
          Nothing -> do
            rollbackSession
            loop
          Just res -> pure res
  ExceptT loop
  where
    mayCommit :: Hasql.Session (Either (TransactionError e) a) -> Hasql.Session (Either (TransactionError e) a)
    mayCommit m =
      m >>= \case
        Left err -> do
          rollbackSession
          pure (Left err)
        Right a -> do
          commit
          pure (Right a)

beginTransaction :: IsolationLevel -> Mode -> Hasql.Session ()
beginTransaction hiso hmode =
  Session.statement () (Interp.interp False [Interp.sql| BEGIN ISOLATION LEVEL ^{iso} ^{mode} |])
  where
    iso =
      case hiso of
        ReadCommitted -> [Interp.sql| READ COMMITTED |]
        RepeatableRead -> [Interp.sql| REPEATABLE READ |]
        Serializable -> [Interp.sql| SERIALIZABLE |]
    mode =
      case hmode of
        ReadWrite -> [Interp.sql| READ WRITE |]
        Read -> [Interp.sql| READ ONLY |]

commit :: Hasql.Session ()
commit = Session.statement () (Hasql.Statement "commit" Encoders.noParams Decoders.noResult True)

rollbackSession :: Hasql.Session ()
rollbackSession = Session.statement () (Hasql.Statement "rollback" Encoders.noParams Decoders.noResult True)

-- | Rollback the current transaction
rollback :: e -> Transaction e x
rollback e = Transaction do
  pure (Left (Err e))

transactionStatement :: a -> Hasql.Statement a b -> Transaction e b
transactionStatement v stmt = Transaction do
  Right <$> Session.statement v stmt

-- | Run a read-only transaction within a session
readTransaction :: Transaction e a -> Session e a
readTransaction t = transaction defaultIsolationLevel Read t

-- | Run a write transaction within a session
writeTransaction :: Transaction e a -> Session e a
writeTransaction t = transaction defaultIsolationLevel ReadWrite t

-- -- | Run a write transaction within a session
-- writeTransaction :: Transaction e a -> Session e a
-- writeTransaction t = ExceptT $ unTransaction t

-- | Run a transaction that doesn't throw errors in the App monad.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
runTransaction :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => Transaction Void a -> m a
runTransaction t = runSession (writeTransaction t)

runTransactionMode :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => IsolationLevel -> Mode -> Transaction Void a -> m a
runTransactionMode isoLevel mode t = runSession (transaction isoLevel mode t)

-- | Run a transaction in the App monad, returning an Either error.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
tryRunTransaction :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => Transaction e a -> m (Either e a)
tryRunTransaction t = tryRunSession (writeTransaction t)

tryRunTransactionMode :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => IsolationLevel -> Mode -> Transaction e a -> m (Either e a)
tryRunTransactionMode isoLevel mode t = tryRunSession (transaction isoLevel mode t)

-- | Run a transaction in the App monad, responding to the request with an error if it fails.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
runTransactionOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => Transaction e a -> WebApp a
runTransactionOrRespondError t = runSessionOrRespondError (writeTransaction t)

-- | Unlift a transaction to run in IO.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
unliftTransaction :: Transaction e a -> AppM x (IO (Either e a))
unliftTransaction t = unliftSession (writeTransaction t)

-- | Run a session in the App monad without any errors.
runSession :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => Session Void a -> m a
runSession t = either absurd id <$> tryRunSession t

-- | Run a session in the App monad, returning an Either error.
tryRunSession :: (MonadReader (Env.Env x) m, MonadIO m, HasCallStack) => Session e a -> m (Either e a)
tryRunSession s = do
  pool <- asks Env.pgConnectionPool
  liftIO $ tryRunSessionWithPool pool s

-- | Unlift a session to run in IO.
unliftSession :: Session e a -> AppM x (IO (Either e a))
unliftSession s = do
  pool <- asks Env.pgConnectionPool
  pure $ tryRunSessionWithPool pool s

-- | Manually run an unfailing session using a connection pool.
runSessionWithPool :: (HasCallStack) => Pool.Pool -> Session Void a -> IO a
runSessionWithPool pool s = either absurd id <$> tryRunSessionWithPool pool s

-- | Manually run a session using a connection pool, returning an Either error.
tryRunSessionWithPool :: (HasCallStack) => Pool.Pool -> Session e a -> IO (Either e a)
tryRunSessionWithPool pool s = do
  liftIO (Pool.use pool (runExceptT s)) >>= \case
    Left err -> throwIO . someServerError $ PostgresError err
    Right (Left (Unrecoverable e)) -> throwIO e
    Right (Left (Err e)) -> pure (Left e)
    Right (Right a) -> pure (Right a)

-- | Run a session in the App monad, responding to the request with an error if it fails.
runSessionOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => Session e a -> WebApp a
runSessionOrRespondError t = tryRunSession t >>= either respondError pure

-- | Represents anywhere we can run a statement
class (Applicative m) => QueryA m e | m -> e where
  statement :: q -> Hasql.Statement q r -> m r

  -- | Fail the transaction and whole request with an unrecoverable server error.
  unrecoverableErrorA :: (HasCallStack, ToServerError x, Loggable x, Show x) => m (Either x a) -> m a

  throwErrA :: (ToServerError e, Loggable e, Show e) => m (Either e a) -> m a

  pipelined :: Pipeline e a -> m a

class (Monad m, QueryA m e) => QueryM m e | m -> e where
  -- | Allow running IO actions in a transaction. These actions may be run multiple times if
  -- the transaction is retried.
  transactionUnsafeIO :: IO a -> m a

instance QueryA (Transaction e) e where
  statement q s = do
    transactionStatement q s

  throwErrA m = m >>= either throwError pure

  pipelined (Pipeline p) = Transaction (Hasql.pipeline p)

  unrecoverableErrorA me =
    me >>= \case
      Right a -> pure a
      Left e -> Transaction (pure (Left (Unrecoverable (someServerError e))))

instance QueryM (Transaction e) e where
  transactionUnsafeIO io = Transaction (Right <$> liftIO io)

instance QueryA (Session e) e where
  statement q s = do
    lift $ Session.statement q s

  throwErrA m = m >>= either (throwError . Err) pure

  pipelined (Pipeline p) = do
    ExceptT $ Hasql.pipeline p

  unrecoverableErrorA me =
    me >>= \case
      Right a -> pure a
      Left e -> throwError (Unrecoverable (someServerError e))

instance QueryM (Session e) e where
  transactionUnsafeIO io = lift $ liftIO io

instance QueryA (Pipeline e) e where
  statement q s = Pipeline (Right <$> Hasql.Pipeline.statement q s)

  throwErrA (Pipeline me) =
    -- Flatten error into pipeline
    Pipeline $
      me <&> \case
        Left e -> Left e
        Right (Left e) -> Left (Err e)
        Right (Right a) -> Right a

  pipelined p = p

  unrecoverableErrorA (Pipeline me) =
    Pipeline
      ( me <&> \case
          Right (Left e) -> Left . Unrecoverable . someServerError $ e
          Right (Right a) -> Right a
          Left e -> Left e
      )

-- Pipeline $ pure (Left (Unrecoverable (someServerError e)))

instance (QueryM m e) => QueryA (ReaderT r m) e where
  statement q s = lift $ statement q s

  throwErrA m = mapReaderT throwErrA m

  pipelined p = lift $ pipelined p

  unrecoverableErrorA me = mapReaderT unrecoverableErrorA me

instance (QueryM m e) => QueryM (ReaderT r m) e where
  transactionUnsafeIO io = lift $ transactionUnsafeIO io

instance (QueryM m e) => QueryA (MaybeT m) e where
  statement q s = lift $ statement q s

  throwErrA m =
    m >>= \case
      Left e -> lift $ throwErr e
      Right a -> pure a

  pipelined p = lift $ pipelined p

  unrecoverableErrorA m =
    m >>= \case
      Left e -> lift $ unrecoverableError e
      Right a -> pure a

instance (QueryM m e) => QueryM (MaybeT m) e where
  transactionUnsafeIO io = lift $ transactionUnsafeIO io

unrecoverableError :: (QueryA m e) => (ToServerError x, Loggable x, Show x) => x -> m a
unrecoverableError e = unrecoverableErrorA (pure $ Left e)

throwErr :: (QueryA m e, ToServerError e, Loggable e, Show e) => e -> m a
throwErr e = throwErrA (pure $ Left e)

prepareStatements :: Bool
prepareStatements = True

queryListRows :: forall r m e. (Interp.DecodeRow r, QueryA m e) => Interp.Sql -> m [r]
queryListRows sql = statement () (Interp.interp prepareStatements sql)

query1Row :: forall r m e. (QueryA m e) => (Interp.DecodeRow r) => Interp.Sql -> m (Maybe r)
query1Row sql = listToMaybe <$> queryListRows sql

query1Col :: forall a m e. (QueryA m e, Interp.DecodeField a) => Interp.Sql -> m (Maybe a)
query1Col sql = listToMaybe <$> queryListCol sql

queryListCol :: forall a m e. (QueryA m e) => (Interp.DecodeField a) => Interp.Sql -> m [a]
queryListCol sql = queryListRows @(Interp.OneColumn a) sql <&> coerce @[Interp.OneColumn a] @[a]

execute_ :: (QueryA m e) => Interp.Sql -> m ()
execute_ sql = statement () (Interp.interp prepareStatements sql)

queryExpect1Row :: forall r m e. (HasCallStack) => (Interp.DecodeRow r, QueryM m e) => Interp.Sql -> m r
queryExpect1Row sql =
  query1Row sql >>= \case
    Nothing -> error "queryExpect1Row: expected 1 row, got 0"
    Just r -> pure r

queryExpect1Col :: forall a m e. (HasCallStack) => (Interp.DecodeField a, QueryM m e) => Interp.Sql -> m a
queryExpect1Col sql =
  query1Col sql >>= \case
    Nothing -> error "queryExpect1Col: expected 1 row, got 0"
    Just r -> pure r

-- | Decode a single field as part of a Row
decodeField :: (Interp.DecodeField a) => Decoders.Row a
decodeField = Decoders.column Interp.decodeField

-- | Helper for decoding a row which contains many types which each have their own DecodeRow
-- instance.
--
-- E.g. queryExpect1Row userAndProjectSql <&> \(user :. project) -> (user, project)
data a :. b = a :. b
  deriving (Show)

infixr 3 :.

instance (Interp.DecodeRow a, Interp.DecodeRow b) => Interp.DecodeRow (a :. b) where
  decodeRow = (:.) <$> Interp.decodeRow <*> Interp.decodeRow

-- | Helper for decoding a single column when using (:.), or encoding a single-column table using PG.toTable.
-- You shouldn't usually need it otherwise.
--
-- E.g. queryExpect1Col userAndProjectSql <&> \(user :. Only projectCount) -> (user, projectCount)
newtype Only a = Only {fromOnly :: a}
  deriving (Show, Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Interp.EncodeRow)

instance (Interp.DecodeField a) => Interp.DecodeRow (Only a) where
  decodeRow = Only <$> decodeField

likeEscape :: Text -> Text
likeEscape = Text.replace "%" "\\%" . Text.replace "_" "\\_"

-- | Helper for encoding a single-column table using PG.toTable.
--
-- The @EncodeRow (Only a)@ instance might seem strange, but without it we get overlapping
-- instances on @EncodeValue a@.
singleColumnTable :: forall a. (Interp.EncodeRow (Only a)) => [a] -> Interp.Sql
singleColumnTable cols = Interp.toTable (coerce @[a] @[Only a] cols)

-- | Helper for looking things up, using a cache for keys we've already seen (within the same
-- traversal)
--
-- E.g. if you have:
--
-- @@
-- data Contribution user = Contribution
--   { createdBy :: user
--   , updatedBy :: user
--   ...
--   }
-- @@
--
-- Then @cachedForOf traversed myUsers Q.userById@ will only query the database once if each user
-- is the same.
--
-- >>> cachedForOf traversed [1, 2, 1, 3] (\i -> print ("fetching: ", i) *> pure i)
cachedForOf :: (Monad m, Ord a) => Traversal s t a b -> s -> (a -> m b) -> m t
cachedForOf trav s f = do
  flip evalStateT mempty $ do
    forOf trav s \a -> do
      gets (Map.lookup a) >>= \case
        Nothing -> do
          b <- lift $ f a
          modify' (Map.insert a b)
          pure b
        Just b -> pure b

-- | cachedForOf, but for traversables.
cachedFor :: (Traversable t, Monad m, Ord a) => t a -> (a -> m b) -> m (t b)
cachedFor = cachedForOf traversed

-- | Preferably you should use custom newtypes for your bytes, but you can use this with
-- deriving via to get the encoding/decoding instances.
newtype RawBytes = RawBytes {unRawBytes :: ByteString}
  deriving stock (Show, Eq, Ord)

instance Interp.EncodeValue RawBytes where
  encodeValue = contramap unRawBytes Encoders.bytea

instance Interp.DecodeValue RawBytes where
  decodeValue = RawBytes <$> Decoders.bytea

-- | Useful when running queries using a join over `toTable` which may be empty.
-- Without explicitly handling the empty case we'll waste time sending a query to PG
-- that we know can't return any results.
--
-- E.g.
--
-- @@
--   whenNonEmpty usersTable $ do
--     queryListRows [sql|
--       WITH users (id) AS (
--         SELECT * FROM ^{toTable usersTable})
--      ) SELECT * FROM something JOIN users on something.user_id = users.id
--     |]
-- @@
whenNonEmpty :: forall m f a x. (Monad m, Foldable f, Monoid a) => f x -> m a -> m a
whenNonEmpty f m = if null f then pure mempty else m

timeTransaction :: (QueryM m e) => String -> m a -> m a
timeTransaction label ma =
  if Debug.shouldDebug Debug.Timing
    then do
      transactionUnsafeIO $ putStrLn $ "Timing Transaction: " ++ label ++ "..."
      systemStart <- transactionUnsafeIO getSystemTime
      cpuPicoStart <- transactionUnsafeIO getCPUTime
      !a <- ma
      cpuPicoEnd <- transactionUnsafeIO getCPUTime
      systemEnd <- transactionUnsafeIO getSystemTime
      let systemDiff = diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart)
      let cpuDiff = picosecondsToDiffTime (cpuPicoEnd - cpuPicoStart)
      transactionUnsafeIO $ putStrLn $ "Finished " ++ label ++ " in " ++ show cpuDiff ++ " (cpu), " ++ show systemDiff ++ " (system)"
      pure a
    else ma
