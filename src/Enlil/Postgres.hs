{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Postgres helpers
module Enlil.Postgres
  ( -- * Types
    Transaction,
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
    QueryM (..),
    decodeField,
    (:.) (..),

    -- * Running Transactions and Sessions
    readTransaction,
    writeTransaction,
    runTransaction,
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
    transactionUnsafeIO,
    defaultIsolationLevel,

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
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 qualified as BSC
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Void
import Enlil.App
import Enlil.Env qualified as Env
import Enlil.Postgres.Orphans ()
import Enlil.Prelude
import Enlil.Utils.Logging (Loggable (..))
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.App
import Enlil.Web.Errors (ErrorID (..), SomeServerError, ToServerError (..), internalServerError, respondError, someServerError)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Interp
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql

debug :: Bool
debug = False

data TransactionError e
  = Unrecoverable SomeServerError
  | Err e

-- | A transaction that may fail with an error 'e' (or throw an unrecoverable error)
newtype Transaction e a = Transaction {unTransaction :: Hasql.Session (Either (TransactionError e) a)}
  deriving (Functor, Applicative, Monad, MonadIO) via (ExceptT (TransactionError e) Hasql.Session)

instance MonadError e (Transaction e) where
  throwError = Transaction . pure . Left . Err
  catchError (Transaction t) f = Transaction do
    t >>= \case
      Left (Err e) -> unTransaction (f e)
      Left (Unrecoverable err) -> pure (Left (Unrecoverable err))
      Right a -> pure (Right a)

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

transactionUnsafeIO :: IO a -> Transaction e a
transactionUnsafeIO io = Transaction (Right <$> liftIO io)

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
runSessionWithPool :: HasCallStack => Pool.Pool -> Session Void a -> IO a
runSessionWithPool pool s = either absurd id <$> tryRunSessionWithPool pool s

-- | Manually run a session using a connection pool, returning an Either error.
tryRunSessionWithPool :: HasCallStack => Pool.Pool -> Session e a -> IO (Either e a)
tryRunSessionWithPool pool s = do
  liftIO (Pool.use pool (runExceptT s)) >>= \case
    Left err -> throwIO . someServerError $ PostgresError err
    Right (Left (Unrecoverable e)) -> throwIO e
    Right (Left (Err e)) -> pure (Left e)
    Right (Right a) -> pure (Right a)

-- | Run a session in the App monad, responding to the request with an error if it fails.
runSessionOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => Session e a -> WebApp a
runSessionOrRespondError t = tryRunSession t >>= either respondError pure

-- | Represents any monad in which we can run a statement
class Monad m => QueryM m where
  statement :: q -> Hasql.Statement q r -> m r

  -- | Fail the transaction and whole request with an unrecoverable server error.
  unrecoverableError :: (HasCallStack, ToServerError e, Loggable e, Show e) => e -> m a

instance QueryM (Transaction e) where
  statement q s@(Hasql.Statement bs _ _ _) = do
    when debug $ transactionUnsafeIO $ BSC.putStrLn bs
    transactionStatement q s

  unrecoverableError e = Transaction (pure (Left (Unrecoverable (someServerError e))))

instance QueryM (Session e) where
  statement q s@(Hasql.Statement bs _ _ _) = do
    when debug $ liftIO $ BSC.putStrLn bs
    lift $ Session.statement q s

  unrecoverableError e = throwError (Unrecoverable (someServerError e))

instance QueryM m => QueryM (ReaderT e m) where
  statement q s = lift $ statement q s

  unrecoverableError e = lift $ unrecoverableError e

instance QueryM m => QueryM (MaybeT m) where
  statement q s = lift $ statement q s

  unrecoverableError e = lift $ unrecoverableError e

prepareStatements :: Bool
prepareStatements = True

queryListRows :: forall r m. (Interp.DecodeRow r, QueryM m) => Interp.Sql -> m [r]
queryListRows sql = statement () (Interp.interp prepareStatements sql)

query1Row :: forall r m. QueryM m => (Interp.DecodeRow r) => Interp.Sql -> m (Maybe r)
query1Row sql = listToMaybe <$> queryListRows sql

query1Col :: forall a m. (QueryM m, Interp.DecodeField a) => Interp.Sql -> m (Maybe a)
query1Col sql = listToMaybe <$> queryListCol sql

queryListCol :: forall a m. QueryM m => (Interp.DecodeField a) => Interp.Sql -> m [a]
queryListCol sql = queryListRows @(Interp.OneColumn a) sql <&> coerce @[Interp.OneColumn a] @[a]

execute_ :: QueryM m => Interp.Sql -> m ()
execute_ sql = statement () (Interp.interp prepareStatements sql)

queryExpect1Row :: forall r m. HasCallStack => (Interp.DecodeRow r, QueryM m) => Interp.Sql -> m r
queryExpect1Row sql =
  query1Row sql >>= \case
    Nothing -> error "queryExpect1Row: expected 1 row, got 0"
    Just r -> pure r

queryExpect1Col :: forall a m. HasCallStack => (Interp.DecodeField a, QueryM m) => Interp.Sql -> m a
queryExpect1Col sql =
  query1Col sql >>= \case
    Nothing -> error "queryExpect1Col: expected 1 row, got 0"
    Just r -> pure r

-- | Decode a single field as part of a Row
decodeField :: Interp.DecodeField a => Decoders.Row a
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

instance Interp.DecodeField a => Interp.DecodeRow (Only a) where
  decodeRow = Only <$> decodeField

likeEscape :: Text -> Text
likeEscape = Text.replace "%" "\\%" . Text.replace "_" "\\_"

-- | Helper for encoding a single-column table using PG.toTable.
--
-- The @EncodeRow (Only a)@ instance might seem strange, but without it we get overlapping
-- instances on @EncodeValue a@.
singleColumnTable :: forall a. Interp.EncodeRow (Only a) => [a] -> Interp.Sql
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
