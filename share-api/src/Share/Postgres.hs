{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- For the empty constraint introduced by DebugCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Postgres helpers
module Share.Postgres
  ( -- * Types
    Transaction,
    UnliftIOTransaction (..),
    Pipeline,
    T,
    Session (..),
    Mode (..),
    IsolationLevel (..),
    Interp.EncodeValue (..),
    Interp.EncodeRow (..),
    Interp.DecodeValue (..),
    Interp.DecodeRow (..),
    Interp.DecodeField,
    DecodeComposite (..),
    Only (..),
    TupleVal (..),
    QueryA (..),
    QueryM (..),
    TransactionError (..),
    decodeField,
    (:.) (..),

    -- * Running Transactions and Sessions
    readTransaction,
    writeTransaction,
    runTransaction,
    runTransactionMode,
    tryRunTransaction,
    tryRunTransactionMode,
    catchTransaction,
    catchAllTransaction,
    runTransactionOrRespondError,
    runTransactionModeOrRespondError,
    transaction,
    runSession,
    tryRunSession,
    runSessionOrRespondError,
    runSessionWithEnv,
    tryRunSessionWithEnv,
    defaultIsolationLevel,
    pEitherMap,
    pFor,
    pFor_,

    -- * query Helpers
    rollback,
    queryListRows,
    queryVectorRows,
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

    -- * Debugging and observability
    timeTransaction,
    transactionSpan,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Lazy qualified as HM
import Data.Kind (Constraint, Type)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Vector (Vector)
import Data.Void
import GHC.Exception (SrcLoc (srcLocModule))
import GHC.Stack qualified as Stack
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Hasql.Interpolate qualified as Interp
import Hasql.Pipeline qualified as Hasql.Pipeline
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import OpenTelemetry.Context qualified as Trace
import OpenTelemetry.Context.ThreadLocal qualified as Trace
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Safe (headMay)
import Share.Debug qualified as Debug
import Share.Env.Types qualified as Env
import Share.Postgres.Composites
import Share.Postgres.Orphans ()
import Share.Prelude
import Share.Telemetry qualified as Trace
import Share.Utils.Logging (Loggable (..))
import Share.Utils.Logging qualified as Logging
import Share.Utils.Postgres (likeEscape)
import Share.Utils.Tags (HasTags (..), MonadTags (..))
import Share.Web.App
import Share.Web.Errors (ErrorID (..), SomeServerError (SomeServerError), ToServerError (..), internalServerError, respondError, someServerError)
import System.CPUTime (getCPUTime)
import UnliftIO qualified

#ifdef QUERY_SPANS
type DebugCallStack = HasCallStack :: Constraint
#else
type DebugCallStack = () :: Constraint
#endif

-- | Returns the name of the function which contains the transaction call.
-- This is useful when qualifying queries, e.g. "getUsers:queryListCol"
getQualifiedExternalCallsite :: (HasCallStack) => Maybe (String, Stack.SrcLoc)
getQualifiedExternalCallsite =
  cs
    & dropWhile (\(_, Stack.SrcLoc {srcLocModule}) -> srcLocModule == thisModuleName)
    & \case
      ((innerFunc, innerCallstack) : (outerFunc, _) : _) -> Just (innerFunc <> ":" <> outerFunc, innerCallstack)
      [(func, funcCallstack)] -> Just (func, funcCallstack)
      [] -> Nothing
  where
    cs = Stack.getCallStack Stack.callStack

-- | Get span info which includes the function name and call site of the first call site
-- that's NOT in the Postgres module.
spanInfo :: (HasCallStack) => (Text, Trace.AttributeMap)
spanInfo =
  ( maybe "<unknown>" (Text.pack . fst) cs,
    HM.fromList $
      [ ("loc.callSite", maybe "<unknown-callsite>" (Trace.toAttribute . prettySrcLoc) cs)
      ]
  )
  where
    cs = getQualifiedExternalCallsite

thisModuleName :: (HasCallStack) => String
thisModuleName =
  Stack.getCallStack Stack.callStack
    & headMay
    & fromMaybe (error "getModuleEntrypoint: no module name found in call stack")
    & \(_, Stack.SrcLoc {srcLocModule}) -> srcLocModule

data TransactionError e
  = Unrecoverable SomeServerError
  | Err !e

newtype Tags = Tags (Map Text Text)

instance HasTags Tags where
  getTags (Tags tags) = pure $ tags
  addTags newTags (Tags tags) = Tags (tags <> newTags)

data TransactionCtx = TransactionCtx
  { tags :: Tags,
    -- Track the umber of queries run within a transaction for span metadata
    numQueriesVar :: UnliftIO.TVar Int
  }

instance HasTags TransactionCtx where
  getTags TransactionCtx {tags} = getTags tags
  addTags newTags TransactionCtx {tags = Tags tags, numQueriesVar} =
    TransactionCtx {tags = Tags (addTags tags newTags), numQueriesVar}

-- | A transaction that may fail with an error 'e' (or throw an unrecoverable error)
newtype Transaction e a = Transaction {unTransaction :: Logging.LoggerT (ReaderT (Env.Env TransactionCtx) Hasql.Session) (Either (TransactionError e) a)}
  deriving
    (Functor, Applicative, Monad, MonadReader (Env.Env TransactionCtx), Logging.MonadLogger)
    via (Logging.LoggerT (ReaderT (Env.Env TransactionCtx) (ExceptT (TransactionError e) Hasql.Session)))

instance MonadTags (Transaction e) where
  askTags = ask >>= transactionUnsafeIO . getTags
  withTags newTags (Transaction t) = Transaction $ do
    local (addTags newTags) t

instance MonadTracer (Transaction e) where
  getTracer = asks Env.tracer

-- | A very annoying type we must define so that we can embed transactions in IO for the
-- Unison Runtime. You really shouldn't use this unless you absolutely need the MonadUnliftIO
-- class for a PG transaction.
newtype UnliftIOTransaction e a = UnliftIOTransaction {asUnliftIOTransaction :: Transaction e a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env.Env TransactionCtx), Logging.MonadLogger, MonadTracer)

instance MonadIO (UnliftIOTransaction e) where
  liftIO io = UnliftIOTransaction . Transaction $ Right <$> liftIO io

instance (Exception e) => MonadUnliftIO (UnliftIOTransaction e) where
  withRunInIO f = UnliftIOTransaction $ Transaction $ do
    unliftIO <- UnliftIO.askUnliftIO
    r <- UnliftIO.try $ liftIO $ f \(UnliftIOTransaction (Transaction m)) -> do
      case unliftIO of
        UnliftIO.UnliftIO toIO -> do
          toIO m >>= \case
            Left (Unrecoverable err) -> throwIO err
            Left (Err e) -> throwIO e
            Right a -> pure a
    case r of
      Left err@(SomeServerError {}) -> pure (Left (Unrecoverable err))
      Right b -> pure $ Right b

instance MonadError e (Transaction e) where
  throwError = Transaction . pure . Left . Err
  catchError (Transaction t) f = Transaction do
    t >>= \case
      Left (Err e) -> unTransaction (f e)
      Left (Unrecoverable err) -> pure (Left (Unrecoverable err))
      Right a -> pure (Right a)

-- | Applicative pipelining transaction
newtype Pipeline e a = Pipeline {unPipeline :: Hasql.Pipeline.Pipeline (Either (TransactionError e) a)}
  deriving (Functor, Applicative) via (Compose Hasql.Pipeline.Pipeline (Either (TransactionError e)))

-- | Like fmap, but the provided function can throw a recoverable error by returning 'Left'.
pEitherMap :: (a -> Either e b) -> Pipeline e a -> Pipeline e b
pEitherMap f (Pipeline p) =
  Pipeline $
    p <&> \case
      Right x -> mapLeft Err (f x)
      Left e -> Left e

pFor :: (Traversable f) => f a -> (a -> Pipeline e b) -> Transaction e (f b)
pFor f p = pipelined $ for f p

pFor_ :: (Foldable f) => f a -> (a -> Pipeline e b) -> Transaction e ()
pFor_ f p = pipelined $ for_ f p

-- | A transaction that has no errors. Prefer using a fully polymorphic 'e' when possible,
-- but this is very helpful when dealing with data type which include fields which are
-- loaded on-demand from the DB.
type T = Transaction Void

-- | A session that may fail with an error 'e'
newtype Session e a = Session {_unSession :: Logging.LoggerT (ReaderT (Env.Env TransactionCtx) (ExceptT (TransactionError e) Hasql.Session)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env.Env TransactionCtx), MonadIO, Logging.MonadLogger, MonadError (TransactionError e))

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
transaction isoLevel mode (Transaction t) = Session do
  let loop :: Logging.LoggerT (ReaderT (Env.Env TransactionCtx) Session.Session) (Either (TransactionError e) a)
      loop = do
        lift . lift $ beginTransaction isoLevel mode
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
            lift . lift $ catchError rollbackSession (const $ throwError err)
            -- It's very important to rethrow all QueryErrors so Hasql can remove the
            -- connection from the pool.
            throwError err
        case res of
          Nothing -> do
            lift . lift $ rollbackSession
            local (addTags $ Map.singleton "transaction-retry" "true") loop
          Just res -> pure res
  coerce loop
  where
    mayCommit :: Logging.LoggerT (ReaderT (Env.Env TransactionCtx) Hasql.Session) (Either (TransactionError e) a) -> Logging.LoggerT (ReaderT (Env.Env TransactionCtx) Hasql.Session) (Either (TransactionError e) a)
    mayCommit m =
      m >>= \case
        Left err -> do
          lift . lift $ rollbackSession
          pure (Left err)
        Right a -> do
          lift . lift $ commit
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

-- Ormolu doesn't like CPP
{- ORMOLU_DISABLE -}
-- | This is called on every single query, so it needs to be fast.
-- However, it's really nice to track exactly which queries are being run (and how long they
-- each take)
-- for debugging and optimization purposes, so we conditionally track a span here
-- depending on a preprocessor flag.
-- See package.yaml to enable it.
transactionStatement ::
  DebugCallStack =>
  a ->
  Hasql.Statement a b ->
  Transaction e b
transactionStatement v stmt = do
#ifdef QUERY_SPANS
  let (funcName, spanTags) = spanInfo
   in transactionSpan funcName spanTags $ do
#endif
      Transaction do
          env <- ask
          let nqVar = numQueriesVar . Env.ctx $ env
          liftIO $ UnliftIO.atomically $ UnliftIO.modifyTVar' nqVar (+ 1)
          Right <$> (lift . lift $ (Session.statement v stmt))
{- ORMOLU_ENABLE -}

prettySrcLoc :: (String, SrcLoc) -> Text
prettySrcLoc (funcName, Stack.SrcLoc {Stack.srcLocFile, Stack.srcLocStartLine}) =
  Text.intercalate " : " [Text.pack funcName, Text.pack srcLocFile, tShow srcLocStartLine]

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
runTransaction :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Transaction Void a -> m a
runTransaction t = runSession (writeTransaction t)

runTransactionMode :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => IsolationLevel -> Mode -> Transaction Void a -> m a
runTransactionMode isoLevel mode t = runSession (transaction isoLevel mode t)

-- | Run a transaction in the App monad, returning an Either error.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
tryRunTransaction :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Transaction e a -> m (Either e a)
tryRunTransaction t = tryRunSession (writeTransaction t)

tryRunTransactionMode :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => IsolationLevel -> Mode -> Transaction e a -> m (Either e a)
tryRunTransactionMode isoLevel mode t = tryRunSession (transaction isoLevel mode t)

-- | Run a transaction in the App monad, responding to the request with an error if it fails.
--
-- Uses a Write transaction for simplicity since there's not much
-- benefit in distinguishing transaction types.
runTransactionOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => Transaction e a -> WebApp a
runTransactionOrRespondError t = runSessionOrRespondError (writeTransaction t)

runTransactionModeOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => IsolationLevel -> Mode -> Transaction e a -> WebApp a
runTransactionModeOrRespondError isoLevel mode t = runSessionOrRespondError (transaction isoLevel mode t)

-- | Run a session in the App monad without any errors.
runSession :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Session Void a -> m a
runSession t = either absurd id <$> tryRunSession t

-- | Run a session in the App monad, returning an Either error.
tryRunSession :: (MonadReader (Env.Env ctx) m, HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Session e a -> m (Either e a)
tryRunSession s = do
  env <- ask
  tryRunSessionWithEnv env s

-- | Manually run an unfailing session using the connection pool from the provided env.
runSessionWithEnv :: (HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Env.Env ctx -> Session Void a -> m a
runSessionWithEnv env s = either absurd id <$> tryRunSessionWithEnv env s

-- | Manually run a session, using the connection pool from the provided env, returning an Either error.
tryRunSessionWithEnv :: (HasCallStack, MonadTags m, MonadUnliftIO m, MonadTracer m) => Env.Env ctx -> Session e a -> m (Either e a)
tryRunSessionWithEnv env@(Env.Env {pgConnectionPool = pool, minLogSeverity, logger, timeCache}) (Session s) = flip runReaderT env $ Trace.withSpan' entryPointName spanAttrs $ \span -> do
  tags <- askTags
  numQueriesVar <- liftIO $ UnliftIO.newTVarIO 0
  let transactionContext =
        TransactionCtx
          { tags = Tags tags,
            numQueriesVar
          }
  let ioSession =
        s
          & Logging.runLoggerT minLogSeverity logger tags timeCache
          & flip runReaderT (env $> transactionContext)
          & runExceptT
  result <- liftIO (Pool.use pool ioSession)
  nq <- UnliftIO.readTVarIO $ numQueriesVar
  Trace.addAttribute span "numQueries" (Trace.toAttribute nq)
  case result of
    Left err -> throwIO . someServerError $ PostgresError err
    Right (Left (Unrecoverable e)) -> throwIO e
    Right (Left (Err e)) -> pure (Left e)
    Right (Right a) -> pure (Right a)
  where
    (entryPointName, spanAttrs) = spanInfo

-- | Run a session in the App monad, responding to the request with an error if it fails.
runSessionOrRespondError :: (HasCallStack, ToServerError e, Loggable e) => Session e a -> WebApp a
runSessionOrRespondError t = tryRunSession t >>= either respondError pure

-- | Represents anywhere we can run a statement
class (Applicative m) => QueryA m where
  type TError m :: Type
  statement :: (DebugCallStack) => q -> Hasql.Statement q r -> m r

  -- | Fail the transaction and whole request with an unrecoverable server error.
  unrecoverableError :: (HasCallStack, ToServerError e, Loggable e, Show e) => e -> m a

  -- | Map an either-returning function over the result of an action; if it returns Left, throw an unrecoverable error.
  -- This is a trivial combinator for any monad, hence the default signature, but it can be implemented by our
  -- Pipeline applicative, too.
  unrecoverableEitherMap :: (HasCallStack, Loggable e, Show e, ToServerError e) => (a -> Either e b) -> m a -> m b
  default unrecoverableEitherMap :: (HasCallStack, Loggable e, Show e, ToServerError e, Monad m) => (a -> Either e b) -> m a -> m b
  unrecoverableEitherMap f m = do
    x <- m
    case f x of
      Right y -> pure y
      Left e -> unrecoverableError e

class (Logging.MonadLogger m, QueryA m, MonadTracer m, MonadTags m) => QueryM m where
  -- | Allow running IO actions in a transaction. These actions may be run multiple times if
  -- the transaction is retried.
  transactionUnsafeIO :: IO a -> m a

  pipelined :: Pipeline (TError m) a -> m a

instance QueryA (Transaction e) where
  type TError (Transaction e) = e
  statement q s = do
    transactionStatement q s

  unrecoverableError e = do
    Transaction (pure (Left (Unrecoverable (someServerError e))))

instance QueryM (Transaction e) where
  -- Catch any IO exceptions that may occur in the transaction, and convert them to an
  -- unrecoverable error for better error handling.
  transactionUnsafeIO io = Transaction . liftIO $ do
    UnliftIO.tryAny io >>= \case
      Left someException -> pure (Left (Unrecoverable $ someServerError someException))
      Right a -> pure (Right a)
  pipelined p = Transaction (lift . lift $ Hasql.pipeline (unPipeline p))

-- | Run a pipeline in a transaction
-- pipelined :: Pipeline e a -> Transaction e a
-- pipelined
instance QueryA (Session e) where
  type TError (Session e) = e
  statement q s =
    Session . lift . lift . lift $ Session.statement q s

  unrecoverableError e = do
    throwError (Unrecoverable (someServerError e))

instance QueryM (Session e) where
  transactionUnsafeIO io = Session $ do
    liftIO (UnliftIO.tryAny io) >>= \case
      Left someException -> throwError (Unrecoverable (someServerError someException))
      Right a -> pure a
  pipelined p = Session $ do
    lift . lift . ExceptT $ Hasql.pipeline (unPipeline p)

instance MonadTracer (Session e) where
  getTracer = asks Env.tracer

instance MonadTags (Session e) where
  askTags = ask >>= transactionUnsafeIO . getTags
  withTags newTags (Session t) = Session $ do
    local (addTags newTags) t

instance QueryA (Pipeline e) where
  type TError (Pipeline e) = e
  statement q s = Pipeline (Right <$> Hasql.Pipeline.statement q s)

  unrecoverableError e = Pipeline $ pure (Left (Unrecoverable (someServerError e)))

  unrecoverableEitherMap f (Pipeline p) =
    Pipeline $
      p <&> \case
        Right x -> mapLeft (Unrecoverable . someServerError) (f x)
        Left e -> Left e

instance (QueryM m) => QueryA (ReaderT e m) where
  type TError (ReaderT e m) = TError m
  statement q s = lift $ statement q s

  unrecoverableError e = do
    lift $ unrecoverableError e

instance (QueryM m) => QueryM (ReaderT e m) where
  transactionUnsafeIO io = lift $ transactionUnsafeIO io
  pipelined p = lift $ pipelined p

instance (QueryM m) => QueryA (MaybeT m) where
  type TError (MaybeT m) = TError m
  statement q s = lift $ statement q s

  unrecoverableError e = lift $ unrecoverableError e

instance (QueryM m) => QueryM (MaybeT m) where
  transactionUnsafeIO io = lift $ transactionUnsafeIO io
  pipelined p = lift $ pipelined p

prepareStatements :: Bool
prepareStatements = True

queryListRows :: forall r m. (Interp.DecodeRow r, QueryA m, DebugCallStack) => Interp.Sql -> m [r]
queryListRows sql = statement () (Interp.interp prepareStatements sql)

queryVectorRows :: forall r m. (Interp.DecodeRow r, QueryA m, DebugCallStack) => Interp.Sql -> m (Vector r)
queryVectorRows sql = statement () (Interp.interp prepareStatements sql)

query1Row :: forall r m. (QueryA m) => (Interp.DecodeRow r, DebugCallStack) => Interp.Sql -> m (Maybe r)
query1Row sql = listToMaybe <$> queryListRows sql

query1Col :: forall a m. (QueryA m, Interp.DecodeField a, DebugCallStack) => Interp.Sql -> m (Maybe a)
query1Col sql = listToMaybe <$> queryListCol sql

queryListCol :: forall a m. (QueryA m) => (Interp.DecodeField a, DebugCallStack) => Interp.Sql -> m [a]
queryListCol sql = queryListRows @(Interp.OneColumn a) sql <&> coerce @[Interp.OneColumn a] @[a]

execute_ :: (QueryA m, DebugCallStack) => Interp.Sql -> m ()
execute_ sql = statement () (Interp.interp prepareStatements sql)

queryExpect1Row :: forall r m. (HasCallStack) => (Interp.DecodeRow r, QueryA m) => Interp.Sql -> m r
queryExpect1Row sql =
  query1Row sql <&> \case
    Nothing -> error "queryExpect1Row: expected 1 row, got 0"
    Just r -> r

queryExpect1Col :: forall a m. (HasCallStack) => (Interp.DecodeField a, QueryA m) => Interp.Sql -> m a
queryExpect1Col sql =
  query1Col sql <&> \case
    Nothing -> error "queryExpect1Col: expected 1 row, got 0"
    Just r -> r

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
whenNonEmpty :: forall m f a x. (Foldable f, Monoid a, Applicative m) => f x -> m a -> m a
whenNonEmpty f m = if null f then pure mempty else m

timeTransaction :: (QueryM m) => String -> m a -> m a
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

catchTransaction :: Transaction e a -> Transaction e' (Either e a)
catchTransaction (Transaction t) = Transaction do
  t >>= \case
    Left (Err e) -> pure (Right (Left e))
    Left (Unrecoverable err) -> pure (Left (Unrecoverable err))
    Right a -> pure (Right (Right a))

-- | Catch all errors which may occur in a transaction
catchAllTransaction :: Transaction e a -> Transaction e (Either (TransactionError e) a)
catchAllTransaction (Transaction t) = Transaction do
  t >>= \case
    Left (Err e) -> pure (Right $ Left (Err e))
    Left (Unrecoverable err) -> pure (Right $ Left (Unrecoverable err))
    Right a -> pure (Right $ Right a)

-- | Allows tracking a span in a transaction.
transactionSpan :: (HasCallStack, QueryM m) => Text -> HM.HashMap Text Trace.Attribute -> m a -> m a
transactionSpan name spanTags action = do
  tags <- askTags
  let (_mayFuncName, callSiteInfo) = spanInfo
  let spanAttributes = spanTags <> callSiteInfo <> HM.fromList (Map.toList (Trace.toAttribute <$> tags))

  let spanArguments =
        Trace.SpanArguments
          { kind = Trace.Server,
            attributes = spanAttributes,
            links = [],
            startTime = Nothing -- This will be set automatically
          }

  tracer <- getTracer
  (s, parent) <- transactionUnsafeIO do
    ctx <- Trace.getContext
    s <- Trace.createSpanWithoutCallStack tracer ctx name spanArguments
    Trace.adjustContext (Trace.insertSpan s)
    let parent = Trace.lookupSpan ctx
    pure (s, parent)
  r <- action
  transactionUnsafeIO $ do
    Trace.endSpan s Nothing
    Trace.adjustContext $ \ctx -> maybe (Trace.removeSpan ctx) (`Trace.insertSpan` ctx) parent
  pure r

-- | Helper to treat composite types as Values, useful when decoding arrays of rows.
--
-- This is a bit annoying, can probably add a better version to hasql-interpolate somehow.
newtype TupleVal a b = TupleVal (a, b)
  deriving (Show, Eq, Ord)

instance (Interp.DecodeField a, Interp.DecodeField b) => Hasql.DecodeValue (TupleVal a b) where
  decodeValue = TupleVal <$> (Decoders.composite decoder)
    where
      decoder :: Decoders.Composite (a, b)
      decoder = do
        a <- Decoders.field $ Interp.decodeField
        b <- Decoders.field $ Interp.decodeField
        pure (a, b)
