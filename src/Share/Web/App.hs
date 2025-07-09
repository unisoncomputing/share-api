{-# LANGUAGE DataKinds #-}

module Share.Web.App
  ( RequestCtx (..),
    WebApp,
    WebAppServer,
    ReqTagsVar,
    localRequestCtx,
    withLocalTag,
    freshRequestCtx,
    addRequestTag,
    addServerTag,
    getTags,
    shouldUseCaching,
  )
where

import Control.Monad.Reader
import Data.Map qualified as Map
import Network.URI
import Servant
import Servant.Server.Generic (AsServerT)
import Share.App
import Share.Env
import Share.IDs (RequestId, UserId)
import Share.Prelude
import UnliftIO.STM

type WebApp = AppM RequestCtx

type WebAppServer = AsServerT WebApp

type ReqTagsVar = TVar (Map Text Text)

-- | Context which is local to a single request.
data RequestCtx = RequestCtx
  { useCaching :: Bool,
    requestId :: Maybe RequestId,
    authenticatedUser :: Maybe UserId,
    -- Tags which apply to the entire request
    reqTagsVar :: ReqTagsVar,
    -- Context specific tags which will be appended to log messages.
    -- Often useful when performing an operation in a loop over a set of things and you want
    -- to know which thing you were working with when a given log or error occurred.
    localTags :: Map Text Text,
    pathInfo :: [Text],
    -- | The full URI of the request, useful for debugging, don't use this for logic.
    rawURI :: Maybe URI
  }

-- | Generate an empty request context
freshRequestCtx :: (MonadIO m) => m RequestCtx
freshRequestCtx = do
  reqTagsVar <- liftIO $ newTVarIO mempty
  pure
    ( RequestCtx
        { useCaching = True,
          requestId = Nothing,
          authenticatedUser = Nothing,
          reqTagsVar,
          localTags = mempty,
          pathInfo = [],
          rawURI = Nothing
        }
    )

instance HasTags RequestCtx where
  -- Get the tags associated with the current request.
  getTags RequestCtx {reqTagsVar, localTags} = do
    reqTags <- liftIO $ readTVarIO reqTagsVar
    -- local tags take precedence over request tags
    pure $ localTags <> reqTags
  updateTags f reqCtx = do
    tags <- getTags reqCtx
    let newTags = f tags
    pure $ reqCtx {localTags = newTags <> localTags reqCtx}

-- | Add a tag to the current request. This tag will be used in logging and error reports
addRequestTag :: (MonadReader (Env RequestCtx) m, MonadIO m) => Text -> Text -> m ()
addRequestTag k v = do
  RequestCtx {reqTagsVar} <- asks ctx
  atomically $ modifyTVar' reqTagsVar (Map.insert k v)

addServerTag :: (HasServer api '[]) => Proxy api -> Text -> Text -> ServerT api WebApp -> ServerT api WebApp
addServerTag proxy label value api = hoistServer proxy go api
  where
    go :: forall x. WebApp x -> WebApp x
    go m = do
      addRequestTag label value
      m

-- | Set a tag within given block. This tag will be used in logging and error reports within
-- that block.
--
-- E.g. If performing an action over many users, we'll want to know which user we were working
-- on when logs or errors occur:
--
-- for users \user -> withLocalTag "user" (userId user) do
--   ...
withLocalTag :: (MonadReader (Env RequestCtx) m) => Text -> Text -> m a -> m a
withLocalTag k v action = do
  localRequestCtx (\ctx -> ctx {localTags = Map.insert k v $ localTags ctx}) action

localRequestCtx :: (MonadReader (Env RequestCtx) m) => (RequestCtx -> RequestCtx) -> m a -> m a
localRequestCtx f = local \env -> env {ctx = f (ctx env)}

shouldUseCaching :: (MonadReader (Env RequestCtx) m) => m Bool
shouldUseCaching =
  asks (useCaching . ctx)
