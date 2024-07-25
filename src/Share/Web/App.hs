{-# LANGUAGE DataKinds #-}

module Share.Web.App
  ( RequestCtx (..),
    WebApp,
    ReqTagsVar,
    localRequestCtx,
    withLocalTag,
    freshRequestCtx,
    addRequestTag,
    addServerTag,
    getTags,
    shouldUseCaching,
    sharePath,
    sharePathQ,
    shareUIPath,
    shareUIPathQ,
    isTrustedURI,
  )
where

import Control.Monad.Reader
import Data.Map qualified as Map
import Network.URI
import Servant
import Share.App
import Share.Env
import Share.Env qualified as Env
import Share.IDs (RequestId, UserId)
import Share.Prelude
import Share.Utils.Logging
import Share.Utils.URI (setPathAndQueryParams)
import UnliftIO.STM

type WebApp = AppM RequestCtx

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

instance MonadLogger WebApp where
  logMsg msg = do
    log <- asks Env.logger
    currentTags <- getTags
    msg <- pure $ msg {tags = tags msg `Map.union` currentTags}
    minSeverity <- asks Env.minLogSeverity
    when (severity msg >= minSeverity) $ do
      timestamp <- asks timeCache >>= liftIO
      liftIO . log . logFmtFormatter timestamp $ msg

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

-- | Get the tags associated with the current request.
getTags :: (MonadReader (Env RequestCtx) m, MonadIO m) => m (Map Text Text)
getTags = do
  RequestCtx {reqTagsVar, localTags} <- asks requestCtx
  reqTags <- liftIO $ readTVarIO reqTagsVar
  -- local tags take precedence over request tags
  pure $ localTags <> reqTags

-- | Add a tag to the current request. This tag will be used in logging and error reports
addRequestTag :: (MonadReader (Env RequestCtx) m, MonadIO m) => Text -> Text -> m ()
addRequestTag k v = do
  RequestCtx {reqTagsVar} <- asks requestCtx
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
localRequestCtx f = local \env -> env {requestCtx = f (requestCtx env)}

shouldUseCaching :: (MonadReader (Env RequestCtx) m) => m Bool
shouldUseCaching =
  asks (useCaching . requestCtx)

-- | Construct a full URI to a path within share, with provided query params.
sharePathQ :: [Text] -> Map Text Text -> AppM reqCtx URI
sharePathQ pathSegments queryParams = do
  uri <- asks Env.apiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ uri

-- | Construct a full URI to a path within share.
sharePath :: [Text] -> AppM reqCtx URI
sharePath path = sharePathQ path mempty

-- | Check if a URI is a the Share UI, the Cloud UI, the main website, or the
-- Cloud website. This is useful for preventing attackers from generating
-- arbitrary redirections in things like login redirects.
isTrustedURI :: URI -> AppM reqCtx Bool
isTrustedURI uri = do
  shareUiURI <- asks Env.shareUiOrigin
  websiteURI <- asks Env.websiteOrigin
  cloudUiURI <- asks Env.cloudUiOrigin
  cloudWebsiteURI <- asks Env.cloudWebsiteOrigin
  let requestedAuthority = uriAuthority uri
  let trustedURIs = [shareUiURI, websiteURI, cloudUiURI, cloudWebsiteURI]
  pure $ any (\uri -> uriAuthority uri == requestedAuthority) trustedURIs

-- | Construct a full URI to a path within the share UI, with the provided query params.
shareUIPathQ :: [Text] -> Map Text Text -> AppM reqCtx URI
shareUIPathQ pathSegments queryParams = do
  shareUiURI <- asks Env.shareUiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ shareUiURI

shareUIPath :: [Text] -> AppM reqCtx URI
shareUIPath pathSegments = shareUIPathQ pathSegments mempty
