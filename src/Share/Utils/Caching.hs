{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Utils.Caching
  ( cachedResponse,
    conditionallyCachedResponse,
    causalIdCacheKey,
    branchIdCacheKey,
    Cached,
  )
where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.Binary.Builder qualified as Builder
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as Text
import Database.Redis qualified as R
import Network.HTTP.Media
import Network.HTTP.Types qualified as HTTP
import Servant
import Share.Postgres.IDs (BranchHashId (..), CausalId (..))
import Share.Prelude
import Share.Web.App
import Share.Web.Authorization.Types qualified as AuthZ

data Cached ct a
  = Cached BS.ByteString

instance MimeRender JSON (Cached JSON a) where
  mimeRender _proxy = \case
    Cached bs -> BL.fromStrict bs

instance (FromJSON a, ToJSON a) => ToJSON (Cached JSON a) where
  toJSON (Cached bs) = toJSON $ Aeson.decode @a $ BL.fromStrict bs

  toEncoding (Cached bs) = Aeson.unsafeToEncoding $ Builder.fromLazyByteString $ BL.fromStrict bs

-- | Wrap a response in caching.
-- This combinator knows whether a given access is privileged or not and will _not_ cache
-- private content.
cachedResponse ::
  forall ct a.
  (Servant.MimeRender ct a) =>
  AuthZ.AuthZReceipt ->
  -- | The name of the endpoint we're caching. Must be unique.
  Text ->
  -- | Cache Keys: All parameters which affect the response
  [Text] ->
  -- | How to generate the response if it's not in the cache.
  WebApp a ->
  WebApp (Cached ct a)
cachedResponse authzReceipt endpointName cacheParams action =
  conditionallyCachedResponse authzReceipt endpointName cacheParams ((,True) <$> action)

-- | Like 'cachedResponse', but only cache (True, x) values.
conditionallyCachedResponse ::
  forall ct a.
  (Servant.MimeRender ct a) =>
  AuthZ.AuthZReceipt ->
  -- | The name of the endpoint we're caching. Must be unique.
  Text ->
  -- | Cache Keys: All parameters which affect the response
  [Text] ->
  -- | How to generate the response if it's not in the cache. True means cache, false means don't cache.
  WebApp (a, Bool) ->
  WebApp (Cached ct a)
conditionallyCachedResponse authzReceipt endpointName cacheParams action = do
  requestIsCacheable <- shouldUseCaching
  let mayCachingToken = AuthZ.getCacheability authzReceipt
  let shouldUseCaching = requestIsCacheable && isJust mayCachingToken
  mayCachedResponse <-
    if shouldUseCaching
      then getCachedResponse endpointName cacheParams
      else pure Nothing
  case mayCachedResponse of
    Just cachedResponse -> pure cachedResponse
    Nothing -> do
      (a, cache) <- action
      let cachedResponse :: Cached ct a
          cachedResponse = Cached . BL.toStrict $ Servant.mimeRender (Proxy @ct) a
      when (shouldUseCaching && cache) do
        -- Only actually cache the response if it's valid to do so.
        whenJust mayCachingToken \ct ->
          cacheResponse ct endpointName cacheParams cachedResponse
      pure cachedResponse

-- | Cached responses expire if not accessed in 7 days.
-- Or, it could be evicted sooner if we run out of space.
responseCacheTTL :: Integer
responseCacheTTL =
  (60 * 24 * 30) -- 30 days.

-- | Construct a cache key for a response.
cachedResponseKey ::
  -- | The name of the current endpoint
  Text ->
  -- | All the parameters which might affect the response
  [Text] ->
  MediaType ->
  BS.ByteString
cachedResponseKey endpointName cacheParams mediaType =
  BS.intercalate ":" . fmap encode $ ("cached-response" : endpointName : "media-type" : tShow mediaType : cacheParams)
  where
    -- Escape all the ":" in the keys so malicious users can't manually create an invalid key.
    encode :: Text -> ByteString
    encode = HTTP.urlEncode False . Text.encodeUtf8

-- | Cache a response at the given key.
cacheResponse ::
  forall ct a.
  (Servant.Accept ct) =>
  -- A proof that authorization was checked on this request and it was determined to be public and cacheable.
  AuthZ.CachingToken ->
  -- | The name of the current endpoint
  Text ->
  -- | All the parameters which might affect the response
  [Text] ->
  -- | The response to cache
  Cached ct a ->
  WebApp ()
cacheResponse _cachingToken endpointName params (Cached bs) = do
  -- We ignore cache save errors, better to not hold up the response.
  void $ R.setex key responseCacheTTL bs
  where
    key = cachedResponseKey endpointName params (Servant.contentType $ Proxy @ct)

-- | Get a cached response
getCachedResponse ::
  forall ct a.
  (Servant.Accept ct) =>
  -- | The name of the current endpoint
  Text ->
  -- | All the parameters which might affect the response
  [Text] ->
  WebApp (Maybe (Cached ct a))
getCachedResponse endpointName params =
  R.get key >>= \case
    Right (Just bs) -> do
      -- Refresh expiry
      void $ R.expire key responseCacheTTL
      pure . Just $ Cached bs
    _ -> pure Nothing
  where
    key = cachedResponseKey endpointName params (Servant.contentType $ Proxy @ct)

causalIdCacheKey :: CausalId -> Text
causalIdCacheKey (CausalId causalIdInt) =
  -- Causal Ids are globally unique and never re-used.
  "causal-id:" <> tShow @Int32 causalIdInt

branchIdCacheKey :: BranchHashId -> Text
branchIdCacheKey (BranchHashId branchIdInt) =
  -- Branch Ids are globally unique and never re-used.
  "branch-id:" <> tShow @Int32 branchIdInt
