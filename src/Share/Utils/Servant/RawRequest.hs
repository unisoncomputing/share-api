{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides the raw request to a handler. Generally this is a bad idea in Servant, but it
-- can be very useful or even necessary for implementing middleware.
module Share.Utils.Servant.RawRequest (RawRequest) where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Share.Prelude
import Network.Wai qualified as Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

-- | Type-token for injecting the Wai Request into a handler
--
-- Note: Don't consume or interact with the 'body' of a raw request, leave that to Servant.
data RawRequest

instance HasServer api ctx => HasServer (RawRequest :> api) ctx where
  type ServerT (RawRequest :> api) m = Wai.Request -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ ctx d =
    route (Proxy :: Proxy api) ctx $
      passToServer d setPathInfo
    where
      -- For whatever reason servant doesn't seem to set 'pathInfo' so we have to set it
      -- ourselves.
      setPathInfo req =
        req
          { Wai.pathInfo =
              Wai.rawPathInfo req
                & Text.decodeUtf8
                & Text.dropWhile (== '/')
                & Text.dropWhileEnd (== '/')
                & Text.splitOn "/"
          }
