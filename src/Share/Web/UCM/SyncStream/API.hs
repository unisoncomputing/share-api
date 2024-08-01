{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.UCM.SyncStream.API (API, server) where

import Conduit
import Servant
import Servant.Conduit (ConduitToSourceIO (..))
import Share.Prelude
import Share.Utils.Servant.CBOR (CBOR)
import Share.Web.App

type API = "download-causal" :> DownloadCausalStreamEndpoint

type DownloadCausalStreamEndpoint =
  QueryParam "name" Text
    :> StreamGet NewlineFraming CBOR (SourceIO Text)

server :: ServerT API WebApp
server =
  downloadCausalStreamEndpointConduit

downloadCausalStreamEndpointConduit :: Maybe Text -> WebApp (SourceIO Text)
downloadCausalStreamEndpointConduit name = pure . conduitToSourceIO @IO $ do
  yield "hello"
  yield (fromMaybe "mystery" name)
  yield "world"
