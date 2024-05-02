module Enlil.Utils.Servant.Client where

import Enlil.Prelude
import Enlil.Web.App
import Enlil.Web.Errors (respondError)
import Network.HTTP.Client.TLS qualified as HTTP
import Servant.Client

runClient :: BaseUrl -> ClientM a -> WebApp a
runClient baseURL m = do
  resp <- runClientEither baseURL m
  either respondError pure resp

runClientEither :: BaseUrl -> ClientM a -> WebApp (Either ClientError a)
runClientEither baseURL m = do
  httpClient <- liftIO $ HTTP.getGlobalManager
  let env = mkClientEnv httpClient baseURL
  resp <- liftIO $ runClientM m env
  pure resp
