{-# LANGUAGE RecordWildCards #-}

module Enlil.Web.Impl (server) where

import Data.Set qualified as Set
import Enlil.App
import Enlil.IDs qualified as IDs
import Enlil.OAuth.Session
import Enlil.OAuth.Types (ResponseType (ResponseTypeCode))
import Enlil.Postgres.Ops qualified as PGO
import Enlil.Prelude
import Enlil.User
import Enlil.Utils.URI
import Enlil.Web.API qualified as Web
import Enlil.Web.Admin.Impl qualified as Admin
import Enlil.Web.App
import Enlil.Web.Authentication qualified as AuthN
import Enlil.Web.Local.Impl qualified as Local
import Enlil.Web.OAuth.Impl qualified as OAuth
import Enlil.Web.Share.Impl qualified as Share
import Enlil.Web.Share.Projects.Impl qualified as Projects
import Enlil.Web.Support.Impl qualified as Support
import Enlil.Web.Types
import Enlil.Web.UCM.Projects.Impl qualified as UCMProjects
import Enlil.Web.UCM.Sync.Impl qualified as Sync
import Servant

discoveryEndpoint :: WebApp DiscoveryDocument
discoveryEndpoint = do
  issuer <- URIParam <$> enlilIssuer
  authorizationE <- URIParam <$> enlilPath ["oauth", "authorize"]
  tokenE <- URIParam <$> enlilPath ["oauth", "token"]
  userInfoE <- URIParam <$> enlilPath ["user-info"]
  let responseTypesSupported = Set.singleton ResponseTypeCode
  pure $ DiscoveryDocument {..}

-- | https://openid.net/specs/openid-connect-core-1_0.html#UserInfo
userInfoEndpoint :: Maybe Session -> WebApp UserInfo
userInfoEndpoint sess = do
  userId <- AuthN.requireAuthenticatedUser sess
  User {user_name, avatar_url, user_email, handle, user_id} <- PGO.expectUserById userId
  profileUrl <- shareUIPath ["@" <> IDs.toText handle]
  pure $
    UserInfo
      { handle = handle,
        name = user_name,
        picture = avatar_url,
        profile = URIParam profileUrl,
        sub = user_id,
        email = user_email
      }

healthEndpoint :: WebApp Text
healthEndpoint = pure "Healthy"

server :: ServerT Web.API WebApp
server =
  OAuth.serviceProviderServer
    :<|> OAuth.identityProviderServer
    :<|> Share.userCodebaseServer
    :<|> Share.userServer
    :<|> Share.searchEndpoint
    :<|> Share.accountServer
    :<|> Projects.catalogServer
    :<|> discoveryEndpoint
    :<|> userInfoEndpoint
    :<|> Support.server
    :<|> Local.server
    :<|> healthEndpoint
    :<|> Sync.server -- Deprecated path
    :<|> Sync.server
    :<|> UCMProjects.server
    :<|> Admin.server
