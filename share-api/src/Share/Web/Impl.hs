{-# LANGUAGE RecordWildCards #-}

module Share.Web.Impl (server) where

import Crypto.JOSE.JWK qualified as JWK
import Data.Set qualified as Set
import Servant
import Share.App
import Share.Env.Types qualified as Env
import Share.JWT qualified as JWT
import Share.JWT.Types (Issuer (..))
import Share.OAuth.Session
import Share.OAuth.Types (ResponseType (ResponseTypeCode))
import Share.Postgres.Ops qualified as PGO
import Share.Prelude
import Share.User
import Share.Utils.URI
import Share.Web.API qualified as Web
import Share.Web.Admin.Impl qualified as Admin
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Local.Impl qualified as Local
import Share.Web.OAuth.Impl qualified as OAuth
import Share.Web.Share.Impl qualified as Share
import Share.Web.Share.Orgs.Impl qualified as Orgs
import Share.Web.Share.Projects.Impl qualified as Projects
import Share.Web.Share.Webhooks.Impl qualified as Webhooks
import Share.Web.Support.Impl qualified as Support
import Share.Web.Types
import Share.Web.UCM.Projects.Impl qualified as UCMProjects
import Share.Web.UCM.Sync.Impl qualified as Sync
import Share.Web.UCM.SyncV2.Impl qualified as SyncV2
import Share.Web.UCM.SyncV3.Impl qualified as SyncV3
import Share.Web.UI.Links qualified as Links

discoveryEndpoint :: WebApp DiscoveryDocument
discoveryEndpoint = do
  issuer <- coerce @Issuer @URIParam <$> shareIssuer
  authorizationE <- URIParam <$> Links.oauthAuthorize
  tokenE <- URIParam <$> Links.oauthToken
  userInfoE <- URIParam <$> Links.openIDUserInfo
  jwksURI <- URIParam <$> Links.jwksURI
  let responseTypesSupported = Set.singleton ResponseTypeCode
  pure $ DiscoveryDocument {..}

-- | JWK RFC: https://tools.ietf.org/html/rfc7517
jwksEndpoint :: WebApp JWK.JWKSet
jwksEndpoint = do
  jwtSettings <- asks (Env.jwtSettings)
  shareIssuer <- shareIssuer
  JWT.publicJWKSet jwtSettings shareIssuer

-- | https://openid.net/specs/openid-connect-core-1_0.html#UserInfo
userInfoEndpoint :: Maybe Session -> WebApp UserInfo
userInfoEndpoint sess = do
  userId <- AuthN.requireAuthenticatedUser sess
  User {user_name, avatar_url, user_email, handle, user_id} <- PGO.expectUserById userId
  profileUrl <- Links.userProfilePage handle
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
    :<|> Orgs.server
    :<|> Share.searchEndpoint
    :<|> Share.searchDefinitionNamesEndpoint
    :<|> Share.searchDefinitionsEndpoint
    :<|> Share.accountServer
    :<|> Projects.catalogServer
    :<|> ( discoveryEndpoint
             :<|> jwksEndpoint
         )
    :<|> userInfoEndpoint
    :<|> Support.server
    :<|> Local.server
    :<|> healthEndpoint
    :<|> Sync.server -- Deprecated path
    :<|> Sync.server
    :<|> UCMProjects.server
    :<|> SyncV2.server
    :<|> SyncV3.server
    :<|> Admin.server
    :<|> Webhooks.server
