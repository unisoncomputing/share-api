{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.API where

import Crypto.JOSE.JWK qualified as JWK
import Servant
import Share.OAuth.API qualified as OAuth
import Share.OAuth.Session (MaybeAuthenticatedSession, MaybeAuthenticatedUserId)
import Share.Prelude
import Share.Web.Admin.API qualified as Admin
import Share.Web.Local.API qualified as Local
import Share.Web.Share.API qualified as Share
import Share.Web.Share.Orgs.API qualified as Orgs
import Share.Web.Share.Projects.API qualified as Projects
import Share.Web.Share.Users.API qualified as Users
import Share.Web.Share.Webhooks.API qualified as Webhooks
import Share.Web.Support.API qualified as Support
import Share.Web.Types
import Share.Web.UCM.SyncV2.API qualified as SyncV2
import Unison.Share.API.Projects qualified as UCMProjects
import Unison.Sync.API qualified as Unison.Sync

-- Some APIs are pulled out separately to make building clients easier.
type OrgsAPI = ("orgs" :> Orgs.API)

type UsersAPI = ("users" :> Users.API)

type API =
  OAuth.ServiceProviderAPI
    :<|> OAuth.IdentityProviderAPI
    :<|> ("codebases" :> Share.UserPublicCodebaseAPI)
    :<|> UsersAPI
    :<|> OrgsAPI
    :<|> ("search" :> Share.OmniSearchEndpoint)
    :<|> ("search-names" :> Share.SearchDefinitionNamesEndpoint)
    :<|> ("search-definitions" :> Share.SearchDefinitionsEndpoint)
    :<|> ("account" :> Share.AccountAPI)
    :<|> ("catalog" :> Projects.CatalogAPI)
    :<|> ( ".well-known"
             :> (
                  -- This path is part of the standard: https://datatracker.ietf.org/doc/html/rfc5785
                  ("openid-configuration" :> DiscoveryEndpoint)
                    -- This path is convention, the location is provided explicitly as part of
                    -- the discovery document's jwks_uri field.
                    :<|> ("jwks.json" :> JWKSEndpoint)
                )
         )
    :<|> ("user-info" :> UserInfoEndpoint)
    :<|> ("support" :> Support.API)
    :<|> ("local" :> Local.API)
    :<|> ("health" :> HealthEndpoint)
    -- This path is deprecated, but is still in use by existing clients.
    :<|> ("sync" :> MaybeAuthenticatedSession :> Unison.Sync.API)
    :<|> ("ucm" :> "v1" :> "sync" :> MaybeAuthenticatedSession :> Unison.Sync.API)
    :<|> ("ucm" :> "v1" :> "projects" :> MaybeAuthenticatedSession :> UCMProjects.ProjectsAPI)
    :<|> ("ucm" :> "v2" :> "sync" :> MaybeAuthenticatedUserId :> SyncV2.API)
    :<|> ("admin" :> Admin.API)
    :<|> ("webhooks" :> Webhooks.API)

api :: Proxy API
api = Proxy @API

-- | https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfigurationRequest
type DiscoveryEndpoint =
  Get '[JSON] DiscoveryDocument

type JWKSEndpoint =
  Get '[JSON] JWK.JWKSet

type UserInfoEndpoint =
  MaybeAuthenticatedSession :> Get '[JSON] UserInfo

type HealthEndpoint =
  Get '[PlainText] Text
