{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.API where

import Share.OAuth.API qualified as OAuth
import Share.OAuth.Session (MaybeAuthenticatedSession)
import Share.Prelude
import Share.Web.Admin.API qualified as Admin
import Share.Web.Local.API qualified as Local
import Share.Web.Share.API qualified as Share
import Share.Web.Share.Projects.API qualified as Projects
import Share.Web.Support.API qualified as Support
import Share.Web.Types
import Servant
import Unison.Share.API.Projects qualified as UCMProjects
import Unison.Sync.API qualified as Unison.Sync

type API =
  OAuth.ServiceProviderAPI
    :<|> OAuth.IdentityProviderAPI
    :<|> ("codebases" :> Share.UserPublicCodebaseAPI)
    :<|> ("users" :> Share.UserAPI)
    :<|> ("search" :> Share.SearchEndpoint)
    :<|> ("account" :> Share.AccountAPI)
    :<|> ("catalog" :> Projects.CatalogAPI)
    -- This path is part of the standard: https://datatracker.ietf.org/doc/html/rfc5785
    :<|> (".well-known" :> "openid-configuration" :> DiscoveryEndpoint)
    :<|> ("user-info" :> UserInfoEndpoint)
    :<|> ("support" :> Support.API)
    :<|> ("local" :> Local.API)
    :<|> ("health" :> HealthEndpoint)
    -- This path is deprecated, but is still in use by existing clients.
    :<|> ("sync" :> MaybeAuthenticatedSession :> Unison.Sync.API)
    :<|> ("ucm" :> "v1" :> "sync" :> MaybeAuthenticatedSession :> Unison.Sync.API)
    :<|> ("ucm" :> "v1" :> "projects" :> MaybeAuthenticatedSession :> UCMProjects.ProjectsAPI)
    :<|> ("admin" :> Admin.API)

api :: Proxy API
api = Proxy @API

-- | https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfigurationRequest
type DiscoveryEndpoint =
  Get '[JSON] DiscoveryDocument

type UserInfoEndpoint =
  MaybeAuthenticatedSession :> Get '[JSON] UserInfo

type HealthEndpoint =
  Get '[PlainText] Text
