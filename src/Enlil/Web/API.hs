{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.API where

import Enlil.OAuth.API qualified as OAuth
import Enlil.OAuth.Session (MaybeAuthenticatedSession)
import Enlil.Prelude
import Enlil.Web.Admin.API qualified as Admin
import Enlil.Web.Local.API qualified as Local
import Enlil.Web.Share.API qualified as Share
import Enlil.Web.Share.Projects.API qualified as Projects
import Enlil.Web.Support.API qualified as Support
import Enlil.Web.Types
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
