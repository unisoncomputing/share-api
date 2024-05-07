{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides the endpoints for performing an OAuth2 flow using the PKCE extension.
-- Here's a high-level overview of OAuth2: https://developer.okta.com/blog/2019/10/21/illustrated-guide-to-oauth-and-oidc
-- Here's a good reference guide to various steps: https://www.oauth.com/
-- Here's a good reference for PKCE specifically: https://www.oauth.com/oauth2-servers/pkce/
module Share.OAuth.API
  ( ServiceProviderAPI,
    IdentityProviderAPI,
    LoginEndpoint,
    LogoutEndpoint,
    AuthorizationEndpoint,
    TokenEndpoint,
    RedirectReceiverEndpoint,
  )
where

import Data.Text (Text)
import Share.OAuth.Scopes
import Share.OAuth.Session
import Share.OAuth.Types
import Share.Utils.Servant.Cookies (Cookie)
import Share.Utils.URI (URIParam)
import Servant
import Web.Cookie (SetCookie)

type RequiredQueryParam = QueryParam' '[Required, Strict]

-- | Endpoints which will be hosted within the Service Provider.
type ServiceProviderAPI =
  ("login" :> LoginEndpoint)
    :<|> ("logout" :> LogoutEndpoint)
    :<|> ("oauth" :> "redirect" :> RedirectReceiverEndpoint)

-- | API of the Identity Provider servers we will auth with.
--
-- Implements parts of the OpenID connect standard
-- https://openid.net/specs/openid-connect-core-1_0.html
type IdentityProviderAPI =
  "oauth"
    :> ( ("authorize" :> AuthorizationEndpoint)
           :<|> ("token" :> TokenEndpoint)
       )

-- | GET /login
type LoginEndpoint =
  -- Where to redirect to after login
  QueryParam "return_to" URIParam
    :> Verb 'GET 302 '[PlainText] (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

-- | GET /logout
type LogoutEndpoint =
  Verb 'GET 302 '[PlainText] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.1
type AuthorizationEndpoint =
  RequiredQueryParam "response_type" ResponseType
    :> RequiredQueryParam "client_id" OAuthClientId
    :> RequiredQueryParam "redirect_uri" URIParam
    :> RequiredQueryParam "scope" Scopes
    :> RequiredQueryParam "state" Text
    :> RequiredQueryParam "code_challenge" PKCEChallenge
    :> RequiredQueryParam "code_challenge_method" PKCEChallengeMethod
    :> MaybeAuthenticatedSession
    :> Verb 'GET 302 '[PlainText] (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3
type TokenEndpoint =
  ReqBody '[FormUrlEncoded] TokenRequest
    :> Post '[JSON] (Headers '[Header "Cache-Control" String] TokenResponse)

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.2
type RedirectReceiverEndpoint =
  QueryParam "code" Code
    :> QueryParam "state" OAuth2State
    :> QueryParam "error" Text
    :> QueryParam "error_description" Text
    :> Cookie PendingSessionCookieKey PendingSessionId
    :> MaybeAuthenticatedSession
    :> Verb 'GET 302 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
