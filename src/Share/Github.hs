{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Share.Github where

import Control.Monad.Reader
import Data.Aeson
import Share.Env qualified as Env
import Share.OAuth.Types (OAuth2State)
import Share.OAuth.Types qualified as OAuth2
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant.Client (runClientEither)
import Share.Utils.Show
import Share.Utils.URI (URIParam (URIParam))
import Share.Web.App
import Share.Web.Errors
import Network.HTTP.Types (Status (..))
import Network.URI (URIAuth (URIAuth))
import Servant
import Servant.Client

data GithubError
  = GithubClientError ClientError
  | GithubUserWithoutPrimaryEmail
  | UnauthenticatedInGithubCodeExchange ClientError
  | UnauthorizedInGithubCodeExchange ClientError
  | GithubUnavailable ClientError
  | GithubDecodeError ClientError
  deriving stock (Show)

instance Logging.Loggable GithubError where
  toLog = Logging.withSeverity Logging.Error . Logging.textLog . tShow

instance ToServerError GithubError where
  toServerError = \case
    GithubClientError _ce -> (ErrorID "github:client-error", internalServerError)
    GithubUserWithoutPrimaryEmail -> (ErrorID "github:user-without-primary-email", err400 {errBody = "Github user must have a registered primary email"})
    UnauthenticatedInGithubCodeExchange _ce -> (ErrorID "github:unauthenticated-in-code-exchange", err401 {errBody = "Github code exchange failed. Please try again."})
    UnauthorizedInGithubCodeExchange _ce -> (ErrorID "github:unauthorized-in-code-exchange", err403 {errBody = "Github code exchange failed. Please try again."})
    GithubUnavailable _ce -> (ErrorID "github:unavailable", err503 {errBody = "Github is currently unavailable. Please try again later."})
    GithubDecodeError _ce -> (ErrorID "github:decode-error", err500 {errBody = "Github returned an unexpected response. Please try again."})

data GithubUser = GithubUser
  { github_user_login :: Text,
    github_user_id :: Int64,
    github_user_avatar_url :: URIParam,
    github_user_name :: Maybe Text
  }
  deriving (Show)

instance FromJSON GithubUser where
  parseJSON = withObject "GithubUser" $ \u ->
    GithubUser
      <$> u
        .: "login"
      <*> u
        .: "id"
      <*> u
        .: "avatar_url"
      -- We don't use this email because it's the "publicly visible" email, instead we fetch
      -- the primary email using the emails API.
      -- <*> u .: "email"
      <*> u
        .:? "name"

data GithubEmail = GithubEmail
  { github_email_email :: Text,
    github_email_primary :: Bool,
    github_email_verified :: Bool
  }
  deriving (Show)

instance FromJSON GithubEmail where
  parseJSON = withObject "GithubEmail" $ \u ->
    GithubEmail
      <$> u
        .: "email"
      <*> u
        .: "primary"
      <*> u
        .: "verified"

type GithubTokenApi =
  "login"
    :> "oauth"
    :> "access_token"
    :> Header "Accept" Text
    :> QueryParam "client_id" Text
    :> QueryParam "client_secret" Text
    :> QueryParam "code" OAuth2.Code
    :> Post '[JSON] GithubToken

githubTokenApi :: Proxy GithubTokenApi
githubTokenApi = Proxy

data GithubToken = GithubToken
  { access_token :: Text,
    scope :: Text,
    token_type :: Text
  }
  deriving (Show, Generic, FromJSON)

_githubTokenClient :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe OAuth2.Code -> ClientM GithubToken
_githubTokenClient = client githubTokenApi

githubTokenClient :: Text -> Text -> OAuth2.Code -> ClientM GithubToken
githubTokenClient clientId clientSecret code = _githubTokenClient (Just "application/json") (Just clientId) (Just clientSecret) (Just code)

type GithubUserApi =
  "user"
    :> Header "User-Agent" Text
    :> Header "Authorization" Text
    :> Get '[JSON] GithubUser

githubUserApi :: Proxy GithubUserApi
githubUserApi = Proxy

_githubUserClient :: Maybe Text -> Maybe Text -> ClientM GithubUser
_githubUserClient = client githubUserApi

githubUserClient :: Text -> ClientM GithubUser
githubUserClient auth = _githubUserClient (Just "UnisonWeb-Nimbus") (Just auth)

type GithubEmailApi =
  "user"
    :> "emails"
    :> Header "User-Agent" Text
    :> Header "Authorization" Text
    :> QueryParam "accept" Text
    :> QueryParam "per_page" Int
    :> QueryParam "page" Int
    :> Get '[JSON] [GithubEmail]

githubEmailApi :: Proxy GithubEmailApi
githubEmailApi = Proxy

_githubEmailsClient :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> ClientM [GithubEmail]
_githubEmailsClient = client githubEmailApi

githubEmailsClient :: GithubAccessToken -> ClientM [GithubEmail]
githubEmailsClient githubToken = _githubEmailsClient (Just "UnisonWeb-Nimbus") (Just (tokenToAuthHeaderString githubToken)) (Just "application/vnd.github.v3+json") Nothing Nothing

githubBaseURL :: BaseUrl
githubBaseURL = (BaseUrl Https "github.com" 443 "")

githubAPIBaseURL :: BaseUrl
githubAPIBaseURL = (BaseUrl Https "api.github.com" 443 "")

handleGithubError :: Either ClientError a -> WebApp a
handleGithubError = \case
  Left err -> do
    Logging.logMsg $ Logging.toLog err
    case err of
      (FailureResponse _ resp)
        | (statusCode . responseStatusCode $ resp) == 401 -> respondError $ UnauthenticatedInGithubCodeExchange err
        | (statusCode . responseStatusCode $ resp) == 403 -> respondError $ UnauthorizedInGithubCodeExchange err
        | (statusCode . responseStatusCode $ resp) == 500 -> respondError $ GithubUnavailable err
        | (statusCode . responseStatusCode $ resp) == 503 -> respondError $ GithubUnavailable err
      (DecodeFailure {}) -> respondError $ GithubDecodeError err
      _ -> respondError err
  Right a -> pure a

runGithubClient :: BaseUrl -> ClientM b -> WebApp b
runGithubClient baseUrl m = runClientEither baseUrl m >>= handleGithubError

githubTokenForCode :: OAuth2.Code -> WebApp GithubAccessToken
githubTokenForCode code = do
  env <- ask
  let clientId = Env.githubClientID env
      clientSecret = Env.githubClientSecret env
  (GithubToken accessToken _ _) <- runGithubClient githubBaseURL (githubTokenClient clientId clientSecret code)
  pure (GithubAccessToken accessToken)

githubUser :: GithubAccessToken -> WebApp GithubUser
githubUser githubToken = do
  runGithubClient githubAPIBaseURL (githubUserClient $ tokenToAuthHeaderString githubToken)

tokenToAuthHeaderString :: GithubAccessToken -> Text
tokenToAuthHeaderString (GithubAccessToken token) = ("token " <> token)

-- | Fetch a user's primary github email.
-- Github's api docs suggest there will always be a primary email.
-- https://docs.github.com/en/rest/users/emails#list-email-addresses-for-the-authenticated-user
primaryGithubEmail :: GithubAccessToken -> WebApp GithubEmail
primaryGithubEmail auth = do
  emails <- runGithubClient githubAPIBaseURL (githubEmailsClient auth)
  -- Github's api docs suggest there will always be a primary email.
  -- https://docs.github.com/en/rest/users/emails#list-email-addresses-for-the-authenticated-user
  case find github_email_primary emails of
    Nothing -> respondError GithubUserWithoutPrimaryEmail
    Just email -> pure email

-- | The github.com url we redirect users to for them to authenticate to github
type GithubOauthApi =
  "login/oauth/authorize"
    :> QueryParam "scope" Text
    :> QueryParam "client_id" Text
    :> QueryParam "redirect_uri" URIParam
    :> QueryParam "login" Text
    :> QueryParam "state" OAuth2State
    :> Get '[JSON] ()

githubOauthApi :: Proxy GithubOauthApi
githubOauthApi = Proxy

githubAuthenticationURI :: OAuth2State -> WebApp URI
githubAuthenticationURI oauth2State = do
  oauthClientId <- asks Env.githubClientID
  redirectUri <- enlilPath ["oauth", "redirect"]
  let ghAuth = Just (URIAuth "" "github.com" "")
      ghLink = linkURI (uri oauthClientId redirectUri)
   in return $
        ghLink
          { uriScheme = "https:",
            uriPath = "/login/oauth/authorize",
            uriAuthority = ghAuth
          }
  where
    uri :: Text -> URI -> Link
    uri oauthClientId enUri =
      safeLink
        githubOauthApi
        githubOauthApi
        (Just "user:email") -- scope
        (Just oauthClientId) -- client_id (our app)
        (Just (URIParam enUri)) -- where to redirect on success
        (Nothing @Text) -- don't care which login
        (Just oauth2State) -- state to track who's being authed.

newtype GithubAccessToken = GithubAccessToken Text
  deriving (Show) via (Censored GithubAccessToken)
