{-# LANGUAGE DataKinds #-}

module Share.Web.UI.Links
  ( oauthRedirect,
    oauthRedirectWithCodeAndState,
    oauthAuthorize,
    oauthToken,
    openIDUserInfo,
    jwksURI,
    handleTakenRedirect,
    homePage,
    ucmConnected,
    userProfilePage,
    notificationLink,
    projectBranchBrowseLink,
    contributionLink,
    unisonLogoImage,

    -- * Utilities
    isTrustedURI,
    errorRedirectLink,
    errorRedirect,
    ShareUIError (..),
    shareUIErrorToUIText,
    UIEvents (..),
  )
where

import Control.Monad.Reader
import Data.Map qualified as Map
import Network.URI
import Network.URI qualified as URI
import Servant (ToHttpApiData (..))
import Share.App
import Share.Env qualified as Env
import Share.IDs
import Share.IDs qualified as IDs
import Share.Notifications.Types
import Share.OAuth.Types (OAuth2State)
import Share.Prelude
import Share.Utils.URI (setPathAndQueryParams)
import Share.Web.App (WebApp)
import Share.Web.Errors (ErrorRedirect (..), respondError)

oauthRedirect :: AppM reqCtx URI
oauthRedirect = sharePath ["oauth", "redirect"]

oauthRedirectWithCodeAndState :: Text -> OAuth2State -> AppM reqCtx URI
oauthRedirectWithCodeAndState code oauth2State = do
  sharePathQ ["oauth", "redirect"] $
    Map.fromList
      [ ("code", code),
        ("state", toQueryParam oauth2State)
      ]

oauthAuthorize :: AppM reqCtx URI
oauthAuthorize = sharePath ["oauth", "authorize"]

oauthToken :: AppM reqCtx URI
oauthToken = sharePath ["oauth", "token"]

openIDUserInfo :: AppM reqCtx URI
openIDUserInfo = sharePath ["user-info"]

jwksURI :: AppM reqCtx URI
jwksURI = sharePath [".well-known", "jwks.json"]

handleTakenRedirect :: UserHandle -> AppM reqCtx URI
handleTakenRedirect userHandle = shareUIPathQ ["finish-signup"] (Map.fromList [("state", "handle-taken"), ("conflictingHandle", IDs.toText userHandle)])

ucmConnected :: AppM reqCtx URI
ucmConnected = shareUIPath ["ucm-connected"]

userProfilePage :: UserHandle -> AppM reqCtx URI
userProfilePage userHandle = do
  shareUIPath ["@" <> IDs.toText userHandle]

data UIEvents
  = NewUserLogIn
  | LogIn
  | LogOut
  deriving (Show, Eq)

homePage :: Maybe UIEvents -> AppM reqCtx URI
homePage mayEvent = do
  shareUIPathQ [] q
  where
    q = case mayEvent of
      Nothing -> mempty
      Just event -> case event of
        NewUserLogIn -> Map.singleton "event" "new-user-log-in"
        LogIn -> Map.singleton "event" "log-in"
        LogOut -> Map.singleton "event" "log-out"

-- E.g. https://share.unison-lang.org/@unison/base/code/@ceedubs/each-first/latest
projectBranchBrowseLink :: ProjectBranchShortHand -> AppM reqCtx URI
projectBranchBrowseLink (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}) = do
  let branchPath = case contributorHandle of
        Just contributor -> [IDs.toText contributor, IDs.toText branchName]
        Nothing -> [IDs.toText branchName]
      path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "code"] <> branchPath <> ["latest"]
  shareUIPath path

-- E.g. https://share.unison-lang.org/@unison/base/contributions/100
contributionLink :: ProjectShortHand -> ContributionNumber -> AppM reqCtx URI
contributionLink (ProjectShortHand {userHandle, projectSlug}) contributionNumber = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "contributions", IDs.toText contributionNumber]
  shareUIPath path

-- | Where the user should go when clicking on a notification
notificationLink :: HydratedEventPayload -> AppM reqCtx URI
notificationLink = \case
  HydratedProjectBranchUpdatedPayload payload ->
    projectBranchBrowseLink payload.branchInfo.projectBranchShortHand
  HydratedProjectContributionCreatedPayload payload ->
    contributionLink payload.projectInfo.projectShortHand payload.contributionInfo.contributionNumber

unisonLogoImage :: URI
unisonLogoImage =
  URI.parseURI "https://share.unison-lang.org/static/unison-logo-circle.png"
    & fromMaybe (error "unisonLogoImage: invalid URI")

----------- Utilities -----------

-- | Construct a full URI to a path within share, with provided query params.
sharePathQ :: [Text] -> Map Text Text -> AppM reqCtx URI
sharePathQ pathSegments queryParams = do
  uri <- asks Env.apiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ uri

-- | Construct a full URI to a path within share.
sharePath :: [Text] -> AppM reqCtx URI
sharePath path = sharePathQ path mempty

-- | Check if a URI is a the Share UI, the Cloud UI, the main website, or the
-- Cloud website. This is useful for preventing attackers from generating
-- arbitrary redirections in things like login redirects.
isTrustedURI :: URI -> AppM reqCtx Bool
isTrustedURI uri = do
  shareUiURI <- asks Env.shareUiOrigin
  websiteURI <- asks Env.websiteOrigin
  cloudUiURI <- asks Env.cloudUiOrigin
  cloudWebsiteURI <- asks Env.cloudWebsiteOrigin
  let requestedAuthority = uriAuthority uri
  let trustedURIs = [shareUiURI, websiteURI, cloudUiURI, cloudWebsiteURI]
  pure $ any (\uri -> uriAuthority uri == requestedAuthority) trustedURIs

-- | Construct a full URI to a path within the share UI, with the provided query params.
shareUIPathQ :: [Text] -> Map Text Text -> AppM reqCtx URI
shareUIPathQ pathSegments queryParams = do
  shareUiURI <- asks Env.shareUiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ shareUiURI

shareUIPath :: [Text] -> AppM reqCtx URI
shareUIPath pathSegments = shareUIPathQ pathSegments mempty

-- | Various Error types that the Share UI knows how to interpret
data ShareUIError
  = UnspecifiedError
  | AccountCreationGitHubPermissionsRejected
  | AccountCreationInvalidHandle Text {- the invalid handle -}
  deriving (Show)

-- | Convert ShareUIError to the Text that the UI expects
shareUIErrorToUIText :: ShareUIError -> Text
shareUIErrorToUIText e =
  case e of
    UnspecifiedError ->
      "UnspecifiedError"
    AccountCreationGitHubPermissionsRejected {} ->
      "AccountCreationGitHubPermissionsRejected"
    AccountCreationInvalidHandle {} ->
      "AccountCreationInvalidHandle"

errorRedirectLink :: ShareUIError -> AppM reqCtx URI
errorRedirectLink shareUIError = shareUIPathQ ["error"] (Map.fromList [("appError", shareUIErrorToUIText shareUIError)])

-- | Redirect the user to the Share UI and show an error message.
errorRedirect :: ShareUIError -> WebApp a
errorRedirect shareUIError = do
  errURI <- errorRedirectLink shareUIError
  respondError $ ErrorRedirect (shareUIErrorToUIText shareUIError) errURI
