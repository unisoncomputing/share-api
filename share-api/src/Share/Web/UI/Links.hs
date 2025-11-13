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
import Share.Env.Types qualified as Env
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
projectBranchBrowseLink :: (MonadReader (Env.Env ctx) m) => ProjectBranchShortHand -> m URI
projectBranchBrowseLink (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}) = do
  let branchPath = case contributorHandle of
        Just contributor -> [IDs.toText contributor, IDs.toText branchName]
        Nothing -> [IDs.toText branchName]
      path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "code"] <> branchPath <> ["latest"]
  shareUIPath path

-- E.g. https://share.unison-lang.org/@unison/base/contributions/100
contributionLink :: (MonadReader (Env.Env ctx) m) => ProjectShortHand -> ContributionNumber -> m URI
contributionLink (ProjectShortHand {userHandle, projectSlug}) contributionNumber = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "contributions", IDs.toText contributionNumber]
  shareUIPath path

contributionCommentLink :: (MonadReader (Env.Env ctx) m) => ProjectShortHand -> ContributionNumber -> CommentId -> m URI
contributionCommentLink (ProjectShortHand {userHandle, projectSlug}) contributionNumber _commentId = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "contributions", IDs.toText contributionNumber]
  shareUIPath path

ticketLink :: (MonadReader (Env.Env ctx) m) => ProjectShortHand -> TicketNumber -> m URI
ticketLink (ProjectShortHand {userHandle, projectSlug}) ticketNumber = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "tickets", IDs.toText ticketNumber]
  shareUIPath path

ticketCommentLink :: (MonadReader (Env.Env ctx) m) => ProjectShortHand -> TicketNumber -> CommentId -> m URI
ticketCommentLink (ProjectShortHand {userHandle, projectSlug}) ticketNumber _commentId = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "tickets", IDs.toText ticketNumber]
  shareUIPath path

releaseLink :: (MonadReader (Env.Env ctx) m) => ProjectShortHand -> ReleaseVersion -> m URI
releaseLink (ProjectShortHand {userHandle, projectSlug}) releaseVersion = do
  let path = [IDs.toText (PrefixedID @"@" userHandle), IDs.toText projectSlug, "releases", IDs.toText releaseVersion]
  shareUIPath path

-- | Where the user should go when clicking on a notification
notificationLink :: (MonadReader (Env.Env ctx) m) => HydratedEventPayload -> m URI
notificationLink = \case
  HydratedProjectBranchUpdatedPayload _project branch ->
    projectBranchBrowseLink branch.projectBranchShortHand
  HydratedProjectContributionCreatedPayload project contribution ->
    contributionLink project.projectShortHand contribution.contributionNumber
  HydratedProjectContributionStatusUpdatedPayload project contribution _status ->
    contributionLink project.projectShortHand contribution.contributionNumber
  HydratedProjectContributionCommentPayload project contribution comment ->
    contributionCommentLink project.projectShortHand contribution.contributionNumber comment.commentId
  HydratedProjectTicketCreatedPayload project ticket ->
    ticketLink project.projectShortHand ticket.ticketNumber
  HydratedProjectTicketStatusUpdatedPayload project ticket _status ->
    ticketLink project.projectShortHand ticket.ticketNumber
  HydratedProjectTicketCommentPayload project ticket comment ->
    ticketCommentLink project.projectShortHand ticket.ticketNumber comment.commentId
  HydratedProjectReleaseCreatedPayload project release ->
    releaseLink project.projectShortHand release.releaseVersion

unisonLogoImage :: URI
unisonLogoImage =
  URI.parseURI "https://share.unison-lang.org/static/unison-logo-circle.png"
    & fromMaybe (error "unisonLogoImage: invalid URI")

----------- Utilities -----------

-- | Construct a full URI to a path within share, with provided query params.
sharePathQ :: (MonadReader (Env.Env ctx) m) => [Text] -> Map Text Text -> m URI
sharePathQ pathSegments queryParams = do
  uri <- asks Env.apiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ uri

-- | Construct a full URI to a path within share.
sharePath :: (MonadReader (Env.Env ctx) m) => [Text] -> m URI
sharePath path = sharePathQ path mempty

-- | Check if a URI is a the Share UI, the Cloud UI, the main website, or the
-- Cloud website. This is useful for preventing attackers from generating
-- arbitrary redirections in things like login redirects.
isTrustedURI :: (MonadReader (Env.Env ctx) m) => URI -> m Bool
isTrustedURI uri = do
  shareUiURI <- asks Env.shareUiOrigin
  websiteURI <- asks Env.websiteOrigin
  cloudUiURI <- asks Env.cloudUiOrigin
  cloudWebsiteURI <- asks Env.cloudWebsiteOrigin
  let requestedAuthority = uriAuthority uri
  let trustedURIs = [shareUiURI, websiteURI, cloudUiURI, cloudWebsiteURI]
  pure $ any (\uri -> uriAuthority uri == requestedAuthority) trustedURIs

-- | Construct a full URI to a path within the share UI, with the provided query params.
shareUIPathQ :: (MonadReader (Env.Env ctx) m) => [Text] -> Map Text Text -> m URI
shareUIPathQ pathSegments queryParams = do
  shareUiURI <- asks Env.shareUiOrigin
  pure . setPathAndQueryParams pathSegments queryParams $ shareUiURI

shareUIPath :: (MonadReader (Env.Env ctx) m) => [Text] -> m URI
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

errorRedirectLink :: (MonadReader (Env.Env ctx) m) => ShareUIError -> m URI
errorRedirectLink shareUIError = shareUIPathQ ["error"] (Map.fromList [("appError", shareUIErrorToUIText shareUIError)])

-- | Redirect the user to the Share UI and show an error message.
errorRedirect :: ShareUIError -> WebApp a
errorRedirect shareUIError = do
  errURI <- errorRedirectLink shareUIError
  respondError $ ErrorRedirect (shareUIErrorToUIText shareUIError) errURI
