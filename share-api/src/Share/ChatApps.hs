{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Share.ChatApps
  ( ChatProvider (..),
    MessageContent (..),
    ChatAppFailure (..),
    Author (..),
    sendMessage,
    shareAuthor,
    authorFromUserId,
  )
where

import Control.Monad.Trans.Except (except, runExceptT)
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types ((.=))
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as POSIX
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI)
import Network.URI qualified as URI
import Share.App (AppM)
import Share.Env qualified as Env
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.User (User (..))
import Share.Utils.Tags (HasTags)
import Share.Utils.URI (URIParam (..), uriToText)
import Share.Web.UI.Links qualified as Links
import UnliftIO (SomeException)

data ChatProvider
  = Slack
  | Discord
  deriving stock (Show, Eq)

data Author = Author
  { authorName :: Maybe Text,
    authorLink :: Maybe URI,
    authorAvatarUrl :: Maybe URI
  }
  deriving (Show, Eq, Ord)

authorFromUserId :: (HasTags reqCtx) => UserId -> AppM reqCtx Author
authorFromUserId userId = do
  User {avatar_url = avatarUrl, user_name, handle} <- PG.runTransaction $ UsersQ.expectUser userId
  authorLink <- Links.userProfilePage handle
  pure $
    Author
      { authorName = user_name,
        authorLink = Just authorLink,
        authorAvatarUrl = unpackURI <$> avatarUrl
      }

-- A type to unify slack and discord message types
data MessageContent (provider :: ChatProvider) = MessageContent
  { -- Text of the bot message
    preText :: Text,
    -- Title of the attachment
    title :: Text,
    -- Text of the attachment
    content :: Text,
    -- Title link
    mainLink :: Maybe URI,
    author :: Author,
    thumbnailUrl :: Maybe URI,
    timestamp :: UTCTime
  }
  deriving stock (Show, Eq)

instance ToJSON (MessageContent 'Slack) where
  toJSON MessageContent {preText, content, title, mainLink, author = Author {authorName, authorLink, authorAvatarUrl}, thumbnailUrl, timestamp} =
    Aeson.object
      [ "text" .= preText,
        "attachments"
          .= [ Aeson.object
                 ( [ "title" .= cutOffText 250 title,
                     "text" .= content,
                     "author_name" .= authorName,
                     "author_icon" .= fmap uriToText authorAvatarUrl,
                     "thumb_url" .= fmap uriToText thumbnailUrl,
                     "ts" .= (round (POSIX.utcTimeToPOSIXSeconds timestamp) :: Int64),
                     "color" .= ("#36a64f" :: Text)
                   ]
                     <> (mainLink & foldMap (\mainURI -> ["title_link" .= uriToText mainURI]))
                     <> (authorLink & foldMap (\authorURI -> ["author_link" .= uriToText authorURI]))
                 )
             ]
      ]

instance ToJSON (MessageContent 'Discord) where
  toJSON MessageContent {preText, content, title, mainLink, author = Author {authorName, authorLink, authorAvatarUrl}, thumbnailUrl, timestamp} =
    Aeson.object
      [ "username" .= ("Share Notifications" :: Text),
        "avatar_url" .= Links.unisonLogoImage,
        "content" .= cutOffText 1950 preText,
        "embeds"
          .= [ Aeson.object
                 ( [ "title" .= cutOffText 250 title,
                     "description" .= cutOffText 4000 content,
                     "author"
                       .= Aeson.object
                         ( [ "name" .= (cutOffText 250 <$> authorName),
                             "icon_url" .= fmap uriToText authorAvatarUrl
                           ]
                             <> (authorLink & foldMap (\authorURI -> ["url" .= uriToText authorURI]))
                         ),
                     "timestamp" .= (Just $ Text.pack $ Time.formatTime Time.defaultTimeLocale "%FT%T%QZ" timestamp),
                     "thumbnail" .= fmap (\url -> Aeson.object ["url" .= uriToText url]) thumbnailUrl
                   ]
                     <> (mainLink & foldMap (\mainURI -> ["url" .= uriToText mainURI]))
                 )
             ]
      ]

-- | Nicely cut off text so that it doesn't exceed the max length
cutOffText :: Int -> Text -> Text
cutOffText maxLength text =
  if Text.length text > maxLength
    then Text.take (maxLength - 3) text <> "..."
    else text

shareAuthor :: Author
shareAuthor =
  let shareAuthorAvatarUrl = fromMaybe (error "Invalid shareAuthorAvatarUrl") $ URI.parseURI "https://share.unison-lang.org/static/unison-logo-circle.png"
   in Author
        { authorName = Just "Share",
          authorLink = Nothing,
          authorAvatarUrl = Just shareAuthorAvatarUrl
        }

data ChatAppFailure
  = InvalidRequest SomeException
  | ErrorResponse HTTP.Status

chatAppTimeout :: HTTPClient.ResponseTimeout
chatAppTimeout = HTTPClient.responseTimeoutMicro $ 10 * 1_000_000 -- 10 seconds

sendMessage :: (ToJSON (MessageContent provider)) => URI -> MessageContent provider -> AppM reqCtx (Either ChatAppFailure ())
sendMessage uri messageContent = runExceptT do
  req <-
    HTTPClient.requestFromURI uri
      & mapLeft InvalidRequest
      & except
  let req' =
        req
          { HTTPClient.method = "POST",
            HTTPClient.responseTimeout = chatAppTimeout,
            HTTPClient.requestHeaders = [(HTTP.hContentType, "application/json")],
            HTTPClient.requestBody = HTTPClient.RequestBodyLBS $ Aeson.encode messageContent
          }
  proxiedHTTPManager <- asks Env.proxiedHttpClient
  resp <- liftIO $ HTTPClient.httpLbs req' proxiedHTTPManager
  case HTTPClient.responseStatus resp of
    httpStatus@(HTTP.Status status _)
      | status >= 400 -> throwError $ ErrorResponse httpStatus
      | otherwise -> pure ()
