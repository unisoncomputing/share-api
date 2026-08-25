{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Share.ChatApps.Types
  ( ChatProvider (..),
    MessageContent (..),
    ChatAppFailure (..),
    Author (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types ((.=))
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as POSIX
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI)
import Network.URI qualified as URI
import Share.Prelude
import Share.Utils.URI (uriToText)
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
        "avatar_url" .= unisonLogoImage,
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
    where
      unisonLogoImage :: URI
      unisonLogoImage =
        URI.parseURI "https://share.unison-lang.org/static/unison-logo-circle.png"
          & fromMaybe (error "unisonLogoImage: invalid URI")

-- | Nicely cut off text so that it doesn't exceed the max length
cutOffText :: Int -> Text -> Text
cutOffText maxLength text =
  if Text.length text > maxLength
    then Text.take (maxLength - 3) text <> "..."
    else text

data ChatAppFailure
  = InvalidRequest SomeException
  | ErrorResponse HTTP.Status
