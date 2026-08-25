{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Share.ChatApps.Utils
  ( shareAuthor,
    authorFromUserId,
  )
where

import Network.URI qualified as URI
import Share.App (AppM)
import Share.ChatApps.Types
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.User (User (..))
import Share.Utils.Tags (HasTags)
import Share.Utils.URI (URIParam (..))
import Share.Web.UI.Links qualified as Links

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

shareAuthor :: Author
shareAuthor =
  let shareAuthorAvatarUrl = fromMaybe (error "Invalid shareAuthorAvatarUrl") $ URI.parseURI "https://share.unison-lang.org/static/unison-logo-circle.png"
   in Author
        { authorName = Just "Share",
          authorLink = Nothing,
          authorAvatarUrl = Just shareAuthorAvatarUrl
        }
