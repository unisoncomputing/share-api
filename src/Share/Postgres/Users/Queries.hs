{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Common queries for users.
module Share.Postgres.Users.Queries
  ( userDisplayInfoOf,
    userProfileById,
    updateUser,
  )
where

import Control.Lens
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude
import Share.UserProfile (UserProfile (..))
import Share.Utils.API (NullableUpdate, fromNullableUpdate)
import Share.Utils.URI (URIParam (..))
import Share.Web.Share.Types

-- | Efficiently resolve User Display Info for UserIds within a structure.
userDisplayInfoOf :: Traversal s t UserId UserDisplayInfo -> s -> PG.Transaction e t
userDisplayInfoOf trav s = do
  s
    & unsafePartsOf trav %%~ \userIds -> do
      let usersTable = zip [0 :: Int32 ..] userIds
      result <-
        PG.queryListRows @(UserHandle, Maybe Text, Maybe URIParam, UserId)
          [PG.sql|
      WITH values(ord, user_id) AS (
        SELECT * FROM ^{PG.toTable usersTable}
      )
      SELECT users.handle, users.name, users.avatar_url, users.id
        FROM values
          JOIN users ON users.id = values.user_id
      ORDER BY ord
      |]
          <&> fmap \(handle, name, avatarUrl, userId) ->
            UserDisplayInfo
              { handle,
                name,
                avatarUrl = unpackURI <$> avatarUrl,
                userId
              }
      if length result /= length userIds
        then error "userDisplayInfoOf: Missing user display info."
        else pure result

userProfileById :: UserId -> PG.Transaction e (Maybe UserProfile)
userProfileById userId = do
  PG.query1Row
    [PG.sql|
        SELECT u.id, u.name, u.avatar_url, u.handle, u.bio, u.website, u.location, u.twitterHandle, u.pronouns
        FROM users u
        WHERE u.id = #{userId}
      |]

updateUser ::
  UserId ->
  NullableUpdate Text ->
  NullableUpdate URIParam ->
  NullableUpdate Text ->
  NullableUpdate Text ->
  NullableUpdate Text ->
  NullableUpdate Text ->
  NullableUpdate Text ->
  PG.Transaction e ()
updateUser toUpdateUserId newName newAvatarUrl newBio newWebsite newLocation newTwitterHandle newPronouns = void . runMaybeT $ do
  UserProfile
    { user_name = existingName,
      avatar_url = existingAvatarUrl,
      bio = existingBio,
      website = existingWebsite,
      location = existingLocation,
      twitterHandle = existingTwitterHandle,
      pronouns = existingPronouns
    } <-
    MaybeT $ userProfileById toUpdateUserId
  let updatedName = fromNullableUpdate existingName newName
  let updatedAvatarUrl = fromNullableUpdate existingAvatarUrl newAvatarUrl
  let updatedBio = fromNullableUpdate existingBio newBio
  let updatedWebsite = fromNullableUpdate existingWebsite newWebsite
  let updatedLocation = fromNullableUpdate existingLocation newLocation
  let updatedTwitterHandle = fromNullableUpdate existingTwitterHandle newTwitterHandle
  let updatedPronouns = fromNullableUpdate existingPronouns newPronouns
  PG.execute_
    [PG.sql|
      UPDATE users
      SET
        name = #{updatedName},
        avatar_url = #{updatedAvatarUrl},
        bio = #{updatedBio},
        website = #{updatedWebsite},
        location = #{updatedLocation},
        twitterHandle = #{updatedTwitterHandle},
        pronouns = #{updatedPronouns}
      WHERE id = #{toUpdateUserId}
  |]
