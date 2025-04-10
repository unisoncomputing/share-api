{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | Common queries for users.
module Share.Postgres.Users.Queries
  ( userDisplayInfoOf,
    userIdsByHandlesOf,
    userProfileById,
    updateUser,
    expectUserByUserId,
    userByUserId,
    userByEmail,
    userByGithubUserId,
    userByHandle,
    createFromGithubUser,
    NewOrPreExisting (..),
    getNewOrPreExisting,
    isNew,
    findOrCreateGithubUser,
    searchUsersByNameOrHandlePrefix,
    UserCreationError (..),
    allUsers,
    createUser,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Servant qualified
import Share.Codebase qualified as Codebase
import Share.Github
import Share.IDs
import Share.IDs qualified as IDs
import Share.Postgres (unrecoverableError)
import Share.Postgres qualified as PG
import Share.Postgres.LooseCode.Queries qualified as LCQ
import Share.Prelude
import Share.User
import Share.UserProfile (UserProfile (..))
import Share.Utils.API
import Share.Utils.Logging qualified as Logging
import Share.Utils.Postgres
import Share.Utils.URI (URIParam (..))
import Share.Web.Authorization.Types qualified as AuthZ
import Share.Web.Errors (EntityMissing (EntityMissing), ErrorID (..), ToServerError (..))
import Share.Web.Share.DisplayInfo (UserDisplayInfo (..))

-- | Efficiently resolve User Display Info for UserIds within a structure.
userDisplayInfoOf :: (PG.QueryA m) => Traversal s t UserId UserDisplayInfo -> s -> m t
userDisplayInfoOf trav s = do
  s
    & unsafePartsOf trav %%~ \userIds -> do
      let usersTable = zip [0 :: Int32 ..] userIds
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
        <&> fmap
          ( \(handle, name, avatarUrl, userId) ->
              UserDisplayInfo
                { handle,
                  name,
                  avatarUrl = unpackURI <$> avatarUrl,
                  userId
                }
          )
        <&> \result ->
          if length result /= length userIds
            then error "userDisplayInfoOf: Missing user display info."
            else result

userIdsByHandlesOf :: (PG.QueryA m) => Traversal s t UserHandle UserId -> s -> m t
userIdsByHandlesOf trav s = do
  s
    & unsafePartsOf trav %%~ \userHandles -> do
      let usersTable = zip [0 :: Int32 ..] userHandles
      PG.queryListCol @(UserId)
        [PG.sql|
      WITH values(ord, handle) AS (
        SELECT * FROM ^{PG.toTable usersTable}
      )
      SELECT u.id
        FROM values
          JOIN users u ON u.handle = values.handle
      ORDER BY ord
      |]
        <&> \result ->
          if length result /= length userHandles
            then error "userIdsByHandlesOf: Missing user ids."
            else result

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

expectUserByUserId :: (PG.QueryM m) => UserId -> m User
expectUserByUserId uid = do
  userByUserId uid >>= \case
    Just user -> pure user
    Nothing -> unrecoverableError $ EntityMissing (ErrorID "user:missing") ("User with id " <> IDs.toText uid <> " not found")

userByUserId :: (PG.QueryM m) => UserId -> m (Maybe User)
userByUserId uid = do
  PG.query1Row
    [PG.sql|
        SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private
        FROM users u
        WHERE u.id = #{uid}
      |]

userByEmail :: Text -> PG.Transaction e (Maybe User)
userByEmail email = do
  PG.query1Row
    [PG.sql|
        SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private
        FROM users u
        WHERE lower(u.primary_email) = lower(#{email})
        LIMIT 1
      |]

userByGithubUserId :: Int64 -> PG.Transaction e (Maybe User)
userByGithubUserId githubUserId = do
  PG.query1Row
    [PG.sql|
        SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private
        FROM github_users gh
          JOIN users u
            ON gh.unison_user_id = u.id
        WHERE gh.github_user_id = #{githubUserId}
      |]

userByHandle :: UserHandle -> PG.Transaction e (Maybe User)
userByHandle handle = do
  PG.query1Row
    [PG.sql|
        SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private
        FROM users u
        WHERE u.handle = lower(#{handle})
      |]

createFromGithubUser :: AuthZ.AuthZReceipt -> GithubUser -> GithubEmail -> PG.Transaction UserCreationError User
createFromGithubUser !authzReceipt (GithubUser githubHandle githubUserId avatar_url user_name) primaryEmail = do
  let (GithubEmail {github_email_email = user_email, github_email_verified = emailVerified}) = primaryEmail
  userHandle <- case IDs.fromText @UserHandle (Text.toLower githubHandle) of
    Left err -> throwError (InvalidUserHandle err githubHandle)
    Right handle -> pure handle
  userId <- createUser authzReceipt user_email user_name (Just avatar_url) userHandle emailVerified
  PG.execute_
    [PG.sql|
          INSERT INTO github_users
            (github_user_id, unison_user_id)
            VALUES (#{githubUserId}, #{userId})
        |]
  let codebase = Codebase.codebaseEnv authzReceipt (Codebase.codebaseLocationForUserCodebase userId)
  Codebase.codebaseMToTransaction codebase LCQ.initialize
  let visibility = UserPublic
  pure $
    User
      { handle = userHandle,
        avatar_url = Just avatar_url,
        user_id = userId,
        user_name,
        user_email,
        visibility
      }

-- | Note: Since there's currently no way to choose a handle during user creation,
-- manually creating users that aren't mapped to a github user WILL lock out any github
-- user by that name from creating a share account. Use caution.
createUser :: AuthZ.AuthZReceipt -> Text -> Maybe Text -> Maybe URIParam -> UserHandle -> Bool -> PG.Transaction UserCreationError UserId
createUser !_authZReceipt userEmail userName avatarUrl userHandle emailVerified = do
  handleExists <-
    PG.queryExpect1Col
      [PG.sql|
        SELECT EXISTS (SELECT from users WHERE handle = #{userHandle})
      |]
  if handleExists
    then do
      throwError $ UserHandleTaken userHandle
    else do
      -- All users are created public, private users are currently only possible via
      -- manual Postgres manipulation.
      let private = False
      PG.queryExpect1Col
        [PG.sql|
              INSERT INTO users
                (primary_email, email_verified, avatar_url, name, handle, private)
                VALUES (#{userEmail}, #{emailVerified}, #{avatarUrl}, #{userName}, #{userHandle}, #{private})
              RETURNING id
            |]

data NewOrPreExisting a
  = New a
  | PreExisting a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

getNewOrPreExisting :: NewOrPreExisting a -> a
getNewOrPreExisting (New a) = a
getNewOrPreExisting (PreExisting a) = a

isNew :: NewOrPreExisting a -> Bool
isNew New {} = True
isNew _ = False

findOrCreateGithubUser :: AuthZ.AuthZReceipt -> GithubUser -> GithubEmail -> PG.Transaction UserCreationError (NewOrPreExisting User)
findOrCreateGithubUser authZReceipt ghu@(GithubUser _login githubUserId _avatarUrl _name) primaryEmail = do
  user <- userByGithubUserId githubUserId
  case user of
    Just user' -> pure (PreExisting user')
    Nothing -> do
      New <$> createFromGithubUser authZReceipt ghu primaryEmail

searchUsersByNameOrHandlePrefix :: Query -> Limit -> PG.Transaction e [(User, Maybe OrgId)]
searchUsersByNameOrHandlePrefix (Query prefix) (Limit limit) = do
  let q = likeEscape prefix <> "%"
  PG.queryListRows @(User PG.:. (PG.Only (Maybe OrgId)))
    [PG.sql|
    SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private, org.id
      FROM users u
      LEFT JOIN orgs org ON org.user_id = u.id
      WHERE (u.handle ILIKE #{q}
             OR u.name ILIKE #{q}
            ) AND NOT u.private
      LIMIT #{limit}
      |]
    <&> fmap \(user PG.:. PG.Only mayOrgId) -> (user, mayOrgId)

data UserCreationError
  = UserHandleTaken UserHandle
  | -- A given user handle isn't valid according to Share.
    -- This shouldn't happen for Github Handles, but in the case it does, we throw an error.
    -- (Error Message, Invalid Handle)
    InvalidUserHandle Text Text

instance Logging.Loggable UserCreationError where
  toLog = \case
    UserHandleTaken handle ->
      Logging.textLog ("User handle taken: " <> Text.pack (show handle))
        & Logging.withSeverity Logging.UserFault
    InvalidUserHandle err handle ->
      Logging.textLog ("Invalid user handle: " <> Text.pack (show err) <> " " <> handle)
        & Logging.withSeverity Logging.UserFault

instance ToServerError UserCreationError where
  toServerError = \case
    UserHandleTaken {} -> (ErrorID "user-creation:handle-taken", Servant.err409 {Servant.errBody = "User handle taken."})
    InvalidUserHandle err _handle -> (ErrorID "user-creation:invalid-handle", Servant.err400 {Servant.errBody = "Invalid user handle: " <> BL.fromStrict (Text.encodeUtf8 err)})

allUsers :: PG.Transaction e [UserId]
allUsers = do
  PG.queryListCol
    [PG.sql|
        SELECT id FROM users
      |]
