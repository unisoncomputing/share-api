{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Queries where

import Control.Monad.Except
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Share.Branch
import Share.Codebase qualified as Codebase
import Share.Contribution
import Share.Github
import Share.IDs
import Share.IDs qualified as IDs
import Share.OAuth.Types
import Share.Postgres (unrecoverableError)
import Share.Postgres qualified as PG
import Share.Postgres.IDs
import Share.Postgres.LooseCode.Queries qualified as LCQ
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.Project
import Share.Release
import Share.Ticket (TicketStatus)
import Share.Ticket qualified as Ticket
import Share.User
import Share.Utils.API
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (EntityMissing (EntityMissing), ErrorID (..))
import Share.Web.Share.Branches.Types (BranchKindFilter (..))
import Share.Web.Share.Projects.Types (ContributionStats (..), DownloadStats (..), FavData, ProjectOwner, TicketStats (..))
import Share.Web.Share.Releases.Types (ReleaseStatusFilter (..), StatusUpdate (..))
import Unison.Util.List qualified as Utils
import Unison.Util.Monoid (intercalateMap)

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

projectById :: ProjectId -> PG.Transaction e (Maybe Project)
projectById projectId = do
  PG.query1Row
    [PG.sql|
        SELECT p.id, p.owner_user_id, p.slug, p.summary, p.tags, p.private, p.created_at, p.updated_at
        FROM projects p
        WHERE p.id = #{projectId}
      |]

expectProjectById :: ProjectId -> PG.Transaction e Project
expectProjectById projectId = do
  mayResult <- projectById projectId
  whenNothing mayResult $ unrecoverableError $ EntityMissing (ErrorID "project:missing") ("Project with id " <> IDs.toText projectId <> " not found")

-- | returns (project, favData, projectOwner, default branch, latest release version)
projectByIdWithMetadata :: Maybe UserId -> ProjectId -> PG.Transaction e (Maybe (Project, FavData, ProjectOwner, Maybe BranchName, Maybe ReleaseVersion))
projectByIdWithMetadata caller projectId = do
  PG.query1Row sql <&> fmap \(p PG.:. favData PG.:. projectOwner PG.:. (branchName, major, minor, patch)) -> (p, favData, projectOwner, branchName, releaseVersionFromInts major minor patch)
  where
    releaseVersionFromInts :: Maybe Int64 -> Maybe Int64 -> Maybe Int64 -> Maybe ReleaseVersion
    releaseVersionFromInts major minor patch =
      ReleaseVersion <$> (fromIntegral <$> major) <*> (fromIntegral <$> minor) <*> (fromIntegral <$> patch)
    -- Select the project, also include the number of favs on the project and whether the
    -- caller has fav'd it, and the default branch and latest release version if they exist.
    sql =
      [PG.sql|
      SELECT p.id, p.owner_user_id, p.slug, p.summary, p.tags, p.private, p.created_at, p.updated_at,
        (SELECT COUNT(*) FROM project_favorites pf WHERE pf.project_id = p.id) AS fav_count,
        (SELECT EXISTS (SELECT FROM project_favorites pf WHERE pf.project_id = p.id AND pf.user_id = #{caller}) AS is_faved),
        owner.handle,
        owner.name,
        EXISTS (SELECT FROM org_members WHERE org_members.organization_user_id = owner.id),
        -- Check if the default branch exists
        (SELECT b.name FROM project_branches b WHERE b.project_id = p.id AND b.name = #{defaultBranchName} AND b.deleted_at IS NULL LIMIT 1),
        -- Get the latest release version
        release.major_version,
        release.minor_version,
        release.patch_version
      FROM projects p
        JOIN users owner ON owner.id = p.owner_user_id
        LEFT JOIN LATERAL (SELECT r.major_version, r.minor_version, r.patch_version FROM project_releases r WHERE r.project_id = p.id ORDER BY r.major_version DESC, r.minor_version DESC, r.patch_version DESC, r.id DESC LIMIT 1) release ON true
      WHERE p.id = #{projectId}
      |]

projectIDFromHandleAndSlug :: UserHandle -> ProjectSlug -> PG.Transaction e (Maybe ProjectId)
projectIDFromHandleAndSlug userHandle slug =
  PG.query1Col
    [PG.sql|
        SELECT projects.id
        FROM projects
        JOIN users on users.handle = #{userHandle}
        WHERE projects.owner_user_id = users.id
              AND projects.slug = #{slug}
      |]

projectIDFromUserIdAndSlug :: UserId -> ProjectSlug -> PG.Transaction e (Maybe ProjectId)
projectIDFromUserIdAndSlug userId slug =
  PG.query1Col
    [PG.sql|
        SELECT projects.id
        FROM projects
        WHERE projects.owner_user_id = #{userId}
              AND projects.slug = #{slug}
      |]

-- | Get the UserId of a project's owner.
projectOwner :: ProjectId -> PG.Transaction e (Maybe UserId)
projectOwner projectID = do
  PG.query1Col
    [PG.sql|
        SELECT owner_user_id
        FROM projects
        WHERE projects.id = #{projectID}
      |]

setProjectFav :: UserId -> ProjectId -> Bool -> PG.Transaction e ()
setProjectFav userId projectId fav = do
  case fav of
    True -> PG.execute_ favSql
    False -> PG.execute_ unfavSql
  where
    unfavSql =
      [PG.sql|
        DELETE FROM project_favorites
        WHERE project_id = #{projectId}
              AND user_id = #{userId}
      |]
    favSql =
      [PG.sql|
        INSERT INTO project_favorites (project_id, user_id)
        VALUES (#{projectId}, #{userId})
        ON CONFLICT DO NOTHING
      |]

data UserCreationError
  = UserHandleTaken UserHandle
  | -- A given user handle isn't valid according to Share.
    -- This shouldn't happen for Github Handles, but in the case it does, we throw an error.
    -- (Error Message, Invalid Handle)
    InvalidUserHandle Text Text

createFromGithubUser :: GithubUser -> GithubEmail -> PG.Transaction e (Either UserCreationError User)
createFromGithubUser (GithubUser githubHandle githubUserId avatar_url user_name) primaryEmail = runExceptT do
  let (GithubEmail {github_email_email = user_email, github_email_verified = emailVerified}) = primaryEmail
  userHandle <- case IDs.fromText @UserHandle (Text.toLower githubHandle) of
    Left err -> throwError (InvalidUserHandle err githubHandle)
    Right handle -> pure handle
  handleExists <-
    lift $
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
      user_id <-
        lift $
          PG.queryExpect1Col
            [PG.sql|
                            INSERT INTO users
                              (primary_email, email_verified, avatar_url, name, handle, private)
                              VALUES (#{user_email}, #{emailVerified}, #{avatar_url}, #{user_name}, #{userHandle}, #{private})
                            RETURNING id
                          |]
      lift $
        PG.execute_
          [PG.sql|
          INSERT INTO github_users
            (github_user_id, unison_user_id)
            VALUES (#{githubUserId}, #{user_id})
        |]
      let codebase = Codebase.codebaseEnv AuthZ.userCreationOverride (Codebase.codebaseLocationForUserCodebase user_id)
      lift $ Codebase.codebaseMToTransaction codebase LCQ.initialize
      let visibility = UserPublic
      pure $
        User
          { handle = userHandle,
            avatar_url = Just avatar_url,
            user_id,
            user_name,
            user_email,
            visibility
          }

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

findOrCreateGithubUser :: GithubUser -> GithubEmail -> PG.Transaction e (Either UserCreationError (NewOrPreExisting User))
findOrCreateGithubUser ghu@(GithubUser _login githubUserId _avatarUrl _name) primaryEmail = do
  user <- userByGithubUserId githubUserId
  case user of
    Just user' -> return $ Right (PreExisting user')
    Nothing -> do
      fmap New <$> createFromGithubUser ghu primaryEmail

searchUsersByNameOrHandlePrefix :: Query -> Limit -> PG.Transaction e [User]
searchUsersByNameOrHandlePrefix (Query prefix) (Limit limit) = do
  let q = likeEscape prefix <> "%"
  PG.queryListRows
    [PG.sql|
    SELECT u.id, u.name, u.primary_email, u.avatar_url, u.handle, u.private
      FROM users u
      WHERE (u.handle ILIKE #{q}
             OR u.name ILIKE #{q}
            ) AND NOT u.private
      LIMIT #{limit}
      |]

-- | Perform a text search over projects and return Limit results in order of relevance.
--
-- The PG.queryListRows accepts strings as web search queries, see
-- https://www.postgresql.org/docs/current/textsearch-controls.html
searchProjectsByUserQuery :: Maybe UserId -> Query -> Limit -> PG.Transaction e [(Project, UserHandle)]
searchProjectsByUserQuery caller (Query query) limit = do
  let prefixQuery =
        query
          -- Remove any chars with special meaning for tsqueries.
          & Text.filter (\c -> Char.isAlphaNum c || Char.isSpace c || c `elem` ['_', '-'])
          & \case
            -- Empty prefix searches are invalid, Nullify the prefix search.
            "" -> Nothing
            txt ->
              -- Convert into a quoted prefix query expression.
              Just $ "'" <> txt <> "':*"
  results <-
    PG.queryListRows
      [PG.sql|
    SELECT p.id, p.owner_user_id, p.slug, p.summary, p.tags, p.private, p.created_at, p.updated_at, owner.handle
      FROM websearch_to_tsquery('english', #{query}) AS webquery, to_tsquery('english', #{prefixQuery}) AS prefixquery, projects AS p
        JOIN users AS owner ON p.owner_user_id = owner.id
      WHERE (webquery @@ p.project_text_document OR prefixquery @@ p.project_text_document)
      AND (NOT p.private OR (#{caller} IS NOT NULL AND EXISTS (SELECT FROM accessible_private_projects WHERE user_id = #{caller} AND project_id = p.id)))
      ORDER BY (ts_rank_cd(p.project_text_document, webquery), ts_rank_cd(p.project_text_document, prefixquery)) DESC
      LIMIT #{limit}
      |]
  pure (results <&> \(project PG.:. PG.Only handle) -> (project, handle))

-- | Returns the list of tours the user has completed.
getCompletedToursForUser :: UserId -> PG.Transaction e [TourId]
getCompletedToursForUser uid = do
  PG.queryListCol
    [PG.sql|
    SELECT tour_id
    FROM tours
    WHERE tours.user_id = #{uid}
      |]

-- | Marks a set of tours as completed.
completeToursForUser :: UserId -> NonEmpty TourId -> PG.Transaction e ()
completeToursForUser uid tours = do
  let toursToInsert = ((uid,) <$> toList tours)
  PG.execute_
    [PG.sql|
    INSERT INTO tours(user_id, tour_id)
      SELECT * FROM ^{PG.toTable toursToInsert}
      ON CONFLICT DO NOTHING
      |]

allUsers :: PG.Transaction e [UserId]
allUsers = do
  PG.queryListCol
    [PG.sql|
        SELECT id FROM users
      |]

-- | Return all projects
allProjects :: PG.Transaction e [ProjectId]
allProjects = do
  PG.queryListCol
    [PG.sql|
        SELECT id FROM projects
      |]

-- | Returns all projects owned by that user.
-- Note that the user may still have access to other projects via orgs.
projectIdsOwnedByUser :: UserId -> Bool -> PG.Transaction e [ProjectId]
projectIdsOwnedByUser userId includePrivate = do
  PG.queryListCol
    [PG.sql|
        SELECT id FROM projects
          WHERE owner_user_id = #{userId}
            AND CASE
                WHEN #{includePrivate} THEN true
                ELSE private = false
                END
      |]

projectsOwnedByUser :: UserId -> Bool -> PG.Transaction e [Project]
projectsOwnedByUser userId includePrivate = do
  PG.queryListRows
    [PG.sql|
        SELECT id, owner_user_id, slug, summary, tags, private, created_at, updated_at FROM projects
          WHERE owner_user_id = #{userId}
            AND CASE
                WHEN #{includePrivate} THEN true
                ELSE private = false
                END
      |]

listProjectsByUserWithMetadata ::
  Maybe UserId ->
  UserId ->
  -- | (project, numFavs, isFavedByCaller)
  PG.Transaction e [(Project, FavData, ProjectOwner)]
listProjectsByUserWithMetadata callerUserId projectOwnerUserId = do
  unpackRows
    <$> PG.queryListRows @(Project PG.:. FavData PG.:. ProjectOwner)
      -- Selects all projects belonging to a user which are visible to the caller.
      -- Join in whether the caller has favorited the project,
      -- and how many favorites the project has in total.
      [PG.sql|
        SELECT
          p.id,
          p.owner_user_id,
          p.slug,
          p.summary,
          p.tags,
          p.private,
          p.created_at,
          p.updated_at,
          (SELECT COUNT(*) FROM project_favorites WHERE project_favorites.project_id = p.id) AS num_favs,
          (SELECT EXISTS(SELECT FROM project_favorites f WHERE f.project_id = p.id AND f.user_id = #{callerUserId}) AS is_faved),
          owner.handle,
          owner.name,
          EXISTS (SELECT FROM org_members WHERE org_members.organization_user_id = owner.id) AS is_org
        FROM projects p
          JOIN users owner ON owner.id = p.owner_user_id
        WHERE p.owner_user_id = #{projectOwnerUserId}
          AND (EXISTS (SELECT FROM accessible_private_projects accessible
                      WHERE accessible.user_id = #{callerUserId}
                        AND accessible.project_id = p.id
                     )
                OR NOT p.private
              )
        ORDER BY p.created_at DESC
      |]
  where
    unpackRows :: [Project PG.:. FavData PG.:. ProjectOwner] -> [(Project, FavData, ProjectOwner)]
    unpackRows = fmap \(project PG.:. favData PG.:. projectOwner) ->
      (project, favData, projectOwner)

listProjectsFromCatalogWithMetadata ::
  Maybe UserId ->
  -- | (project, numFavs, isFavedByCaller)
  PG.Transaction e (Map CategoryName [(Project, FavData, ProjectOwner)])
listProjectsFromCatalogWithMetadata callerUserId = do
  projects :: [(Project PG.:. PG.Only CategoryName PG.:. FavData PG.:. ProjectOwner)] <- PG.queryListRows sql
  projects
    & Utils.groupBy (\(_ PG.:. PG.Only category PG.:. _) -> category)
    & (fmap . fmap) (\(p PG.:. _category PG.:. favData PG.:. projectOwner) -> (p, favData, projectOwner))
    & pure
  where
    -- Selects all projects belonging to a user, join in whether the caller
    -- has favorited the project, and how many favorites the project has in total.
    sql =
      [PG.sql|
        SELECT
          p.id,
          p.owner_user_id,
          p.slug,
          p.summary,
          p.tags,
          p.private,
          p.created_at,
          p.updated_at,
          cc.name,
          (SELECT COUNT(*) FROM project_favorites WHERE project_favorites.project_id = p.id) AS num_favs,
          (SELECT EXISTS(SELECT FROM project_favorites f WHERE f.project_id = p.id AND f.user_id = #{callerUserId}) AS is_faved),
          owner.handle,
          owner.name,
          EXISTS (SELECT FROM org_members WHERE org_members.organization_user_id = owner.id)
        FROM project_categories pc
             JOIN catalog_categories cc ON cc.id = pc.category_id
             JOIN projects p ON pc.project_id = p.id
             JOIN users owner ON p.owner_user_id = owner.id
        WHERE p.private = false
        ORDER BY cc.name
      |]

addProjectToCatalogCategory :: ProjectId -> CategoryID -> PG.Transaction e ()
addProjectToCatalogCategory projectId categoryId = do
  PG.execute_
    [PG.sql|
        INSERT INTO project_categories(project_id, category_id)
          VALUES (#{projectId}, #{categoryId})
          ON CONFLICT DO NOTHING
      |]

removeProjectFromCatalogCategory :: ProjectId -> CategoryID -> PG.Transaction e ()
removeProjectFromCatalogCategory projectId categoryId = do
  PG.execute_
    [PG.sql|
        DELETE FROM project_categories
          WHERE project_id = #{projectId} AND category_id = #{categoryId}
      |]

-- | Get or create catalog category.
getOrCreateCatalogCategory :: CategoryName -> PG.Transaction e CategoryID
getOrCreateCatalogCategory categoryName = do
  PG.query1Col getCatSql >>= \case
    Just catId -> pure catId
    Nothing -> PG.queryExpect1Col insertCatSql
  where
    getCatSql =
      [PG.sql|
        SELECT id FROM catalog_categories
          WHERE name = #{categoryName}
      |]
    insertCatSql =
      [PG.sql|
        INSERT INTO catalog_categories(name)
          VALUES (#{categoryName})
          RETURNING id
      |]

-- | Get or create catalog category.
getCatalogCategory :: CategoryName -> PG.Transaction e (Maybe CategoryID)
getCatalogCategory categoryName = do
  PG.query1Col
    [PG.sql|
        SELECT id FROM catalog_categories
          WHERE name = #{categoryName}
      |]

createProject :: UserId -> ProjectSlug -> Maybe Text -> Set ProjectTag -> ProjectVisibility -> PG.Transaction e ProjectId
createProject ownerUserId slug summary tags visibility = do
  let tagsList = Set.toList tags
  PG.queryExpect1Col
    [PG.sql|
        INSERT INTO projects(owner_user_id, slug, summary, tags, private)
          VALUES (#{ownerUserId}, #{slug}, #{summary}, #{tagsList}, #{visibility})
          RETURNING id;
      |]

-- | Returns false if project could not be found.
--
-- Must be called with a transaction to be safe.
updateProject :: ProjectId -> NullableUpdate Text -> SetUpdate ProjectTag -> Maybe ProjectVisibility -> PG.Transaction e Bool
updateProject projectId newSummary tagChanges newVisibility =
  -- This method is a bit naive, we just get the old project, update the fields accordingly,
  -- then save the entire project again.
  isJust <$> runMaybeT do
    Project {..} <- lift $ expectProjectById projectId
    let updatedSummary = fromNullableUpdate summary newSummary
    let updatedTags = Set.toList $ applySetUpdate tags tagChanges
    let updatedVisibility = fromMaybe visibility newVisibility
    lift $
      PG.execute_
        [PG.sql|
      UPDATE projects
      SET
        summary = #{updatedSummary},
        tags = #{updatedTags},
        private = #{updatedVisibility}
      WHERE
        projects.id = #{projectId}
      |]

-- | Deletes a project and anything referencing it entirely. No tombstones are left behind.
deleteProject :: ProjectId -> PG.Transaction e ()
deleteProject projectId =
  PG.execute_
    [PG.sql|
      DELETE FROM projects
      WHERE
        projects.id = #{projectId}
      |]

branchByProjectIdAndShortHand :: ProjectId -> BranchShortHand -> PG.Transaction e (Maybe (Branch CausalId))
branchByProjectIdAndShortHand projectId BranchShortHand {contributorHandle, branchName} = runMaybeT do
  mayContributorUserId <- case contributorHandle of
    Nothing -> pure Nothing
    Just handle -> do
      User {user_id} <- MaybeT (userByHandle handle)
      pure (Just user_id)

  MaybeT $
    PG.query1Row
      [PG.sql|
        SELECT
          id,
          project_id,
          name,
          contributor_id,
          causal_id,
          merge_target_branch_id,
          created_at,
          updated_at,
          creator_id
        FROM project_branches
        WHERE project_id = #{projectId}
              AND (contributor_id = #{mayContributorUserId} OR (#{mayContributorUserId} IS NULL AND contributor_id IS NULL))
              AND name = #{branchName}
              AND deleted_at IS NULL
      |]

branchById :: BranchId -> PG.Transaction e (Maybe (Branch CausalId))
branchById branchId = do
  PG.query1Row
    [PG.sql|
        SELECT
          id,
          project_id,
          name,
          contributor_id,
          causal_id,
          merge_target_branch_id,
          created_at,
          updated_at,
          creator_id
        FROM project_branches
        WHERE id = #{branchId}
              AND deleted_at IS NULL
      |]

branchByProjectBranchShortHand :: ProjectBranchShortHand -> PG.Transaction e (Maybe (Branch CausalId))
branchByProjectBranchShortHand ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName} = do
  PG.query1Row
    [PG.sql|
        SELECT
          b.id,
          b.project_id,
          b.name,
          b.contributor_id,
          b.causal_id,
          b.merge_target_branch_id,
          b.created_at,
          b.updated_at,
          b.creator_id
        FROM project_branches b
          JOIN projects p ON p.id = b.project_id
          JOIN users u ON u.id = p.owner_user_id
          LEFT JOIN users c ON c.id = b.contributor_id
        WHERE
          u.handle = #{userHandle}
          AND p.slug = #{projectSlug}
          AND (c.handle = #{contributorHandle} OR (#{contributorHandle} IS NULL AND c.handle IS NULL))
          AND b.deleted_at IS NULL
          AND b.name = #{branchName}
      |]

projectBranchShortHandByBranchId :: BranchId -> PG.Transaction e (Maybe ProjectBranchShortHand)
projectBranchShortHandByBranchId branchId = do
  PG.query1Row
    [PG.sql|
        SELECT
          u.handle,
          p.slug,
          c.handle,
          b.name
        FROM project_branches b
          JOIN projects p ON p.id = b.project_id
          JOIN users u ON u.id = p.owner_user_id
          LEFT JOIN users c ON c.id = b.contributor_id
        WHERE
          b.id = #{branchId}
          AND b.deleted_at IS NULL
      |]
    <&> fmap \(userHandle, projectSlug, contributorHandle, branchName) ->
      ProjectBranchShortHand
        { userHandle,
          projectSlug,
          contributorHandle,
          branchName
        }

-- | Sets the 'deleted_at' field on the branch so it won't appear
-- in queries, but the row is kept.
softDeleteBranch :: BranchId -> PG.Transaction e ()
softDeleteBranch branchId = do
  PG.execute_
    [PG.sql|
        UPDATE project_branches
        SET deleted_at = NOW()
        WHERE id = #{branchId}
      |]

projectByShortHand :: ProjectShortHand -> PG.Transaction e (Maybe Project)
projectByShortHand ProjectShortHand {userHandle, projectSlug} = do
  PG.query1Row
    [PG.sql|
        SELECT
          p.id,
          p.owner_user_id,
          p.slug,
          p.summary,
          p.tags,
          p.private,
          p.created_at,
          p.updated_at
        FROM projects p
          JOIN users u ON u.id = p.owner_user_id
        WHERE u.handle = #{userHandle}
          AND p.slug = #{projectSlug}
      |]

createBranch ::
  NameLookupReceipt ->
  ProjectId ->
  BranchName ->
  Maybe UserId ->
  CausalId ->
  Maybe BranchId ->
  UserId ->
  PG.Transaction e BranchId
createBranch !_nlReceipt projectId branchName contributorId causalId mergeTarget creatorId = do
  branchId <- PG.queryExpect1Col createBranchSQL
  PG.execute_ (updateReflogSQL branchId ("Branch Created" :: Text))
  pure branchId
  where
    createBranchSQL =
      [PG.sql|
        INSERT INTO project_branches(
          project_id,
          name,
          contributor_id,
          creator_id,
          causal_id,
          merge_target_branch_id
        )
        VALUES (
          #{projectId},
          #{branchName},
          #{contributorId},
          #{creatorId},
          #{causalId},
          -- It's possible the merge target is from a deleted project, so we select to null it out if it no longer exists.
          (SELECT merge_target.id FROM project_branches merge_target WHERE merge_target.id = #{mergeTarget})
        ) RETURNING id
      |]
    updateReflogSQL branchId description =
      [PG.sql|
        INSERT INTO project_branch_reflog(
          branch_id,
          old_causal_id,
          new_causal_id,
          user_id,
          description
        )
        SELECT
          #{branchId},
          NULL,
          #{causalId},
          #{creatorId},
          #{description}
        FROM project_branches
        WHERE id = #{branchId}
      |]

createRelease ::
  (PG.QueryM m) =>
  NameLookupReceipt ->
  ProjectId ->
  ReleaseVersion ->
  CausalId ->
  CausalId ->
  UserId ->
  m (Release CausalId UserId)
createRelease !_nlReceipt projectId ReleaseVersion {major, minor, patch} squashedCausalId unsquashedCausalId creatorId = do
  PG.queryExpect1Row
    [PG.sql|
        INSERT INTO project_releases(
          project_id,
          created_by,
          squashed_causal_id,
          unsquashed_causal_id,
          major_version,
          minor_version,
          patch_version
        )
        VALUES (#{projectId}, #{creatorId}, #{squashedCausalId}, #{unsquashedCausalId}, #{major}, #{minor}, #{patch})
        RETURNING
          id,
          project_id,
          unsquashed_causal_id,
          squashed_causal_id,
          created_at,
          updated_at,
          created_at,
          created_by,
          deprecated_at,
          deprecated_by,
          created_by,
          major_version,
          minor_version,
          patch_version
      |]

setBranchCausalHash ::
  NameLookupReceipt ->
  Text ->
  UserId ->
  BranchId ->
  CausalId ->
  PG.Transaction e ()
setBranchCausalHash !_nameLookupReceipt description callerUserId branchId causalId = do
  PG.execute_ updateReflogSQL
  PG.execute_ setBranchSQL
  where
    setBranchSQL =
      [PG.sql|
        UPDATE project_branches
        SET causal_id = #{causalId},
            updated_at = NOW()
        WHERE id = #{branchId}
      |]
    updateReflogSQL =
      [PG.sql|
        INSERT INTO project_branch_reflog(
          branch_id,
          old_causal_id,
          new_causal_id,
          user_id,
          description
        )
        SELECT
          #{branchId},
          project_branches.causal_id,
          #{causalId},
          #{callerUserId},
          #{description}
        FROM project_branches
        WHERE id = #{branchId}
      |]

getCausalIdForBranch :: BranchId -> PG.Transaction e CausalId
getCausalIdForBranch branchId = do
  PG.queryExpect1Col sql
  where
    sql =
      [PG.sql|
        SELECT causal_id FROM project_branches
          WHERE id = #{branchId}
                AND deleted_at IS NULL
      |]

listBranchesByProject ::
  Limit ->
  Maybe (Cursor (UTCTime, BranchId)) ->
  Maybe Query ->
  -- Either a contributor user Id or contributor handle prefix
  Maybe (Either UserId Query) ->
  BranchKindFilter ->
  ProjectId ->
  -- | (branch, contributorHandle)
  PG.Transaction e [(Branch CausalId, Maybe UserHandle)]
listBranchesByProject limit mayCursor mayBranchNamePrefix mayContributorQuery kind projectId = do
  let kindFilter = case kind of
        AllBranchKinds -> ""
        OnlyContributorBranches -> "AND b.contributor_id IS NOT NULL"
        OnlyCoreBranches -> "AND b.contributor_id IS NULL"
  let contributorFilter = case mayContributorQuery of
        Nothing -> mempty
        -- Allow null contributor here for the case where we're listing 'all' branch kinds.
        Just (Left contributorId) -> [PG.sql| AND (b.contributor_id IS NULL OR (b.contributor_id = #{contributorId})) |]
        Just (Right (Query contributorHandlePrefix)) -> [PG.sql| AND (contributor.handle IS NULL OR starts_with(contributor.handle, #{contributorHandlePrefix})) |]
  let branchNameFilter = case mayBranchNamePrefix of
        Nothing -> mempty
        Just (Query branchNamePrefix) -> [PG.sql| AND starts_with(b.name, #{branchNamePrefix}) |]
  let cursorFilter = case mayCursor of
        Nothing -> mempty
        Just (Cursor (beforeTime, branchId)) -> [PG.sql| AND (b.updated_at, b.id) < (#{beforeTime}, #{branchId})|]
  let sql =
        intercalateMap
          "\n"
          id
          [ ( [PG.sql|
        SELECT
          b.id,
          b.project_id,
          b.name,
          b.contributor_id,
          b.causal_id,
          b.merge_target_branch_id,
          b.created_at,
          b.updated_at,
          b.creator_id,
          contributor.handle
        FROM project_branches b
        LEFT JOIN users AS contributor ON contributor.id = b.contributor_id
        WHERE
          b.deleted_at IS NULL
          AND b.project_id = #{projectId}
          |]
            ),
            kindFilter,
            contributorFilter,
            branchNameFilter,
            cursorFilter,
            ( [PG.sql|
        ORDER BY b.updated_at DESC, b.id DESC
        LIMIT #{limit}
      |]
            )
          ]
  PG.queryListRows sql
    <&> fmap (\(branch PG.:. PG.Only contributorHandle) -> (branch, contributorHandle))

-- | List all BranchHashes which are reachable within a given user's codebase.
accessibleCausalsForUser ::
  UserId ->
  PG.Transaction e [CausalId]
accessibleCausalsForUser userId = do
  PG.queryListCol
    [PG.sql|
  -- include core branches
  SELECT DISTINCT causal_id
    FROM project_branches
      JOIN projects ON projects.id = project_branches.project_id
      JOIN users ON users.id = projects.owner_user_id
    WHERE
      project_branches.deleted_at IS NULL
      AND users.id = #{userId}
      AND project_branches.contributor_id IS NULL
  -- include contributor branches
  UNION
  SELECT DISTINCT causal_id
    FROM project_branches
    WHERE
      project_branches.deleted_at IS NULL
      AND project_branches.contributor_id = #{userId}
  -- include releases
  UNION
  SELECT DISTINCT squashed_causal_id
    FROM project_releases
      JOIN projects ON projects.id = project_releases.project_id
      JOIN users ON users.id = projects.owner_user_id
    WHERE
      project_releases.deleted_at IS NULL
      AND users.id = #{userId}
  UNION
  SELECT DISTINCT unsquashed_causal_id
    FROM project_releases
      JOIN projects ON projects.id = project_releases.project_id
      JOIN users ON users.id = projects.owner_user_id
    WHERE
      project_releases.deleted_at IS NULL
      AND users.id = #{userId}
  -- include loose code root
  UNION
  SELECT DISTINCT causal_id
    FROM loose_code_roots
    WHERE
      loose_code_roots.user_id = #{userId}
    |]

-- | List all contributor branches of a given user which the caller has access to.
listContributorBranchesOfUserAccessibleToCaller ::
  UserId ->
  Maybe UserId ->
  Limit ->
  Maybe (Cursor (UTCTime, BranchId)) ->
  Maybe Query ->
  Maybe ProjectId ->
  -- | (branch, project, projectOwnerHandle)
  PG.Transaction e [(Branch CausalId, Project, ProjectOwner)]
listContributorBranchesOfUserAccessibleToCaller contributorUserId mayCallerUserId limit mayCursor mayBranchNamePrefix mayProjectId = do
  let branchNameFilter = case mayBranchNamePrefix of
        Nothing -> mempty
        Just (Query branchNamePrefix) -> [PG.sql| AND starts_with(b.name, #{branchNamePrefix}) |]
  let cursorFilter = case mayCursor of
        Nothing -> mempty
        Just (Cursor (beforeTime, branchId)) -> [PG.sql| AND (b.updated_at, b.id) < (#{beforeTime}, #{branchId}) |]
  let projectFilter = case mayProjectId of
        Nothing -> mempty
        Just projId -> [PG.sql| AND b.project_id = #{projId} |]
  let callerFilter = case mayCallerUserId of
        Just callerUserId ->
          -- See any projects that are public or that the caller has access to.
          ( [PG.sql| AND (
                NOT project.private
                OR EXISTS (
                  SELECT FROM accessible_private_projects ap
                  WHERE
                    ap.user_id = #{callerUserId}
                    AND ap.project_id = project.id
                  )
                )
          |]
          )
        Nothing ->
          -- No caller auth means they only see public projects
          [PG.sql| AND NOT project.private |]
  let sql =
        intercalateMap
          "\n"
          id
          [ [PG.sql|
        SELECT
          b.id,
          b.project_id,
          b.name,
          b.contributor_id,
          b.causal_id,
          b.merge_target_branch_id,
          b.created_at,
          b.updated_at,
          b.creator_id,
          project.id,
          project.owner_user_id,
          project.slug,
          project.summary,
          project.tags,
          project.private,
          project.created_at,
          project.updated_at,
          project_owner.handle,
          project_owner.name,
          EXISTS (SELECT FROM org_members WHERE org_members.organization_user_id = project.owner_user_id)
        FROM project_branches b
        JOIN projects project ON project.id = b.project_id
        JOIN users AS project_owner ON project_owner.id = project.owner_user_id
        WHERE
          b.deleted_at IS NULL
          AND b.contributor_id = #{contributorUserId}
          |],
            branchNameFilter,
            cursorFilter,
            projectFilter,
            callerFilter,
            [PG.sql|
        ORDER BY b.updated_at DESC, b.id DESC
        LIMIT #{limit}
      |]
          ]
  PG.queryListRows sql
    <&> fmap (\(branch PG.:. project PG.:. projectOwner) -> (branch, project, projectOwner))

-- | Returns Project Owner information, including whether that user is an organization or not.
projectOwnerByProjectId :: ProjectId -> PG.Transaction e (Maybe ProjectOwner)
projectOwnerByProjectId projectId = do
  listToMaybe
    <$> PG.queryListRows
      [PG.sql|
        SELECT
          project_owner.handle,
          project_owner.name,
          EXISTS (SELECT FROM org_members WHERE org_members.organization_user_id = project_owner.id)
        FROM projects
        JOIN users AS project_owner ON project_owner.id = projects.owner_user_id
        WHERE projects.id = #{projectId};
      |]

releaseDownloadStatsForProject :: ProjectId -> PG.Transaction e DownloadStats
releaseDownloadStatsForProject projectId = do
  downloadsInLast28Days <-
    PG.queryListCol
      [PG.sql|
        SELECT SUM(downloads)
        FROM project_release_daily_downloads
        JOIN project_releases ON project_releases.id = project_release_daily_downloads.release_id
        WHERE project_releases.project_id = #{projectId}
              AND day >= NOW() - INTERVAL '28 days'
        GROUP BY day, project_id
        ORDER BY day DESC;
      |]
  pure $ DownloadStats downloadsInLast28Days

contributionStatsForProject :: ProjectId -> PG.Transaction e ContributionStats
contributionStatsForProject projectId = do
  statusCounts <-
    PG.queryListRows @(ContributionStatus, Int64)
      [PG.sql|
      SELECT status, COUNT(*)
        FROM contributions
        WHERE project_id = #{projectId}
        GROUP BY status
      |]
  let (Sum draft, Sum inReview, Sum closed, Sum merged) =
        statusCounts
          & foldMap \case
            (Draft, count) -> (Sum $ fromIntegral count, 0, 0, 0)
            (InReview, count) -> (0, Sum $ fromIntegral count, 0, 0)
            (Closed, count) -> (0, 0, Sum $ fromIntegral count, 0)
            (Merged, count) -> (0, 0, 0, Sum $ fromIntegral count)
  pure $ ContributionStats {draft, inReview, closed, merged}

ticketStatsForProject :: ProjectId -> PG.Transaction e TicketStats
ticketStatsForProject projectId = do
  statusCounts <-
    PG.queryListRows @(TicketStatus, Int64)
      [PG.sql|
      SELECT status, COUNT(*)
        FROM tickets
        WHERE project_id = #{projectId}
        GROUP BY status
      |]
  let (Sum numOpenTickets, Sum numClosedTickets) =
        statusCounts
          & foldMap \case
            (Ticket.Open, count) -> (fromIntegral count, mempty)
            (Ticket.Closed, count) -> (mempty, fromIntegral count)
  pure $ TicketStats {numOpenTickets, numClosedTickets}

isUnisonEmployee :: UserId -> PG.Transaction e Bool
isUnisonEmployee uid = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT EXISTS (SELECT FROM org_members
                      JOIN users AS org_user ON organization_user_id = org_user.id
                      WHERE member_user_id = #{uid}
                            AND org_user.handle = 'unison')
      |]

-- | Returns the handles of all orgs the provided user is a member of.
organizationMemberships :: UserId -> PG.Transaction e [UserHandle]
organizationMemberships uid = do
  PG.queryListCol
    [PG.sql|
        SELECT org_user.handle FROM users AS org_user
          JOIN org_members ON organization_user_id = org_user.id
          WHERE member_user_id = #{uid}
      |]

releaseById :: ReleaseId -> PG.Transaction e (Maybe (Release CausalId UserId))
releaseById releaseId = do
  PG.query1Row
    [PG.sql|
        SELECT
          release.id,
          release.project_id,
          release.unsquashed_causal_id,
          release.squashed_causal_id,
          release.created_at,
          release.updated_at,
          release.created_at,
          release.created_by,
          release.deprecated_at,
          release.deprecated_by,
          release.created_by,
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
        WHERE release.id = #{releaseId}
      |]

expectReleaseById :: ReleaseId -> PG.Transaction e (Release CausalId UserId)
expectReleaseById releaseId = do
  mayRow <- releaseById releaseId
  whenNothing mayRow $ unrecoverableError $ EntityMissing (ErrorID "release:missing") ("Release with id " <> IDs.toText releaseId <> " not found")

releaseByProjectReleaseShortHand :: ProjectReleaseShortHand -> PG.Transaction e (Maybe (Release CausalId UserId))
releaseByProjectReleaseShortHand ProjectReleaseShortHand {userHandle, projectSlug, releaseVersion = ReleaseVersion {major, minor, patch}} = do
  PG.query1Row
    [PG.sql|
        SELECT
          release.id,
          release.project_id,
          release.unsquashed_causal_id,
          release.squashed_causal_id,
          release.created_at,
          release.updated_at,
          release.created_at,
          release.created_by,
          release.deprecated_at,
          release.deprecated_by,
          release.created_by,
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
             JOIN projects AS project ON project.id = release.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
        WHERE project_owner.handle = #{userHandle}
              AND project.slug = #{projectSlug}
              AND release.major_version = #{major}
              AND release.minor_version = #{minor}
              AND release.patch_version = #{patch}
              AND release.deleted_at IS NULL
      |]

releaseByProjectIdAndReleaseShortHand :: ProjectId -> ReleaseShortHand -> PG.Transaction e (Maybe (Release CausalId UserId))
releaseByProjectIdAndReleaseShortHand projectId ReleaseShortHand {releaseVersion = ReleaseVersion {major, minor, patch}} = do
  PG.query1Row
    [PG.sql|
        SELECT
          release.id,
          release.project_id,
          release.unsquashed_causal_id,
          release.squashed_causal_id,
          release.created_at,
          release.updated_at,
          release.created_at,
          release.created_by,
          release.deprecated_at,
          release.deprecated_by,
          release.created_by,
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
        WHERE release.project_id = #{projectId}
              AND release.major_version = #{major}
              AND release.minor_version = #{minor}
              AND release.patch_version = #{patch}
              AND release.deleted_at IS NULL
      |]

listReleasesByProject ::
  Limit ->
  Maybe (Cursor (Int64, Int64, Int64, ReleaseId)) ->
  Maybe Query ->
  ReleaseStatusFilter ->
  ProjectId ->
  PG.Transaction e [Release CausalId UserHandle]
listReleasesByProject limit mayCursor mayVersionPrefix status projectId = do
  let statusFilter = case status of
        AllReleases -> ""
        OnlyPublished -> [PG.sql| AND deprecated_at IS NULL |]
        OnlyDeprecated -> [PG.sql| AND deprecated_at IS NOT NULL |]
  let versionFilters :: [PG.Sql]
      versionFilters =
        case mayVersionPrefix of
          Nothing -> []
          Just (Query versionPrefix) ->
            let numericalPrefix =
                  versionPrefix
                    & Text.splitOn "."
                    & fmap (readMaybe @Int64 . Text.unpack)
                    & List.takeWhile isJust
                    & catMaybes
                numericalFilters = case numericalPrefix of
                  [] -> []
                  [major] -> do
                    [[PG.sql| AND major_version = #{major} |]]
                  [major, minor] -> do
                    [[PG.sql| AND major_version = #{major} AND minor_version = #{minor} |]]
                  -- Just ignore any trailing components
                  (major : minor : patch : _) -> do
                    [[PG.sql| AND major_version = #{major} AND minor_version = #{minor} AND patch_version = #{patch} |]]
             in numericalFilters
  let cursorFilter = case mayCursor of
        Nothing -> mempty
        Just (Cursor (major, minor, patch, releaseId)) ->
          [PG.sql| AND (release.major_version, release.minor_version, release.patch_version, release.id) < (#{major}, #{minor}, #{patch}, #{releaseId}) |]
  let (sql) =
        intercalateMap
          "\n"
          id
          ( [ ( [PG.sql|SELECT
                release.id,
                release.project_id,
                release.unsquashed_causal_id,
                release.squashed_causal_id,
                release.created_at,
                release.updated_at,
                release.created_at,
                publisher.handle,
                release.deprecated_at,
                deprecator.handle,
                publisher.handle,
                release.major_version,
                release.minor_version,
                release.patch_version
                FROM project_releases AS release
                LEFT JOIN users AS publisher
                  ON publisher.id = release.created_by
                LEFT JOIN users AS deprecator
                  ON deprecator.id = release.deprecated_by
                WHERE release.project_id = #{projectId}
                      AND release.deleted_at IS NULL
                      |]
              )
            ]
              <> versionFilters
              <> [ statusFilter,
                   cursorFilter,
                   [PG.sql| ORDER BY release.major_version DESC, release.minor_version DESC, release.patch_version DESC, release.id DESC|],
                   [PG.sql| LIMIT #{limit} |]
                 ]
          )
  PG.queryListRows sql

-- | Add one to the daily downloads of a release.
incrementReleaseDownloads :: ReleaseId -> PG.Transaction e ()
incrementReleaseDownloads releaseId = do
  PG.execute_
    [PG.sql|
      INSERT INTO project_release_daily_downloads (release_id, day, downloads)
        VALUES (#{releaseId}, CURRENT_DATE, 1)
        ON CONFLICT (release_id, day)
          DO UPDATE
          SET downloads = project_release_daily_downloads.downloads + 1
      |]

-- | Add one to the daily downloads of a branch.
incrementBranchDownloads :: BranchId -> PG.Transaction e ()
incrementBranchDownloads branchId = do
  PG.execute_
    [PG.sql|
      INSERT INTO project_branch_daily_downloads (branch_id, day, downloads)
        VALUES (#{branchId}, CURRENT_DATE, 1)
        ON CONFLICT (branch_id, day)
          DO UPDATE
          SET downloads = project_branch_daily_downloads.downloads + 1
      |]

likeEscape :: Text -> Text
likeEscape = Text.replace "%" "\\%" . Text.replace "_" "\\_"

data UpdateReleaseResult
  = UpdateRelease'Success
  | UpdateRelease'NotFound
  | UpdateRelease'CantPublishDeprecated

updateRelease :: UserId -> ReleaseId -> Maybe StatusUpdate -> PG.Transaction e UpdateReleaseResult
updateRelease caller releaseId newStatus = do
  fromMaybe UpdateRelease'NotFound <$> runMaybeT do
    Release {..} <- lift $ expectReleaseById releaseId
    -- Can go from draft -> published -> deprecated
    -- or straight from draft -> deprecated
    -- but can't go from published -> draft or deprecated -> draft.
    case (status, newStatus) of
      (_, Nothing) ->
        -- No-op
        pure UpdateRelease'Success
      (DeprecatedRelease {}, Just MakePublished) -> do
        pure UpdateRelease'CantPublishDeprecated
      (PublishedRelease {}, Just MakePublished) ->
        -- No-op
        pure UpdateRelease'Success
      (PublishedRelease {}, Just MakeDeprecated) -> do
        lift makeDeprecated
        pure UpdateRelease'Success
      (DeprecatedRelease {}, Just MakeDeprecated) -> do
        -- No-op
        pure UpdateRelease'Success
  where
    makeDeprecated = do
      PG.execute_
        [PG.sql|
          UPDATE project_releases
          SET
            deprecated_at = NOW(),
            deprecated_by = #{caller}
          WHERE
            id = #{releaseId}
        |]

latestReleaseVersionByProjectShorthand :: ProjectShortHand -> PG.Transaction e (Maybe ReleaseVersion)
latestReleaseVersionByProjectShorthand ProjectShortHand {userHandle, projectSlug} = do
  PG.query1Row
    [PG.sql|
        SELECT
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
             JOIN projects AS project ON project.id = release.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
        WHERE project_owner.handle = #{userHandle}
              AND project.slug = #{projectSlug}
              AND release.deleted_at IS NULL
        ORDER BY release.major_version DESC, release.minor_version DESC, release.patch_version DESC
        LIMIT 1
      |]

getOAuthConfigForClient :: OAuthClientId -> PG.Transaction e (Maybe OAuthClientConfig)
getOAuthConfigForClient clientId = do
  PG.query1Row
    [PG.sql|
        SELECT
          client_id,
          client_secret,
          redirect_host,
          approved_scopes,
          audience
        FROM oauth_clients
        WHERE client_id = #{clientId}
      |]
