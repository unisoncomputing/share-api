{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Projects.Queries
  ( isPremiumProject,
    listProjectMaintainers,
    addMaintainers,
    updateMaintainers,
    expectProjectShortHandsOf,
  )
where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Share.IDs
import Share.Postgres
import Share.Prelude
import Share.Web.Authorization.Types (ProjectMaintainerPermissions (..))
import Share.Web.Share.Projects.Types (Maintainer (..))

isPremiumProject :: ProjectId -> Transaction e Bool
isPremiumProject projId =
  fromMaybe False <$> do
    query1Col
      [sql|
    SELECT EXISTS (SELECT FROM premium_projects WHERE project_id = #{projId})
    |]

listProjectMaintainers :: ProjectId -> Transaction e [Maintainer UserId]
listProjectMaintainers projId = do
  results <-
    queryListRows @(UserId, Bool, Bool, Bool)
      [sql|
    SELECT pm.user_id, pm.can_view, pm.can_maintain, pm.can_admin
        FROM project_maintainers pm 
          JOIN users u ON pm.user_id = u.id
          WHERE pm.project_id = #{projId}
      -- Just need some order to make tests deterministic
      ORDER BY u.handle ASC
    |]
  results
    & fmap \case
      (userId, canView, canMaintain, canAdmin) ->
        let permissions = ProjectMaintainerPermissions {canView, canMaintain, canAdmin}
         in Maintainer {user = userId, permissions}
    & pure

addMaintainers :: ProjectId -> [Maintainer UserId] -> Transaction e (Either [UserId] [Maintainer UserId])
addMaintainers projId maintainers = runExceptT $ do
  let userIds = fmap user maintainers
  -- Check if any of the maintainers already exist
  alreadyExistingUserIds <-
    lift $
      queryListCol @UserId
        [sql|
      WITH values(user_id) AS (
        SELECT * FROM ^{singleColumnTable userIds}
      )
      SELECT values.user_id FROM values
          WHERE EXISTS (SELECT FROM project_maintainers pm
                         WHERE pm.project_id = #{projId}
                           AND pm.user_id = values.user_id
                       )
    |]

  case alreadyExistingUserIds of
    (_ : _) -> throwError alreadyExistingUserIds
    [] -> do
      let newMaintainersTable = maintainers <&> \Maintainer {user, permissions = ProjectMaintainerPermissions {canView, canMaintain, canAdmin}} -> (projId, user, canView, canMaintain, canAdmin)
      -- Insert the maintainers
      lift $
        execute_
          [sql|
        WITH values(project_id, user_id, can_view, can_maintain, can_admin) AS (
          SELECT * FROM ^{toTable newMaintainersTable}
        ) INSERT INTO project_maintainers (project_id, user_id, can_view, can_maintain, can_admin)
          SELECT v.project_id, v.user_id, v.can_view, v.can_maintain, v.can_admin
            FROM values v
        |]
      lift $ listProjectMaintainers projId

updateMaintainers :: ProjectId -> [Maintainer UserId] -> Transaction e (Either [UserId] [Maintainer UserId])
updateMaintainers projId maintainers = runExceptT $ do
  let userIds = fmap user maintainers
  -- Check if any of the maintainers don't already exist
  missingUserIds <-
    lift $
      queryListCol @UserId
        [sql|
      WITH values(user_id) AS (
        SELECT * FROM ^{singleColumnTable userIds}
      )
      SELECT values.user_id FROM values
          WHERE NOT EXISTS (SELECT FROM project_maintainers pm
                             WHERE pm.project_id = #{projId}
                               AND pm.user_id = values.user_id
                           )
    |]

  case missingUserIds of
    (_ : _) -> throwError missingUserIds
    [] -> do
      let updatedMaintainersTable = maintainers <&> \Maintainer {user, permissions = ProjectMaintainerPermissions {canView, canMaintain, canAdmin}} -> (projId, user, canView, canMaintain, canAdmin)
      lift $
        execute_
          [sql|
        WITH values(project_id, user_id, can_view, can_maintain, can_admin) AS (
          SELECT * FROM ^{toTable updatedMaintainersTable}
        ) UPDATE project_maintainers
          SET can_view = v.can_view, can_maintain = v.can_maintain, can_admin = v.can_admin
            FROM values v
          WHERE project_maintainers.project_id = v.project_id
            AND project_maintainers.user_id = v.user_id
        |]
      -- Delete any maintainers that have no permissions
      lift $
        execute_
          [sql|
          DELETE FROM project_maintainers pm
            WHERE pm.project_id = #{projId}
              AND pm.can_view = false
              AND pm.can_maintain = false
              AND pm.can_admin = false
        |]
      lift $ listProjectMaintainers projId

expectProjectShortHandsOf :: Traversal s t ProjectId ProjectShortHand -> s -> Transaction e t
expectProjectShortHandsOf trav s = do
  s
    & unsafePartsOf trav %%~ \projIds -> do
      let numberedProjIds = zip [1 :: Int32 ..] projIds
      results :: [ProjectShortHand] <-
        queryListRows @(UserHandle, ProjectSlug)
          [sql|
      WITH proj_ids(ord, id) AS (
        SELECT * FROM ^{toTable numberedProjIds}
      )
      SELECT u.handle, p.slug
        FROM proj_ids JOIN projects p ON proj_ids.id = p.id
          JOIN users u ON p.owner_user_id = u.id
        ORDER BY proj_ids.ord ASC
      |]
          <&> fmap \(userHandle, projectSlug) -> ProjectShortHand {userHandle, projectSlug}

      if length results /= length projIds
        then error "expectProjectShortHandsOf: Missing expected project short hand"
        else pure results
