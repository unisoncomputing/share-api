{-# LANGUAGE TypeOperators #-}

module Enlil.Postgres.Projects.Queries
  ( isPremiumProject,
    listProjectMaintainers,
    addMaintainers,
    updateMaintainers,
  )
where

import Control.Lens
import Control.Monad.Except (MonadError (..), runExceptT)
import Enlil.IDs
import Enlil.Postgres qualified as PG
import Enlil.Prelude
import Enlil.Web.Authorization.Types (ProjectMaintainerPermissions (..))
import Enlil.Web.Share.Projects.Types (Maintainer (..))

isPremiumProject :: ProjectId -> PG.Transaction e Bool
isPremiumProject projId =
  fromMaybe False <$> do
    PG.query1Col
      [PG.sql|
    SELECT EXISTS (SELECT FROM premium_projects WHERE project_id = #{projId})
    |]

listProjectMaintainers :: ProjectId -> PG.Transaction e [Maintainer UserId]
listProjectMaintainers projId = do
  results <-
    PG.queryListRows @(UserId, Bool, Bool, Bool)
      [PG.sql|
    SELECT pm.user_id, pm.can_view, pm.can_maintain, pm.can_admin
        FROM project_maintainers pm
          WHERE pm.project_id = #{projId}
    |]
  results
    & fmap \case
      (userId, canView, canMaintain, canAdmin) ->
        let permissions = ProjectMaintainerPermissions {canView, canMaintain, canAdmin}
         in Maintainer {user = userId, permissions}
    & pure

addMaintainers :: ProjectId -> [Maintainer UserId] -> PG.Transaction e (Either [UserId] [Maintainer UserId])
addMaintainers projId maintainers = runExceptT $ do
  let userIds = fmap user maintainers
  -- Check if any of the maintainers already exist
  alreadyExistingUserIds <-
    lift $
      PG.queryListCol @UserId
        [PG.sql|
      WITH values(user_id) AS (
        SELECT * FROM ^{PG.singleColumnTable userIds}
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
        PG.execute_
          [PG.sql|
        WITH values(project_id, user_id, can_view, can_maintain, can_admin) AS (
          SELECT * FROM ^{PG.toTable newMaintainersTable}
        ) INSERT INTO project_maintainers (project_id, user_id, can_view, can_maintain, can_admin)
          SELECT v.project_id, v.user_id, v.can_view, v.can_maintain, v.can_admin
            FROM values v
        |]
      lift $ listProjectMaintainers projId

updateMaintainers :: ProjectId -> [Maintainer UserId] -> PG.Transaction e (Either [UserId] [Maintainer UserId])
updateMaintainers projId maintainers = runExceptT $ do
  let userIds = fmap user maintainers
  -- Check if any of the maintainers don't already exist
  missingUserIds <-
    lift $
      PG.queryListCol @UserId
        [PG.sql|
      WITH values(user_id) AS (
        SELECT * FROM ^{PG.singleColumnTable userIds}
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
        PG.execute_
          [PG.sql|
        WITH values(project_id, user_id, can_view, can_maintain, can_admin) AS (
          SELECT * FROM ^{PG.toTable updatedMaintainersTable}
        ) UPDATE project_maintainers
          SET can_view = v.can_view, can_maintain = v.can_maintain, can_admin = v.can_admin
            FROM values v
          WHERE project_maintainers.project_id = v.project_id
            AND project_maintainers.user_id = v.user_id
        |]
      -- Delete any maintainers that have no permissions
      lift $
        PG.execute_
          [PG.sql|
          DELETE FROM project_maintainers pm
            WHERE pm.project_id = #{projId}
              AND pm.can_view = false
              AND pm.can_maintain = false
              AND pm.can_admin = false
        |]
      lift $ listProjectMaintainers projId
