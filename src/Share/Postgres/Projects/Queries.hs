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
import Data.Set qualified as Set
import Share.IDs
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.Authorization.Types (GenericAuthSubject, ProjectMaintainerPermissions (..), ResolvedAuthSubject, RoleAssignment (..), RoleRef (..), projectRoles)

isPremiumProject :: ProjectId -> Transaction e Bool
isPremiumProject projId =
  fromMaybe False <$> do
    query1Col
      [sql|
    SELECT EXISTS (SELECT FROM premium_projects WHERE project_id = #{projId})
    |]

listProjectMaintainers :: ProjectId -> Transaction e [(ResolvedAuthSubject, Set RoleRef)]
listProjectMaintainers projId = do
  queryListRows @(ResolvedAuthSubject PG.:. Only ([RoleRef]))
    [sql|
      SELECT sbk.kind, sbk.resolved_id, array_agg(role.ref) as role_refs
      FROM role_memberships rm
      JOIN roles role ON rm.role_id = role.id
      JOIN subjects_by_kind sbk ON rm.subject_id = sbk.subject_id
      WHERE resource_id = (SELECT p.resource_id FROM projects p WHERE p.id = #{projId})
        AND role.ref = ANY(#{toList projectRoles})
        GROUP BY sbk.kind, sbk.resolved_id
        ORDER BY sbk.kind, sbk.resolved_id
    |]
    <&> over _2 Set.fromList

addMaintainers :: ProjectId -> [RoleAssignment SubjectId] -> Transaction e [(ResolvedAuthSubject, Set RoleRef)]
addMaintainers projId toAdd = do
  let addedRolesTable = toAdd <&> \RoleAssignment {subject, roles} -> (subject, roles)
  -- Insert the maintainers
  execute_
    [sql|
        WITH values(subject_id, role_refs) AS (
          SELECT t.subject_id, t.role_refs FROM ^{toTable addedRolesTable} AS t(project_id, user_id, role_refs)
        ) INSERT INTO role_memberships (subject_id, resource_id, role_id)
          SELECT values.subject_id, (SELECT p.resource_id FROM projects p WHERE p.id = #{projId}), r.id
            FROM values, UNNEST(values.role_refs) AS role_ref
            JOIN roles r ON r.ref = role_ref
          ON CONFLICT DO NOTHING
        |]
  listProjectMaintainers projId

updateMaintainers :: ProjectId -> [RoleAssignment UserId] -> Transaction e (Either [UserId] [RoleAssignment UserId])
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
      let updatedMaintainersTable = maintainers <&> \RoleAssignment {subject, roles = ProjectMaintainerPermissions {canView, canMaintain, canAdmin}} -> (projId, subject, canView, canMaintain, canAdmin)
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
