{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Projects.Queries
  ( isPremiumProject,
    listProjectRoles,
    addProjectRoles,
    removeProjectRoles,
    expectProjectShortHandsOf,
    projectNotificationData,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Share.IDs
import Share.Notifications.Types (ProjectData (..))
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Prelude
import Share.Web.Authorization.Types (ResolvedAuthSubject, RoleAssignment (..), RoleRef (..), projectRoles, resolvedAuthSubjectColumns)

isPremiumProject :: (QueryM m) => ProjectId -> m Bool
isPremiumProject projId =
  fromMaybe False <$> do
    query1Col
      [sql|
    SELECT EXISTS (SELECT FROM premium_projects WHERE project_id = #{projId})
    |]

listProjectRoles :: (QueryM m) => ProjectId -> m [(RoleAssignment ResolvedAuthSubject)]
listProjectRoles projId = do
  queryListRows @(ResolvedAuthSubject PG.:. Only ([RoleRef]))
    [sql|
      SELECT sbk.kind, sbk.resolved_id, ARRAY_AGG(role.ref :: role_ref) as role_refs
      FROM role_memberships rm
      JOIN roles role ON rm.role_id = role.id
      JOIN subjects_by_kind sbk ON rm.subject_id = sbk.subject_id
      WHERE resource_id = (SELECT p.resource_id FROM projects p WHERE p.id = #{projId})
        AND (role.ref::role_ref) = ANY(#{toList projectRoles}::role_ref[])
        GROUP BY sbk.kind, sbk.resolved_id
        ORDER BY sbk.kind, sbk.resolved_id
    |]
    <&> fmap \(subject PG.:. Only roleRefs) -> (RoleAssignment {subject, roles = Set.fromList roleRefs})

addProjectRoles :: (QueryM m) => ProjectId -> [RoleAssignment ResolvedAuthSubject] -> m [(RoleAssignment ResolvedAuthSubject)]
addProjectRoles projId toAdd = do
  let addedRolesTable =
        toAdd
          & foldMap
            ( \RoleAssignment {subject, roles} ->
                let (kind, uuid) = resolvedAuthSubjectColumns subject
                 in (kind,uuid,) <$> toList roles
            )
  -- Insert the maintainers
  execute_
    [sql|
        WITH values(subject_id, role_id) AS (
          SELECT sbk.subject_id, role.id
            FROM ^{toTable addedRolesTable} AS t(kind, resolved_id, role_ref)
            JOIN subjects_by_kind sbk ON sbk.kind = (t.kind::subject_kind) AND sbk.resolved_id = t.resolved_id
            JOIN roles role ON role.ref = (t.role_ref::role_ref)
        ) INSERT INTO role_memberships (subject_id, resource_id, role_id)
          SELECT v.subject_id, (SELECT p.resource_id FROM projects p WHERE p.id = #{projId}) AS resource_id, v.role_id
            FROM values v
          ON CONFLICT DO NOTHING
        |]
  listProjectRoles projId

removeProjectRoles :: (QueryM m) => ProjectId -> [RoleAssignment ResolvedAuthSubject] -> m [(RoleAssignment ResolvedAuthSubject)]
removeProjectRoles projId toRemove = do
  let removedRolesTable =
        toRemove
          & foldMap
            ( \RoleAssignment {subject, roles} ->
                let (kind, uuid) = resolvedAuthSubjectColumns subject
                 in (kind,uuid,) <$> toList roles
            )
  -- Insert the maintainers
  execute_
    [sql|
        WITH values(subject_id, role_id) AS (
          SELECT sbk.subject_id, role.id
            FROM ^{toTable removedRolesTable} AS t(kind, resolved_id, role_ref)
            JOIN subjects_by_kind sbk ON sbk.kind = (t.kind::subject_kind) AND sbk.resolved_id = t.resolved_id
            JOIN roles role ON role.ref = (t.role_ref::role_ref)
        ) DELETE FROM role_memberships
          USING values v
          WHERE role_memberships.subject_id = v.subject_id
            AND role_memberships.resource_id = (SELECT p.resource_id FROM projects p WHERE p.id = #{projId})
            AND role_memberships.role_id = v.role_id
        |]
  listProjectRoles projId

expectProjectShortHandsOf :: (QueryM m) => Traversal s t ProjectId ProjectShortHand -> s -> m t
expectProjectShortHandsOf trav s = do
  s
    & asListOf trav %%~ \projIds -> do
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

-- | Info used when constructing a notification within a project.
projectNotificationData :: (QueryA m) => ProjectId -> m (ProjectData, ResourceId, UserId)
projectNotificationData projectId = do
  PG.queryExpect1Row
    [PG.sql|
      SELECT p.resource_id, p.owner_user_id, p.private
      FROM projects p
      WHERE p.id = #{projectId}
    |]
    <&> \(resourceId, ownerUserId, isPrivate) ->
      ( ProjectData
          { projectId,
            public = not isPrivate
          },
        resourceId,
        ownerUserId
      )
