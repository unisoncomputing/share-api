{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.Queries
  ( orgByUserId,
    orgByUserHandle,
    listOrgRoles,
    addOrgRoles,
    removeOrgRoles,
    orgDisplayInfoOf,
    userDisplayInfoByOrgIdOf,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Share.IDs (OrgId, UserHandle, UserId)
import Share.Postgres
import Share.Prelude
import Share.Utils.URI
import Share.Web.Authorization.Types
import Share.Web.Share.Orgs.Types (Org)
import Share.Web.Share.Types (OrgDisplayInfo (..), UserDisplayInfo (..))

orgByUserId :: UserId -> Transaction e (Maybe Org)
orgByUserId orgUserId = do
  query1Row
    [sql|
      SELECT org.id FROM orgs org
      WHERE org.user_id = #{orgUserId}
    |]

orgByUserHandle :: UserHandle -> Transaction e (Maybe Org)
orgByUserHandle orgHandle = do
  query1Row
    [sql|
      SELECT org.id
        FROM orgs org
        JOIN users u ON org.user_id = u.id
      WHERE u.handle = #{orgHandle}
    |]

-- | Efficiently resolve Org Display Info for OrgIds within a structure.
orgDisplayInfoOf :: (QueryA m) => Traversal s t OrgId OrgDisplayInfo -> s -> m t
orgDisplayInfoOf trav s = do
  s
    & unsafePartsOf trav %%~ \orgIds -> do
      userDisplayInfos <- userDisplayInfoByOrgIdOf traversed orgIds
      pure $ zipWith (\orgId userDisplayInfo -> OrgDisplayInfo {orgId, user = userDisplayInfo}) orgIds userDisplayInfos

userDisplayInfoByOrgIdOf :: (QueryA m) => Traversal s t OrgId UserDisplayInfo -> s -> m t
userDisplayInfoByOrgIdOf trav s = do
  s
    & unsafePartsOf trav %%~ \orgIds ->
      do
        let orgTable = zip [0 :: Int32 ..] orgIds
        queryListRows
          [sql|
      WITH values(ord, org_id) AS (
        SELECT * FROM ^{toTable orgTable} AS t(ord, org_id)
      ) SELECT u.handle, u.name, u.avatar_url, u.id
          FROM values
          JOIN orgs org ON org.id = values.org_id
          JOIN users u ON u.id = org.user_id
        ORDER BY ord
        |]
          <&> fmap \(handle, name, avatarUrl, userId) ->
            UserDisplayInfo
              { handle,
                name,
                avatarUrl = unpackURI <$> avatarUrl,
                userId
              }

listOrgRoles :: OrgId -> Transaction e [RoleAssignment ResolvedAuthSubject]
listOrgRoles orgId = do
  queryListRows @(ResolvedAuthSubject :. Only [RoleRef])
    [sql|
      SELECT sbk.kind, sbk.resolved_id, array_agg(role.ref :: role_ref) as role_refs
      FROM role_memberships rm
      JOIN roles role ON rm.role_id = role.id
      JOIN subjects_by_kind sbk ON rm.subject_id = sbk.subject_id
      WHERE resource_id = (SELECT org.resource_id FROM orgs org WHERE org.id = #{orgId})
        GROUP BY sbk.kind, sbk.resolved_id
        ORDER BY sbk.kind, sbk.resolved_id
    |]
    <&> fmap \(subject :. Only roleRefs) -> RoleAssignment {subject, roles = Set.fromList roleRefs}

addOrgRoles :: OrgId -> [RoleAssignment ResolvedAuthSubject] -> Transaction e [RoleAssignment ResolvedAuthSubject]
addOrgRoles orgId newRoles = do
  let newRolesTable =
        newRoles
          & foldMap
            ( \RoleAssignment {subject, roles} ->
                let (kind, uuid) = resolvedAuthSubjectColumns subject
                 in (kind,uuid,) <$> toList roles
            )
  execute_
    [sql|
        WITH values(subject_id, role_id) AS (
          SELECT sbk.subject_id, role.id
            FROM ^{toTable newRolesTable} AS t(kind, resolved_id, role_ref)
            JOIN subjects_by_kind sbk ON sbk.kind = (t.kind::subject_kind) AND sbk.resolved_id = t.resolved_id
            JOIN roles role ON role.ref = (t.role_ref::role_ref)
        ) INSERT INTO role_memberships (subject_id, resource_id, role_id)
          SELECT v.subject_id, (SELECT org.resource_id FROM orgs org WHERE org.id = #{orgId}) AS resource_id, v.role_id
            FROM values v
          ON CONFLICT DO NOTHING
        |]
  listOrgRoles orgId

removeOrgRoles :: OrgId -> [RoleAssignment ResolvedAuthSubject] -> Transaction e [RoleAssignment ResolvedAuthSubject]
removeOrgRoles orgId toRemove = do
  let removedRolesTable =
        toRemove
          & foldMap
            ( \RoleAssignment {subject, roles} ->
                let (kind, uuid) = resolvedAuthSubjectColumns subject
                 in (kind,uuid,) <$> toList roles
            )
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
            AND role_memberships.resource_id = (SELECT org.resource_id FROM orgs org WHERE org.id = #{orgId})
            AND role_memberships.role_id = v.role_id
        |]
  listOrgRoles orgId
