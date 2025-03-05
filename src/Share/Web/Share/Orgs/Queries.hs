{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Orgs.Queries
  ( orgByUserId,
    orgByUserHandle,
    listOrgRoles,
    addOrgRoles,
    removeOrgRoles,
  )
where

import Data.Set qualified as Set
import Share.IDs (OrgId, UserHandle, UserId)
import Share.Postgres
import Share.Prelude
import Share.Web.Authorization.Types
import Share.Web.Share.Orgs.Types (Org)

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
