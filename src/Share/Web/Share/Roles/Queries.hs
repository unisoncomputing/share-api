{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Roles.Queries
  ( displaySubjectsOf,
    assignUserRoleMembership,
    assignRoleMembership,
  )
where

import Share.Prelude
import Control.Lens
import Share.IDs
import Share.Postgres
import Share.Postgres.Users.Queries (userDisplayInfoOf)
import Share.Web.Authorization.Types (AuthZReceipt, DisplayAuthSubject, ResolvedAuthSubject, RoleRef, _OrgSubject, _TeamSubject, _UserSubject)
import Share.Web.Share.Orgs.Queries (orgDisplayInfoOf)
import Share.Web.Share.Teams.Queries (teamDisplayInfoOf)

displaySubjectsOf :: (Traversal s t ResolvedAuthSubject DisplayAuthSubject) -> s -> Transaction e t
displaySubjectsOf trav s = pipelined do
  s & asListOf trav \authSubjectList -> do
    let userSubjects = (authSubjectList ^. asListOf (traversed . _UserSubject))
    newUsers <- userDisplayInfoOf traversed userSubjects
    let teamSubjects = (authSubjectList ^. asListOf (traversed . _TeamSubject))
    newTeams <- teamDisplayInfoOf traversed teamSubjects
    let orgSubjects = (authSubjectList ^. asListOf (traversed . _OrgSubject))
    newOrgs <- orgDisplayInfoOf traversed orgSubjects
    pure
      ( authSubjectList
          & asListOf (traversed . _UserSubject) .~ newUsers
          & asListOf (traversed . _TeamSubject) .~ newTeams
          & asListOf (traversed . _OrgSubject) .~ newOrgs
      )

-- | Add a role to a user for a specific resource.
assignUserRoleMembership :: AuthZReceipt -> UserId -> ResourceId -> RoleRef -> Transaction e ()
assignUserRoleMembership authZReceipt userId resourceId roleRef = do
  subjectId <- queryExpect1Col [sql| SELECT u.subject_id FROM users u WHERE u.id = #{userId} LIMIT 1|]
  assignRoleMembership authZReceipt subjectId resourceId roleRef

-- | Add a role to a subject for a specific resource.
assignRoleMembership :: AuthZReceipt -> SubjectId -> ResourceId -> RoleRef -> Transaction e ()
assignRoleMembership !_authZReceipt subjectId resourceId roleRef = do
  execute_
    [sql|
    INSERT INTO role_memberships (subject_id, resource_id, role_id)
      SELECT #{subjectId}, #{resourceId}, r.id
      FROM roles r
      WHERE r.ref = #{roleRef}::role_ref
    |]
