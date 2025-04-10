{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Authorization.Queries
  ( checkIsUserMaintainer,
    isSuperadmin,
    isOrgMember,
    causalIsInHistoryOf,
    userHasProjectPermission,
    userHasOrgPermission,
    listSubjectsWithResourcePermission,
    subjectIdsForAuthSubjectsOf,
    permissionsForProject,
    permissionsForOrg,
  )
where

import Control.Lens
import Data.Int (Int32)
import Data.Set (Set)
import Data.Set qualified as Set
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs (CausalId)
import Share.Web.Authorization.Types

-- | A user has access if they own the repo, or if they're a member of an org which owns it.
checkIsUserMaintainer :: UserId -> UserId -> PG.Transaction e Bool
checkIsUserMaintainer requestingUserId codebaseOwnerUserId
  | requestingUserId == codebaseOwnerUserId = pure True
  | otherwise =
      PG.queryExpect1Col
        [PG.sql|
      SELECT EXISTS
        (SELECT org.organization_user_id FROM org_members AS org
            WHERE
              org.member_user_id = #{requestingUserId}
              AND org.organization_user_id = #{codebaseOwnerUserId}
        )
      |]

isSuperadmin :: UserId -> PG.Transaction e Bool
isSuperadmin uid = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT EXISTS (SELECT FROM superadmins s WHERE s.user_id = #{uid})
      |]

isOrgMember :: UserId -> UserId -> PG.Transaction e Bool
isOrgMember userId orgUserId = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT EXISTS (SELECT FROM org_members org
                       WHERE org.member_user_id = #{userId}
                         AND org.organization_user_id = #{orgUserId}
                       )
      |]

causalIsInHistoryOf :: CausalId -> CausalId -> PG.Transaction e Bool
causalIsInHistoryOf rootCausalId targetCausalId = do
  PG.queryExpect1Col
    [PG.sql| SELECT EXISTS (
      SELECT ch.causal_id
        FROM causal_history( #{rootCausalId} )
            AS ch(causal_id)
          WHERE ch.causal_id = ( #{targetCausalId} )
          )
      |]

--------------------------------------------------------------------------------

-- * NEW AuthZ * --

userHasProjectPermission :: Maybe UserId -> ProjectId -> RolePermission -> PG.Transaction e Bool
userHasProjectPermission mayUserId projectId permission = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT user_has_permission(#{mayUserId}, (SELECT p.resource_id from projects p WHERE p.id = #{projectId}), #{permission})
    |]

userHasOrgPermission :: UserId -> OrgId -> RolePermission -> PG.Transaction e Bool
userHasOrgPermission userId orgId permission = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT user_has_permission(#{userId}, (SELECT org.resource_id FROM orgs org WHERE org.id = #{orgId}), #{permission})
    |]

-- | Find all the subjects which have access to a given resource.
listSubjectsWithResourcePermission :: ResourceId -> RolePermission -> PG.Transaction e [AuthSubject SubjectId SubjectId SubjectId]
listSubjectsWithResourcePermission resourceId permission = do
  PG.queryListRows @(AuthSubject SubjectId SubjectId SubjectId)
    [PG.sql|
      SELECT subject.id, subject.kind
      FROM subject_resource_permissions
      WHERE resource_id = #{resourceId}
        AND permission = #{permission}
    |]

subjectIdsForAuthSubjectsOf :: Traversal s t ResolvedAuthSubject GenericAuthSubject -> s -> PG.Transaction e t
subjectIdsForAuthSubjectsOf trav s =
  s
    & unsafePartsOf trav %%~ \resolvedAuthSubjects -> PG.pipelined do
      let userIds = zip [0 :: Int32 ..] $ resolvedAuthSubjects ^.. (traversed . _UserSubject)
      let orgIds = zip [0 :: Int32 ..] $ resolvedAuthSubjects ^.. (traversed . _OrgSubject)
      let teamIds = zip [0 :: Int32 ..] $ resolvedAuthSubjects ^.. (traversed . _TeamSubject)
      userSubjects <-
        PG.queryListCol @SubjectId
          [PG.sql|
            WITH values(ord, user_id) AS (
              SELECT * FROM ^{PG.toTable userIds}
            ) SELECT user.subject_id
              FROM values
                JOIN users user ON user.id = values.user_id
                ORDER BY ord
          |]
      orgSubjects <-
        PG.queryListCol @SubjectId
          [PG.sql|
            WITH values(ord, org_id) AS (
              SELECT * FROM ^{PG.toTable orgIds}
            ) SELECT org.subject_id
              FROM values
                JOIN orgs org ON org.user_id = values.org_id
                ORDER BY ord
          |]
      teamSubjects <-
        PG.queryListCol @SubjectId
          [PG.sql|
            WITH values(ord, team_id) AS (
              SELECT * FROM ^{PG.toTable teamIds}
            ) SELECT team.subject_id
              FROM values
                JOIN teams team ON team.id = values.team_id
                ORDER BY ord
          |]
      pure $
        resolvedAuthSubjects
          & unsafePartsOf (traversed . _UserSubject) .~ userSubjects
          & unsafePartsOf (traversed . _OrgSubject) .~ orgSubjects
          & unsafePartsOf (traversed . _TeamSubject) .~ teamSubjects

permissionsForProject :: Maybe UserId -> ProjectId -> PG.Transaction e (Set RolePermission)
permissionsForProject mayUserId projectId = do
  PG.queryListCol @RolePermission
    [PG.sql|
      (SELECT permission FROM
        user_resource_permissions urp
        JOIN projects p ON p.resource_id = urp.resource_id
        WHERE (urp.user_id IS NULL OR urp.user_id = #{mayUserId})
              AND p.id = #{projectId}
        )
      |]
    <&> Set.fromList

permissionsForOrg :: Maybe UserId -> OrgId -> PG.Transaction e (Set RolePermission)
permissionsForOrg mayUserId orgId = do
  PG.queryListCol @RolePermission
    [PG.sql|
      SELECT permission
      FROM user_resource_permissions urp
      JOIN orgs org ON org.resource_id = urp.resource_id
      WHERE (urp IS NULL OR urp.user_id = #{mayUserId})
            AND org.id = #{orgId}
      |]
    <&> Set.fromList
