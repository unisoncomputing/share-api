{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Authorization.Queries
  ( checkIsUserMaintainer,
    isUnisonEmployee,
    isOrgMember,
    causalIsInHistoryOf,
    userHasProjectPermission,
    userHasOrgPermission,
    listSubjectsWithResourcePermission,
  )
where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs (CausalId)
import Share.Web.Authorization.Types (AuthSubject, RolePermission)

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

isUnisonEmployee :: UserId -> PG.Transaction e Bool
isUnisonEmployee uid = do
  PG.queryExpect1Col
    [PG.sql|
        SELECT EXISTS (SELECT FROM org_members org
                      JOIN users AS org_user ON org.organization_user_id = org_user.id
                      WHERE org.member_user_id = #{uid}
                            AND org_user.handle = 'unison')
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

userHasProjectPermission :: UserId -> ProjectId -> RolePermission -> PG.Transaction e Bool
userHasProjectPermission userId projectId permission = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT user_has_permission(#{userId}, (SELECT p.resource_id from projects p WHERE p.id = #{projectId}), #{permission})
    |]

userHasOrgPermission :: UserId -> UserId -> RolePermission -> PG.Transaction e Bool
userHasOrgPermission userId orgUserId permission = do
  PG.queryExpect1Col
    [PG.sql|
      SELECT user_has_permission(#{userId}, (SELECT org.resource_id FROM orgs WHERE org.user_id = #{orgUserId}), #{permission})
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
