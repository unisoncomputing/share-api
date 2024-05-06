{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Authorization.Queries where

import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import Share.Web.Authorization.Types (ProjectMaintainerPermissions (..))

-- | Check if the given user has access to a provided project.
-- If yes, return the UserId of the owner of that project.
--
-- A user has access if they own the project, if they're a member of an org which owns it,
-- or (if they're in the list of project maintainers AND (the project is public OR the owner pays a subscription).
getUserProjectPermissions :: UserId -> ProjectId -> PG.Transaction e ProjectMaintainerPermissions
getUserProjectPermissions requestingUserId projectID = do
  PG.query1Row @(Bool, Bool, Bool)
    [PG.sql|
    -- Project owner can admin
    (SELECT true AS can_view, true AS can_maintain, true AS can_admin
        FROM projects
        WHERE projects.owner_user_id = #{requestingUserId} AND projects.id = #{projectID})
      UNION
    -- Members of an org which owns the project can admin.
    (SELECT true AS can_view, true AS can_maintain, true AS can_admin
      FROM org_members AS org
        JOIN projects
          ON org.organization_user_id = projects.owner_user_id
        WHERE
          org.member_user_id = #{requestingUserId}
          AND projects.id = #{projectID}
    )
      UNION
    -- Project maintainers each have their own granular permissions
    ( SELECT pm.can_view, pm.can_maintain, pm.can_admin
        FROM project_maintainers pm
        WHERE pm.project_id = #{projectID}
          AND pm.user_id = #{requestingUserId}
          -- Project maintainers are a premium feature
          AND EXISTS (SELECT FROM premium_projects
                            WHERE premium_projects.project_id = pm.project_id
                     )
    )
    LIMIT 1
      |]
    <&> \case
      Just (canView, canMaintain, canAdmin) ->
        ProjectMaintainerPermissions
          { canView,
            canMaintain,
            canAdmin
          }
      Nothing -> ProjectMaintainerPermissions False False False

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
