module Share.Web.Share.Orgs.Operations
  ( createOrg,
  )
where

import Data.Set qualified as Set
import Share.IDs (Email, OrgHandle (..), OrgId, UserHandle (..), UserId)
import Share.Postgres
import Share.Postgres.Users.Queries (UserCreationError)
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Utils.URI
import Share.Web.Authorization.Types qualified as AuthZ
import Share.Web.Share.Orgs.Queries qualified as OrgQ
import Share.Web.Share.Roles.Queries qualified as RoleQ

createOrg :: AuthZ.AuthZReceipt -> Text -> OrgHandle -> Maybe Email -> Maybe URIParam -> UserId -> UserId -> Bool -> Transaction UserCreationError OrgId
createOrg !authZReceipt name (OrgHandle handle) email avatarUrl owner creator isCommercial = do
  let emailVerified = False
  let isOrg = True
  orgUserId <- UserQ.createUser authZReceipt isOrg email (Just name) avatarUrl (UserHandle handle) emailVerified
  (orgId, orgResourceId) <-
    queryExpect1Row
      [sql|
    INSERT INTO orgs (user_id, creator_user_id, is_commercial)
      VALUES (#{orgUserId}, #{creator}, #{isCommercial})
      RETURNING id, resource_id
    |]
  RoleQ.assignUserRoleMembership authZReceipt owner orgResourceId AuthZ.RoleOrgOwner
  OrgQ.addOrgMembers orgId (Set.singleton owner)
  pure orgId
