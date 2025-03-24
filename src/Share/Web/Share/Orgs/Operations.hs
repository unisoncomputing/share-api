module Share.Web.Share.Orgs.Operations
  ( createOrg,
  )
where

import Share.IDs (OrgHandle (..), OrgId, UserHandle (..), UserId)
import Share.Postgres
import Share.Postgres.Users.Queries (UserCreationError)
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Utils.URI
import Share.Web.Authorization.Types qualified as AuthZ
import Share.Web.Share.Roles.Queries qualified as RoleQ

createOrg :: AuthZ.AuthZReceipt -> Text -> OrgHandle -> Text -> Maybe URIParam -> UserId -> Transaction UserCreationError OrgId
createOrg !authZReceipt name (OrgHandle handle) email avatarUrl owner = do
  let emailVerified = False
  orgUserId <- UserQ.createUser authZReceipt email (Just name) avatarUrl (UserHandle handle) emailVerified
  (orgId, orgResourceId) <-
    queryExpect1Row
      [sql|
    INSERT INTO orgs (user_id) VALUES (#{orgUserId})
      RETURNING id, resource_id
    |]
  RoleQ.assignUserRoleMembership authZReceipt owner orgResourceId AuthZ.RoleOrgOwner
  pure orgId
