module Share.Web.Share.Roles
  ( canonicalRoleAssignmentOrdering,
  )
where

import Data.List qualified as List
import Share.IDs qualified as IDs
import Share.Web.Authorization.Types
import Share.Web.Share.DisplayInfo (OrgDisplayInfo (..), TeamDisplayInfo (..), UserDisplayInfo (..))

-- | The ordering isn't necessary logic, it just makes transcript tests much easier.
canonicalRoleAssignmentOrdering :: [RoleAssignment DisplayAuthSubject] -> [RoleAssignment DisplayAuthSubject]
canonicalRoleAssignmentOrdering =
  List.sortOn \(RoleAssignment {roles, subject}) ->
    case subject of
      UserSubject (UserDisplayInfo {handle}) -> (0 :: Int, IDs.toText handle, roles)
      OrgSubject (OrgDisplayInfo {user = UserDisplayInfo {handle}}) -> (1, IDs.toText handle, roles)
      TeamSubject (TeamDisplayInfo {name}) -> (2, name, roles)
