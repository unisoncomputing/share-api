module Share.Web.Share.DisplayInfo.Queries (userLikeDisplayInfoOf, unifiedDisplayInfoForUserOf) where

import Control.Lens
import Share.IDs
import Share.Postgres (Transaction)
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Orgs.Queries qualified as OrgsQ
import Share.Web.Share.Teams.Queries qualified as TeamsQ

userLikeDisplayInfoOf :: Traversal s t UserLikeIds UnifiedDisplayInfo -> s -> Transaction e t
userLikeDisplayInfoOf trav s = do
  s & unsafePartsOf trav \userLikeIds -> do
    withUsers <- userLikeIds & UsersQ.userDisplayInfoOf (traversed . unifiedUser_)
    withTeams <- withUsers & TeamsQ.teamDisplayInfoOf (traversed . unifiedTeam_)
    withOrgs <- withTeams & OrgsQ.orgDisplayInfoOf (traversed . unifiedOrg_)
    pure withOrgs

unifiedDisplayInfoForUserOf :: Traversal s t UserId UnifiedDisplayInfo -> s -> Transaction e t
unifiedDisplayInfoForUserOf trav s = do
  s & unsafePartsOf trav \userLikeIds -> do
    userLikes <-
      UserQ.joinOrgIdsToUserIdsOf traversed userLikeIds
        <&> fmap \case
          (_userId, Just orgId) -> UnifiedOrg orgId
          (userId, Nothing) -> UnifiedUser userId
    userLikes & userLikeDisplayInfoOf traversed
