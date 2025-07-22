module Share.Web.Share.DisplayInfo.Queries (userLikeDisplayInfoOf, unifiedDisplayInfoForUserOf) where

import Control.Lens
import Share.IDs
import Share.Postgres (QueryM)
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Orgs.Queries qualified as OrgsQ

userLikeDisplayInfoOf :: (QueryM m) => Traversal s t UserLikeIds UnifiedDisplayInfo -> s -> m t
userLikeDisplayInfoOf trav s = do
  s & asListOf trav \userLikeIds -> do
    withUsers <- userLikeIds & UsersQ.userDisplayInfoOf (traversed . unifiedUser_)
    withOrgs <- withUsers & OrgsQ.orgDisplayInfoOf (traversed . unifiedOrg_)
    pure withOrgs

unifiedDisplayInfoForUserOf :: (QueryM m) => Traversal s t UserId UnifiedDisplayInfo -> s -> m t
unifiedDisplayInfoForUserOf trav s = do
  s & asListOf trav \userLikeIds -> do
    userLikes <-
      UserQ.joinOrgIdsToUserIdsOf traversed userLikeIds
        <&> fmap \case
          (_userId, Just orgId) -> UnifiedOrg orgId
          (userId, Nothing) -> UnifiedUser userId
    userLikes & userLikeDisplayInfoOf traversed
