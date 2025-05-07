module Share.Web.Share.DisplayInfo.Queries (userLikeDisplayInfoOf) where

import Control.Lens
import Share.Postgres (Transaction)
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Orgs.Queries qualified as OrgsQ
import Share.Web.Share.Teams.Queries qualified as TeamsQ

userLikeDisplayInfoOf :: s -> Traversal s t UserLikeIds UnifiedDisplayInfo -> Transaction e t
userLikeDisplayInfoOf s trav = do
  s & unsafePartsOf trav \userLikeIds -> do
    withUsers <- userLikeIds & UsersQ.userDisplayInfoOf (traversed . unifiedUser_)
    withTeams <- withUsers & TeamsQ.teamDisplayInfoOf (traversed . unifiedTeam_)
    withOrgs <- withTeams & OrgsQ.orgDisplayInfoOf (traversed . unifiedOrg_)
    pure withOrgs
