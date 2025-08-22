{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Share.Client.Users
  (
  )
where

import Crypto.JWT qualified as JWT
import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Client
import Share.Client.Utils (maybeJwtToAuthenticatedRequest)
import Share.IDs (UserHandle)
import Share.Web.API (UsersAPI)
import Share.Web.Share.Types

usersClient :: Client ClientM UsersAPI
usersClient = client (Proxy :: Proxy UsersAPI)

getUserProfile :: Maybe JWT.SignedJWT -> UserHandle -> ClientM DescribeUserProfile
getUserProfile = \jwt handle ->
  let ( _ :<|> userProfileEndpoint
          :<|> _
          :<|> _
          :<|> _
          :<|> _
          :<|> _
        ) = usersClient (maybeJwtToAuthenticatedRequest jwt) handle
   in userProfileEndpoint
