{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Admin.API where

import Servant
import Share.IDs
import Share.OAuth.Session
import Share.Web.Admin.Types

type API =
  AuthenticatedSession :> UnauthenticatedAPI

-- | The underlying API after authentication has taken place.
type UnauthenticatedAPI =
  ("catalog" :> "category" :> (AddToCatalogCategoryEndpoint :<|> RemoveFromCatalogCategoryEndpoint))
    :<|> ( "users"
             :> Capture "user_handle" UserHandle
             :> ( ("delete-user" :> DeleteUserEndpoint)
                )
         )
    :<|> ("orgs" :> "create" :> AdminCreateOrgEndpoint)

type CreateMissingLooseCodeMappingsEndpoint =
  Post '[JSON] ()

type UpdateAllNameLookupsEndpoint =
  Post '[JSON] ()

type UpdateUserNameLookupsEndpoint =
  Post '[JSON] ()

type DeleteUnusedNameLookupsForAllUsers = Post '[JSON] ()

type DeleteUnusedNameLookupsForUser = Post '[JSON] ()

type DeleteUserEndpoint =
  ReqBody '[JSON] DeleteUserRequest
    :> Delete '[JSON] ()

type AddToCatalogCategoryEndpoint =
  ReqBody '[JSON] [ProjectCategory]
    :> Post '[JSON] NoContent

type RemoveFromCatalogCategoryEndpoint =
  ReqBody '[JSON] [ProjectCategory]
    :> Delete '[JSON] NoContent

type AddCloudUserEndpoint =
  ReqBody '[JSON] AddCloudUserRequest
    :> Post '[JSON] ()

type RemoveCloudUserEndpoint =
  ReqBody '[JSON] RemoveCloudUserRequest
    :> Delete '[JSON] ()

type AdminCreateOrgEndpoint =
  ReqBody '[JSON] AdminCreateOrgRequest
    :> Post '[JSON] AdminCreateOrgResponse
