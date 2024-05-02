{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Admin.API where

import Enlil.IDs
import Enlil.OAuth.Session
import Enlil.Web.Admin.Types
import Servant

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
