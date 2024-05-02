{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Admin.Impl where

import Data.Either (partitionEithers)
import Data.Time qualified as Time
import Enlil.IDs
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Admin qualified as Admin
import Enlil.Postgres.Ops qualified as PGO
import Enlil.Postgres.Queries qualified as Q
import Enlil.Prelude
import Enlil.User (User (..))
import Enlil.Web.Admin.API qualified as Admin
import Enlil.Web.Admin.Types
import Enlil.Web.App
import Enlil.Web.Authorization qualified as AuthZ
import Enlil.Web.Errors
import Servant
import Unison.Util.Monoid qualified as Monoid

-- | Ensure we have name lookups for views for this user.
deleteUserEndpoint :: AuthZ.AuthZReceipt -> UserHandle -> DeleteUserRequest -> WebApp ()
deleteUserEndpoint !_authzReceipt userHandle DeleteUserRequest {currentDate} = do
  today <- Time.utctDay <$> liftIO Time.getCurrentTime
  if (currentDate /= today)
    then respondError (BadRequest "You must provide the current date in the format YYYY-MM-DD to confirm you want to delete this user.")
    else do
      User {user_id} <- PGO.expectUserByHandle userHandle
      PG.runTransaction $ Admin.hardDeleteUser user_id

addToCatalogCategoryEndpoint :: AuthZ.AuthZReceipt -> [ProjectCategory] -> WebApp NoContent
addToCatalogCategoryEndpoint !_authzReceipt additions = do
  missingProjects <- PG.runTransaction $ do
    forMaybe additions \(pc@(ProjectCategory {categoryName, projectSlug, userHandle})) -> do
      mayProjectID <- Q.projectIDFromHandleAndSlug userHandle projectSlug
      case mayProjectID of
        Nothing -> pure (Just pc)
        Just projectId -> do
          categoryId <- Q.getOrCreateCatalogCategory categoryName
          Q.addProjectToCatalogCategory projectId categoryId
          pure Nothing
  when (not . null $ missingProjects) $ do
    respondError (EntityMissing (ErrorID "project:missing") $ "The following projects could not be found:" <> tShow missingProjects)
  pure NoContent

removeFromCatalogCategoryEndpoint :: AuthZ.AuthZReceipt -> [ProjectCategory] -> WebApp NoContent
removeFromCatalogCategoryEndpoint !_authzReceipt removals = do
  missingThings :: [Either ProjectCategory CategoryName] <- PG.runTransaction $ do
    forMaybe removals \(pc@(ProjectCategory {categoryName, projectSlug, userHandle})) -> do
      mayProjectID <- Q.projectIDFromHandleAndSlug userHandle projectSlug
      case mayProjectID of
        Nothing -> pure (Just (Left pc))
        Just projectId -> do
          Q.getCatalogCategory categoryName >>= \case
            Nothing -> pure (Just (Right categoryName))
            Just categoryId -> do
              Q.removeProjectFromCatalogCategory projectId categoryId
              pure Nothing
  when (not . null $ missingThings) $ do
    let (missingProjects, missingCategories) = partitionEithers missingThings
    let msg =
          (Monoid.whenM (not . null $ missingProjects) $ "The following projects could not be found:" <> tShow missingProjects)
            <> (Monoid.whenM (not . null $ missingCategories) $ "The following categories could not be found:" <> tShow missingCategories)
    respondError (EntityMissing (ErrorID "catalog-categories:missing") msg)
  pure NoContent

server :: ServerT Admin.API WebApp
server authedSession =
  let catalogServer authzReceipt = addToCatalogCategoryEndpoint authzReceipt :<|> removeFromCatalogCategoryEndpoint authzReceipt
   in hoistServer (Proxy @Admin.UnauthenticatedAPI) requireAdmin $
        catalogServer authzReceipt
          :<|> ( \userHandle -> deleteUserEndpoint authzReceipt userHandle
               )
  where
    -- Require that the user has re-authenticated within the last 2 hours in order to
    -- perform admin actions.
    requireAdmin m = do
      AuthZ.permissionGuard $ AuthZ.checkAdminSudo authedSession
      m
    authzReceipt = AuthZ.adminOverride
