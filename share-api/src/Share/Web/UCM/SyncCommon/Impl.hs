module Share.Web.UCM.SyncCommon.Impl
  ( parseBranchRef,
    codebaseForBranchRef,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Servant
import Share.Codebase qualified as Codebase
import Share.IDs (ProjectBranchShortHand (..), ProjectReleaseShortHand (..), ProjectShortHand (..))
import Share.IDs qualified as IDs
import Share.Postgres qualified as PG
import Share.Postgres.Queries qualified as PGQ
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Prelude
import Share.Project (Project (..))
import Share.User (User (..))
import Share.Web.App
import Share.Web.Authorization qualified as AuthZ
import Share.Web.UCM.SyncCommon.Types
import U.Codebase.Sqlite.Orphans ()
import Unison.SyncV2.Types qualified as SyncV2

parseBranchRef :: SyncV2.BranchRef -> Either Text (Either ProjectReleaseShortHand ProjectBranchShortHand)
parseBranchRef (SyncV2.BranchRef branchRef) =
  case parseRelease <|> parseBranch of
    Just a -> Right a
    Nothing -> Left $ "Invalid repo info: " <> branchRef
  where
    parseBranch :: Maybe (Either ProjectReleaseShortHand ProjectBranchShortHand)
    parseBranch = fmap Right . eitherToMaybe $ IDs.fromText @ProjectBranchShortHand branchRef
    parseRelease :: Maybe (Either ProjectReleaseShortHand ProjectBranchShortHand)
    parseRelease = fmap Left . eitherToMaybe $ IDs.fromText @ProjectReleaseShortHand branchRef

codebaseForBranchRef :: SyncV2.BranchRef -> (ExceptT CodebaseLoadingError WebApp Codebase.CodebaseEnv)
codebaseForBranchRef branchRef = do
  case parseBranchRef branchRef of
    Left err -> throwError (CodebaseLoadingErrorInvalidBranchRef err branchRef)
    Right (Left (ProjectReleaseShortHand {userHandle, projectSlug})) -> do
      let projectShortHand = ProjectShortHand {userHandle, projectSlug}
      (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
        project <- PGQ.projectByShortHand projectShortHand `whenNothingM` throwError (CodebaseLoadingErrorProjectNotFound $ projectShortHand)
        pure (project, Nothing)
      authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (CodebaseLoadingErrorNoReadPermission branchRef)
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
      pure $ Codebase.codebaseEnv authZToken codebaseLoc
    Right (Right (ProjectBranchShortHand {userHandle, projectSlug, contributorHandle})) -> do
      let projectShortHand = ProjectShortHand {userHandle, projectSlug}
      (Project {ownerUserId = projectOwnerUserId}, contributorId) <- ExceptT . PG.tryRunTransaction $ do
        project <- (PGQ.projectByShortHand projectShortHand) `whenNothingM` throwError (CodebaseLoadingErrorProjectNotFound projectShortHand)
        mayContributorUserId <- for contributorHandle \ch -> fmap user_id $ (UserQ.userByHandle ch) `whenNothingM` throwError (CodebaseLoadingErrorUserNotFound ch)
        pure (project, mayContributorUserId)
      authZToken <- lift AuthZ.checkDownloadFromProjectBranchCodebase `whenLeftM` \_err -> throwError (CodebaseLoadingErrorNoReadPermission branchRef)
      let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
      pure $ Codebase.codebaseEnv authZToken codebaseLoc
