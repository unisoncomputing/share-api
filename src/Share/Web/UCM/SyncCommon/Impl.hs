module Share.Web.UCM.SyncCommon.Impl (codebaseForBranchRef) where

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
