module Share.Web.UCM.SyncCommon.Types () where

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON) via Text

instance From (ProjectAndBranch ProjectName ProjectBranchName) BranchRef where
  from pab = BranchRef $ from pab


data CodebaseLoadingError
  = CodebaseLoadingErrorProjectNotFound ProjectShortHand
  | CodebaseLoadingErrorUserNotFound UserHandle
  | CodebaseLoadingErrorNoReadPermission SyncV2.BranchRef
  | CodebaseLoadingErrorInvalidBranchRef Text SyncV2.BranchRef
  deriving stock (Show)
  deriving (Logging.Loggable) via Logging.ShowLoggable Logging.UserFault CodebaseLoadingError

instance ToServerError CodebaseLoadingError where
  toServerError = \case
    CodebaseLoadingErrorProjectNotFound projectShortHand -> (ErrorID "codebase-loading:project-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "Project not found: " <> (IDs.toText projectShortHand)})
    CodebaseLoadingErrorUserNotFound userHandle -> (ErrorID "codebase-loading:user-not-found", Servant.err404 {errBody = from . Text.encodeUtf8 $ "User not found: " <> (IDs.toText userHandle)})
    CodebaseLoadingErrorNoReadPermission branchRef -> (ErrorID "codebase-loading:no-read-permission", Servant.err403 {errBody = from . Text.encodeUtf8 $ "No read permission for branch ref: " <> (SyncV2.unBranchRef branchRef)})
    CodebaseLoadingErrorInvalidBranchRef err branchRef -> (ErrorID "codebase-loading:invalid-branch-ref", Servant.err400 {errBody = from . Text.encodeUtf8 $ "Invalid branch ref: " <> err <> " " <> (SyncV2.unBranchRef branchRef)})

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
