{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Share.Web.Share.Branches.Impl where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Servant
import Share.Branch (Branch (..), branchCausals_)
import Share.Codebase qualified as Codebase
import Share.Codebase.CodeCache qualified as CodeCache
import Share.Codebase.CodebaseRuntime qualified as CR
import Share.Env.Types qualified as Env
import Share.IDs (BranchId, BranchShortHand (..), ProjectBranchShortHand (..), ProjectShortHand (..), ProjectSlug (..), UserHandle, UserId)
import Share.IDs qualified as IDs
import Share.OAuth.Session
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries (CausalHistoryCursor)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionsQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.NameLookups.Types (pathToPathSegments)
import Share.Postgres.NamesPerspective.Ops qualified as NP
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Project (Project (..))
import Share.Project qualified as Project
import Share.User (User (..))
import Share.Utils.API
import Share.Utils.Caching
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.Branches.API (ListBranchesCursor)
import Share.Web.Share.Branches.API qualified as API
import Share.Web.Share.Branches.Types (BranchHistoryCausal (..), BranchHistoryEntry (..), BranchHistoryResponse (..), BranchKindFilter (..), ShareBranch (..))
import Share.Web.Share.Branches.Types qualified as API
import Share.Web.Share.CodeBrowsing.API qualified as API
import Share.Web.Share.Contributions.Types
import Share.Web.Share.DisplayInfo.Types
import Share.Web.Share.Projects.Types (projectToAPI)
import Share.Web.Share.Types
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Doc.Markdown.Render qualified as MD
import Unison.Server.Doc.Markdown.Types qualified as MD
import Unison.Server.Share.DefinitionSummary (serveTermSummary, serveTypeSummary)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary, TypeSummary)
import Unison.Server.Share.Definitions qualified as ShareBackend
import Unison.Server.Share.FuzzyFind qualified as Fuzzy
import Unison.Server.Share.NamespaceDetails qualified as ND
import Unison.Server.Share.NamespaceListing (NamespaceListing (..))
import Unison.Server.Share.NamespaceListing qualified as NL
import Unison.Server.Share.RenderDoc (findAndRenderDoc)
import Unison.Server.Types (DefinitionDisplayResults, NamespaceDetails (..), Suffixify (..))
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty qualified as Pretty

getProjectBranch ::
  ProjectBranchShortHand ->
  WebApp (Project, Branch CausalId)
getProjectBranch projectBranchShortHand = do
  onNothingM missingBranch . PG.runTransaction . runMaybeT $ do
    branch@Branch {projectId} <- MaybeT $ Q.branchByProjectBranchShortHand projectBranchShortHand
    project <- lift $ Q.expectProjectById projectId
    pure (project, branch)
  where
    missingBranch = respondError (EntityMissing (ErrorID "missing-project-branch") "Branch could not be found")

branchesServer :: Maybe Session -> UserHandle -> ProjectSlug -> ServerT API.ProjectBranchesAPI WebApp
branchesServer session userHandle projectSlug =
  listBranchesByProjectEndpoint session userHandle projectSlug
    :<|> ( \branchShortHand ->
             hoistServer (Proxy @API.ProjectBranchResourceAPI) (addTags branchShortHand) $
               ( getProjectBranchReadmeEndpoint session userHandle projectSlug branchShortHand
                   :<|> getProjectBranchReleaseNotesEndpoint session userHandle projectSlug branchShortHand
                   :<|> getProjectBranchDetailsEndpoint session userHandle projectSlug branchShortHand
                   :<|> deleteProjectBranchEndpoint session userHandle projectSlug branchShortHand
                   :<|> branchHistoryEndpoint session userHandle projectSlug branchShortHand
                   :<|> branchCodeBrowsingServer session userHandle projectSlug branchShortHand
               )
         )
  where
    addTags :: forall x. BranchShortHand -> WebApp x -> WebApp x
    addTags BranchShortHand {contributorHandle, branchName} m = do
      let pbsh =
            ProjectBranchShortHand
              { userHandle,
                projectSlug,
                contributorHandle,
                branchName
              }
      addRequestTag "branch" (IDs.toText pbsh)
      m

branchCodeBrowsingServer :: Maybe Session -> UserHandle -> ProjectSlug -> BranchShortHand -> ServerT API.CodeBrowseAPI WebApp
branchCodeBrowsingServer session handle projectSlug branchShortHand =
  ( projectBranchBrowseEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionsByNameEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionsByHashEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchTermSummaryEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchTypeSummaryEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionDependenciesByNameEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionDependenciesByHashEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionDependentsByNameEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchDefinitionDependentsByHashEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchFindEndpoint session handle projectSlug branchShortHand
      :<|> projectBranchNamespacesByNameEndpoint session handle projectSlug branchShortHand
  )

projectBranchBrowseEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe Path.Path ->
  Maybe Path.Path ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceListing)
projectBranchBrowseEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle = mayContributorHandle, branchName}) relativeTo namespace rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-browse" cacheParams causalId $ do
    PG.runTransactionModeOrRespondError PG.ReadCommitted PG.ReadWrite $ do
      NL.serve codebase causalId relativeTo namespace `whenNothingM` throwError (EntityMissing (ErrorID "namespace-not-found") "Namespace not found")
  where
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle = mayContributorHandle, branchName}

    cacheParams = [IDs.toText projectBranchShortHand, tShow $ fromMaybe mempty relativeTo, tShow $ fromMaybe mempty namespace]

projectBranchDefinitionsByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  HQ.HashQualified Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
projectBranchDefinitionsByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) name relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  unisonRuntime <- asks Env.sandboxedRuntime
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definitions-by-name" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CR.withCodebaseRuntime codebase unisonRuntime \rt ->
        ShareBackend.displayDefinitionByHQName codebase (fromMaybe mempty relativeTo) causalId renderWidth (Suffixify False) rt name
  where
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, HQ.toTextWith Name.toText name, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchDefinitionsByHashEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Referent ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
projectBranchDefinitionsByHashEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) referent relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  unisonRuntime <- asks Env.sandboxedRuntime
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definitions-by-hash" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CR.withCodebaseRuntime codebase unisonRuntime \rt ->
        ShareBackend.displayDefinitionByHQName codebase (fromMaybe mempty relativeTo) causalId renderWidth (Suffixify False) rt query
  where
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, toUrlPiece referent, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchTermSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Referent ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TermSummary)
projectBranchTermSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) ref mayName relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-term-summary" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      rootBHId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
      np <- NP.namesPerspectiveForRootAndPath rootBHId (maybe mempty pathToPathSegments relativeTo)
      serveTermSummary codebase ref mayName np renderWidth
  where
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchTypeSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Reference ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TypeSummary)
projectBranchTypeSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) ref mayName relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-type-summary" cacheParams causalId do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite do
      CodeCache.withCodeCache codebase \codeCache -> do
        serveTypeSummary codeCache ref mayName renderWidth
  where
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchDefinitionDependenciesByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  HQ.HashQualified Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionSearchResults)
projectBranchDefinitionDependenciesByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug bsh@(BranchShortHand {contributorHandle, branchName}) name relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definition-dependencies-by-name" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CodeCache.withCodeCache codebase \codeCache -> do
        rootBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np <- NP.namesPerspectiveForRootAndPath rootBranchHashId (maybe mempty pathToPathSegments relativeTo)
        DefinitionSearchResults <$> ShareBackend.definitionDependencyResults codebase codeCache name projectShorthand branchOrReleaseShortHand np renderWidth
  where
    branchOrReleaseShortHand = IDs.IsBranchShortHand bsh
    projectShorthand = ProjectShortHand {userHandle, projectSlug}
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, HQ.toTextWith Name.toText name, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchDefinitionDependenciesByHashEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Referent ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionSearchResults)
projectBranchDefinitionDependenciesByHashEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug bsh@(BranchShortHand {contributorHandle, branchName}) referent relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definition-dependencies-by-hash" (cacheParams query) causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CodeCache.withCodeCache codebase \codeCache -> do
        rootBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np <- NP.namesPerspectiveForRootAndPath rootBranchHashId (maybe mempty pathToPathSegments relativeTo)
        DefinitionSearchResults <$> ShareBackend.definitionDependencyResults codebase codeCache query projectShorthand branchOrReleaseShortHand np renderWidth
  where
    branchOrReleaseShortHand = IDs.IsBranchShortHand bsh
    projectShorthand = ProjectShortHand {userHandle, projectSlug}
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams query = [IDs.toText projectBranchShortHand, HQ.toTextWith Name.toText query, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchDefinitionDependentsByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  HQ.HashQualified Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionSearchResults)
projectBranchDefinitionDependentsByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug bsh@(BranchShortHand {contributorHandle, branchName}) name relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definition-dependents-by-name" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CodeCache.withCodeCache codebase \codeCache -> do
        rootBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np <- NP.namesPerspectiveForRootAndPath rootBranchHashId (maybe mempty pathToPathSegments relativeTo)
        DefinitionSearchResults <$> ShareBackend.definitionDependentResults codebase codeCache name projectShorthand branchOrReleaseShortHand np renderWidth
  where
    branchOrReleaseShortHand = IDs.IsBranchShortHand bsh
    projectShorthand = ProjectShortHand {userHandle, projectSlug}
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams = [IDs.toText projectBranchShortHand, HQ.toTextWith Name.toText name, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchDefinitionDependentsByHashEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Referent ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionSearchResults)
projectBranchDefinitionDependentsByHashEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug bsh@(BranchShortHand {contributorHandle, branchName}) referent relativeTo renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  causalId <- resolveRootHash codebase branchHead rootHash
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-definition-dependents-by-hash" (cacheParams query) causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CodeCache.withCodeCache codebase \codeCache -> do
        rootBranchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np <- NP.namesPerspectiveForRootAndPath rootBranchHashId (maybe mempty pathToPathSegments relativeTo)
        DefinitionSearchResults <$> ShareBackend.definitionDependentResults codebase codeCache query projectShorthand branchOrReleaseShortHand np renderWidth
  where
    branchOrReleaseShortHand = IDs.IsBranchShortHand bsh
    projectShorthand = ProjectShortHand {userHandle, projectSlug}
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}
    cacheParams query = [IDs.toText projectBranchShortHand, HQ.toTextWith Name.toText query, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

projectBranchFindEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe Path.Path ->
  Maybe Int ->
  Maybe Pretty.Width ->
  Text ->
  Bool ->
  Maybe CausalHash ->
  WebApp [(Fuzzy.Alignment, Fuzzy.FoundResult)]
projectBranchFindEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) mayRelativeTo limit renderWidth query searchDependencies rootHash = do
  let relativeTo = fromMaybe mempty mayRelativeTo
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
    Fuzzy.serveFuzzyFind codebase inScratch searchDependencies causalId relativeTo limit renderWidth query
  where
    inScratch = False
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}

projectBranchNamespacesByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceDetails)
projectBranchNamespacesByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) path renderWidth rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  unisonRuntime <- asks Env.sandboxedRuntime
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "project-branch-namespaces-by-name" cacheParams causalId $ do
    PG.runTransactionModeOrRespondError PG.ReadCommitted PG.ReadWrite $ do
      CR.withCodebaseRuntime codebase unisonRuntime \rt ->
        ND.namespaceDetails codebase rt (fromMaybe mempty path) causalId renderWidth
          `whenNothingM` throwError (EntityMissing (ErrorID "namespace-not-found") "Namespace not found")
  where
    cacheParams = [IDs.toText projectBranchShortHand, tShow path, foldMap (toUrlPiece . Pretty.widthToInt) renderWidth]
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}

-- | Gets the readme for a project branch.
getProjectBranchReadmeEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe CausalHash ->
  WebApp (Cached JSON ReadmeResponse)
getProjectBranchReadmeEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug (BranchShortHand {contributorHandle, branchName}) rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let rootPath = mempty
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  unisonRuntime <- asks Env.sandboxedRuntime
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "get-project-branch-readme" cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $ do
      CR.withCodebaseRuntime codebase unisonRuntime \rt -> do
        mayNamespaceDetails <- ND.namespaceDetails codebase rt rootPath causalId Nothing
        let mayReadme = do
              NamespaceDetails {readme} <- mayNamespaceDetails
              readme
        pure $ ReadmeResponse {readMe = mayReadme, markdownReadMe = MD.toText . MD.toMarkdown <$> mayReadme}
  where
    cacheParams = [IDs.toText projectBranchShortHand]
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}

getProjectBranchReleaseNotesEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe CausalHash ->
  WebApp (Cached JSON DocResponse)
getProjectBranchReleaseNotesEndpoint caller userHandle projectSlug releaseVersion = do
  getProjectBranchDocEndpoint "get-project-branch-release-notes" releaseNotesNames caller userHandle projectSlug releaseVersion
  where
    releaseNotesNames = Set.fromList $ NameSegment <$> ["release_notes", "ReleaseNotes", "releaseNotes", "RELEASE_NOTES"]

getProjectBranchDetailsEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  WebApp ShareBranch
getProjectBranchDetailsEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug branchShortHand = do
  let projectShortHand = ProjectShortHand {userHandle, projectSlug}
  (project@Project {projectId}, projectOwner, projectBranch@(Branch {branchId = projectBranchId})) <- PG.runTransactionOrRespondError do
    project@Project {projectId, ownerUserId} <- Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @ProjectShortHand projectShortHand))
    projectOwner <- Q.projectOwnerByProjectId projectId `whenNothingM` throwError (EntityMissing (ErrorID "user-not-found") ("User not found: " <> IDs.toText @UserId ownerUserId))
    projectBranch <- Q.branchByProjectIdAndShortHand projectId branchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch-not-found") ("Branch not found: " <> IDs.toText @BranchShortHand branchShortHand))
    projectBranchWithCausals <- CausalQ.expectCausalHashesByIdsOf branchCausals_ projectBranch
    pure (project, projectOwner, projectBranchWithCausals)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  PG.runTransaction $ do
    branchContributions <-
      ContributionsQ.shareContributionsByBranchOf id projectBranchId
        >>= UsersQ.userDisplayInfoOf (traversed . traversed)
    let shareProject = projectToAPI projectOwner project
    pure $ API.branchToShareBranch branchShortHand projectBranch shareProject branchContributions

deleteProjectBranchEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  WebApp ()
deleteProjectBranchEndpoint session userHandle projectSlug branchShortHand = do
  callerUserId <- AuthN.requireAuthenticatedUser session
  let projectShortHand = ProjectShortHand {userHandle, projectSlug}
  (project, projectBranch@Branch {branchId}) <- PG.runTransactionOrRespondError do
    project@Project {projectId} <- Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @ProjectShortHand projectShortHand))
    projectBranch <- Q.branchByProjectIdAndShortHand projectId branchShortHand `whenNothingM` throwError (EntityMissing (ErrorID "branch-not-found") ("Branch not found: " <> IDs.toText @BranchShortHand branchShortHand))
    pure (project, projectBranch)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkBranchDelete callerUserId project projectBranch
  PG.runTransaction $ Q.softDeleteBranch branchId
  pure ()

branchHistoryEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe (Cursor CausalHistoryCursor) ->
  Maybe Limit ->
  WebApp BranchHistoryResponse
branchHistoryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug branchRef@(BranchShortHand {contributorHandle, branchName}) mayCursor mayLimit = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead Nothing
  PG.runTransaction do
    history <-
      CausalQ.pagedCausalAncestors causalId limit mayCursor
        <&> fmap \(causalHash) ->
          BranchHistoryCausalEntry (BranchHistoryCausal {causalHash})
    pure $
      BranchHistoryResponse
        { projectRef,
          branchRef,
          history
        }
  where
    projectRef = ProjectShortHand {userHandle, projectSlug}
    limit = fromMaybe defaultLimit mayLimit
    defaultLimit = Limit 20
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}

getProjectBranchDocEndpoint ::
  Text ->
  Set NameSegment ->
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  BranchShortHand ->
  Maybe CausalHash ->
  WebApp (Cached JSON DocResponse)
getProjectBranchDocEndpoint cacheKey docNames (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug BranchShortHand {contributorHandle, branchName} rootHash = do
  (Project {ownerUserId = projectOwnerUserId, projectId}, Branch {causal = branchHead, contributorId}) <- getProjectBranch projectBranchShortHand
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkProjectBranchRead callerUserId projectId
  let rootPath = mempty
  let codebaseLoc = Codebase.codebaseLocationForProjectBranchCodebase projectOwnerUserId contributorId
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  causalId <- resolveRootHash codebase branchHead rootHash
  unisonRuntime <- asks Env.sandboxedRuntime
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc cacheKey cacheParams causalId $ do
    PG.runTransactionMode PG.ReadCommitted PG.ReadWrite $
      CR.withCodebaseRuntime codebase unisonRuntime \rt -> do
        doc <- findAndRenderDoc codebase docNames rt rootPath causalId Nothing
        pure $ DocResponse {doc}
  where
    cacheParams = [IDs.toText projectBranchShortHand]
    projectBranchShortHand = ProjectBranchShortHand {userHandle, projectSlug, contributorHandle, branchName}

listBranchesByProjectEndpoint ::
  Maybe Session ->
  UserHandle ->
  ProjectSlug ->
  Maybe (Cursor ListBranchesCursor) ->
  Maybe Limit ->
  Maybe (IDs.PrefixedID "@" UserHandle) ->
  Maybe API.BranchKindFilter ->
  Maybe Query ->
  WebApp (Paged (UTCTime, BranchId) ShareBranch)
listBranchesByProjectEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle projectSlug mayCursor mayLimit mayContributorHandle mayKindFilter mayPrefixQuery = do
  let projectShortHand = ProjectShortHand {userHandle, projectSlug}
  (project@Project {projectId}, projectOwner) <- PG.runTransactionOrRespondError do
    project@Project {ownerUserId, projectId} <- Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @ProjectShortHand projectShortHand))
    projectOwner <- Q.projectOwnerByProjectId projectId `whenNothingM` throwError (EntityMissing (ErrorID "user-not-found") ("User not found: " <> IDs.toText @UserId ownerUserId))
    pure (project, projectOwner)
  _authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkListBranchesForProject callerUserId projectId
  (mayNamePrefix, mayContributorFilter) <- computeSearchFilters
  branches <- PG.runTransaction do
    branches <- Q.listBranchesByProject limit mayCursor mayNamePrefix mayContributorFilter (fromMaybe defaultKindFilter mayKindFilter) projectId
    branchesWithContributions :: (Paged (UTCTime, BranchId) (Branch CausalId, [ShareContribution UserDisplayInfo], Maybe UserHandle)) <-
      branches
        & fmap (\(branch@(Branch {branchId}), contributorHandle) -> (branch, branchId, contributorHandle))
        & ContributionsQ.shareContributionsByBranchOf (traversed . _2)
        >>= UsersQ.userDisplayInfoOf (traversed . _2 . traversed . traversed)
    CausalQ.expectCausalHashesByIdsOf (traversed . _1 . branchCausals_) branchesWithContributions

  let shareProject = projectToAPI projectOwner project
  let shareBranches =
        branches
          <&> ( \(branch@(Branch {branchName}), contributions, contributorHandle) -> do
                  let branchShortHand = BranchShortHand {branchName, contributorHandle}
                   in API.branchToShareBranch branchShortHand branch shareProject contributions
              )
  pure shareBranches
  where
    userIdForHandle handle = do
      fmap user_id <$> PG.runTransaction (UserQ.userByHandle handle)
    limit = fromMaybe defaultLimit mayLimit
    defaultLimit = Limit 20
    defaultKindFilter = AllBranchKinds
    -- Compute the search filters from the query parameters,
    -- This gets a bit tricky because the contributor can be filtered either by a name
    -- prefix or a contributor handle, but not both.
    computeSearchFilters :: WebApp (Maybe Query, Maybe (Either IDs.UserId Query))
    computeSearchFilters = do
      (mayNamePrefix, contributorPrefixFilter) <- case mayPrefixQuery of
        Just (Query prefixQuery) ->
          case Text.uncons prefixQuery of
            -- The name prefix has a contributor portion
            Just ('@', query) -> case Text.splitOn "/" query of
              [contributorHandlePrefix] -> pure (Nothing, Just . Right $ Query contributorHandlePrefix)
              (contributorUserHandleTxt : namePrefix) -> do
                -- The name prefix has a contributor portion and a name portion
                -- Try parsing the handle so we can do a proper user-id lookup for the contributor,
                -- but fall back to a prefix search on the contributor portion.
                case IDs.fromText @UserHandle contributorUserHandleTxt of
                  -- If we got a handle, try to find a user for it.
                  -- If we don't find a user, fall back to a prefix search on the contributor portion.
                  Right handle -> do
                    userIdForHandle handle >>= \case
                      Nothing -> pure (Just (Query $ Text.intercalate "/" namePrefix), Just (Right (Query contributorUserHandleTxt)))
                      Just contributorUserId -> pure (Just (Query $ Text.intercalate "/" namePrefix), Just (Left contributorUserId))
                  Left {} -> pure (Just (Query $ Text.intercalate "/" namePrefix), Just (Right (Query contributorUserHandleTxt)))
              [] -> pure (Nothing, Nothing)
            -- No contributor portion, just do a name prefix search
            Just _ -> pure (mayPrefixQuery, Nothing)
            Nothing -> pure (Nothing, Nothing)
        Nothing -> pure (Nothing, Nothing)
      -- If we have a contributor handle, look up the user id for it
      mayContributorUserId <- for mayContributorHandle \(IDs.PrefixedID contributorHandle) -> do
        contributorUserId <- userIdForHandle contributorHandle `whenNothingM` respondError (EntityMissing (ErrorID "user-not-found") ("User not found: " <> IDs.toText @UserHandle contributorHandle))
        pure contributorUserId
      contributorFilter <- case (contributorPrefixFilter, mayContributorUserId) of
        (Just _, Just _) ->
          respondError (BadRequest "Cannot specify both contributor handle and a contributor prefix query")
        (a, b) -> pure (a <|> (Left <$> b))
      pure (mayNamePrefix, contributorFilter)

listBranchesByUserEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe (Cursor ListBranchesCursor) ->
  Maybe ProjectShortHand ->
  Maybe Limit ->
  Maybe Query ->
  WebApp (Paged ListBranchesCursor ShareBranch)
listBranchesByUserEndpoint (AuthN.MaybeAuthedUserID callerUserId) contributorHandle mayCursor mayProjectShortHand mayLimit mayNamePrefix = do
  branches <- PG.runTransactionOrRespondError $ do
    contributorUser <- UserQ.userByHandle contributorHandle `whenNothingM` throwError (EntityMissing (ErrorID "user-not-found") ("User not found: " <> IDs.toText @UserHandle contributorHandle))
    mayProjectId <- for mayProjectShortHand \projSH -> do
      Project.projectId <$> (Q.projectByShortHand projSH) `whenNothingM` throwError (EntityMissing (ErrorID "project-not-found") ("Project not found: " <> IDs.toText @ProjectShortHand projSH))
    Q.listContributorBranchesOfUserAccessibleToCaller (user_id contributorUser) callerUserId limit mayCursor mayNamePrefix mayProjectId

  expandedBranches <- PG.runTransaction $ do
    branchesWithContributions <-
      branches
        & fmap (\(branch@(Branch {branchId}), project, projectOwnerHandle) -> (branch, branchId, project, projectOwnerHandle))
        & ContributionsQ.shareContributionsByBranchOf (traversed . _2)
    branchesWithContributions
      & CausalQ.expectCausalHashesByIdsOf (traversed . _1 . branchCausals_)
      >>= UsersQ.userDisplayInfoOf (traversed . _2 . traversed . traversed)

  let shareBranches =
        expandedBranches
          <&> ( \(branch@(Branch {branchName}), contributions, project, projectOwnerHandle) ->
                  let branchShortHand = BranchShortHand {branchName, contributorHandle = Just contributorHandle}
                      shareProject = projectToAPI projectOwnerHandle project
                   in API.branchToShareBranch branchShortHand branch shareProject contributions
              )
  pure shareBranches
  where
    defaultLimit = Limit 20
    limit = fromMaybe defaultLimit mayLimit

-- | Given an optional root hash, and a branch head, validate that the root hash is accessible from the branch head.
resolveRootHash :: Codebase.CodebaseEnv -> CausalId -> Maybe CausalHash -> WebApp CausalId
resolveRootHash codebase branchHead rootHash = do
  case rootHash of
    Just rh -> do
      rootCausalId <- PG.runTransactionMode PG.ReadCommitted PG.Read $ Codebase.expectCausalIdByHash codebase rh
      AuthZ.assertCausalHashAccessibleFromRoot branchHead rootCausalId
      pure rootCausalId
    Nothing -> pure branchHead
