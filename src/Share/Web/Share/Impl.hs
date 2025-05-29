{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Impl where

import Control.Lens
import Data.Text qualified as Text
import Servant
import Share.Codebase qualified as Codebase
import Share.Codebase.Types qualified as Codebase
import Share.IDs (TourId, UserHandle (..))
import Share.IDs qualified as IDs
import Share.JWT qualified as JWT
import Share.Notifications.Impl qualified as Notifications
import Share.OAuth.Session
import Share.OAuth.Types (UserId)
import Share.Postgres qualified as PG
import Share.Postgres.Authorization.Queries qualified as AuthZQ
import Share.Postgres.IDs (CausalHash)
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Projects.Queries qualified as PQ
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Releases.Queries qualified as RQ
import Share.Postgres.Search.DefinitionSearch.Queries qualified as DDQ
import Share.Postgres.Search.DefinitionSearch.Queries qualified as DSQ
import Share.Postgres.Users.Queries qualified as UserQ
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Project (Project (..))
import Share.Release (Release (..))
import Share.User (User (..))
import Share.User qualified as User
import Share.UserProfile (UserProfile (..))
import Share.Utils.API
import Share.Utils.Caching
import Share.Utils.Logging qualified as Logging
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.API qualified as Share
import Share.Web.Share.Branches.Impl qualified as Branches
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Contributions.Impl qualified as Contributions
import Share.Web.Share.DefinitionSearch qualified as DefinitionSearch
import Share.Web.Share.DisplayInfo.Queries qualified as DisplayInfoQ
import Share.Web.Share.DisplayInfo.Types (OrgDisplayInfo (..), UserLike (..))
import Share.Web.Share.Projects.Impl qualified as Projects
import Share.Web.Share.Types
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Share.DefinitionSummary (serveTermSummary, serveTypeSummary)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary, TypeSummary)
import Unison.Server.Share.Definitions qualified as ShareBackend
import Unison.Server.Share.FuzzyFind qualified as Fuzzy
import Unison.Server.Share.NamespaceDetails qualified as ND
import Unison.Server.Share.NamespaceListing (NamespaceListing (..))
import Unison.Server.Share.NamespaceListing qualified as NL
import Unison.Server.Types (DefinitionDisplayResults, NamespaceDetails (..), Suffixify (..))
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty qualified as Pretty

userCodebaseServer :: ServerT Share.UserPublicCodebaseAPI WebApp
userCodebaseServer session handle =
  hoistServer (Proxy @CodeBrowseAPI) addTags $
    ( browseEndpoint session handle
        :<|> definitionsByNameEndpoint session handle
        :<|> definitionsByHashEndpoint session handle
        :<|> termSummaryEndpoint session handle
        :<|> typeSummaryEndpoint session handle
        :<|> findEndpoint session handle
        :<|> namespacesByNameEndpoint session handle
    )
  where
    addTags :: forall x. WebApp x -> WebApp x
    addTags m = do
      addRequestTag "codebase-handle" (IDs.toText handle)
      m

userServer :: ServerT Share.UserAPI WebApp
userServer session handle =
  hoistServerWithContext (Proxy @Share.UserResourceAPI) ctxType addTags $
    ( getUserReadmeEndpoint session handle
        :<|> getUserProfileEndpoint mayCallerId handle
        :<|> updateUserEndpoint handle
        :<|> Projects.projectServer session handle
        :<|> Branches.listBranchesByUserEndpoint session handle
        :<|> Contributions.listContributionsByUserEndpoint session handle
        :<|> Notifications.server handle
    )
  where
    mayCallerId = sessionUserId <$> session
    ctxType = Proxy @(AuthCheckCtx .++ '[Cookies.CookieSettings, JWT.JWTSettings])
    addTags :: forall x. WebApp x -> WebApp x
    addTags m = do
      addRequestTag "user-handle" (IDs.toText handle)
      m

accountServer :: ServerT Share.AccountAPI WebApp
accountServer session =
  ( accountInfoEndpoint session
      :<|> completeToursEndpoint session
  )

browseEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe Path.Path ->
  Maybe Path.Path ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceListing)
browseEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle relativeTo namespace rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner namespacePrefix
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "browse" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionModeOrRespondError PG.ReadCommitted PG.ReadWrite codebase $ do
      NL.serve rootCausalId relativeTo namespace `whenNothingM` throwError (EntityMissing (ErrorID "no-namespace") $ "No namespace found at " <> Path.toText namespacePrefix)
  where
    cacheParams = [tShow $ fromMaybe mempty relativeTo, tShow $ fromMaybe mempty namespace]
    namespacePrefix :: Path.Path
    namespacePrefix =
      fromMaybe mempty relativeTo <> fromMaybe mempty namespace

definitionsByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  HQ.HashQualified Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
definitionsByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle name relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let query = name
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "definitions-by-name" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
      ShareBackend.definitionForHQName (fromMaybe mempty relativeTo) rootCausalId renderWidth (Suffixify False) rt query
  where
    cacheParams = [HQ.toTextWith Name.toText name, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe mempty relativeTo
          suffix = maybe mempty Path.fromName $ HQ.toName name
       in prefix <> suffix

definitionsByHashEndpoint ::
  Maybe Session ->
  UserHandle ->
  Referent ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON DefinitionDisplayResults)
definitionsByHashEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle referent relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  let authPath = fromMaybe mempty relativeTo
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "definitions-by-hash" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
      ShareBackend.definitionForHQName (fromMaybe mempty relativeTo) rootCausalId renderWidth (Suffixify False) rt query
  where
    cacheParams = [toUrlPiece referent, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]

termSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  Referent ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TermSummary)
termSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle ref mayName relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "term-summary" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
      serveTermSummary ref mayName rootCausalId relativeTo renderWidth
  where
    cacheParams = [toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe mempty relativeTo
          suffix = maybe mempty Path.fromName mayName
       in prefix <> suffix

typeSummaryEndpoint ::
  Maybe Session ->
  UserHandle ->
  Reference ->
  Maybe Name ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON TypeSummary)
typeSummaryEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle ref mayName relativeTo renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "type-summary" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
      serveTypeSummary ref mayName renderWidth
  where
    cacheParams = [toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe mempty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe mempty relativeTo
          suffix = maybe mempty Path.fromName mayName
       in prefix <> suffix

findEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe Path.Path ->
  Maybe Int ->
  Maybe Pretty.Width ->
  Text ->
  Bool ->
  Maybe CausalHash ->
  WebApp [(Fuzzy.Alignment, Fuzzy.FoundResult)]
findEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle mayRelativeTo limit renderWidth query searchDependencies rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  let relativeTo = fromMaybe mempty mayRelativeTo
  let authPath = relativeTo
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
    Fuzzy.serveFuzzyFind isInScratch searchDependencies rootCausalId relativeTo limit renderWidth query
  where
    isInScratch = True

namespacesByNameEndpoint ::
  Maybe Session ->
  UserHandle ->
  Maybe Path.Path ->
  Maybe Pretty.Width ->
  Maybe CausalHash ->
  WebApp (Cached JSON NamespaceDetails)
namespacesByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle (fromMaybe mempty -> path) renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner path
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "namespaces-by-name" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionModeOrRespondError PG.ReadCommitted PG.ReadWrite codebase $ do
      ND.namespaceDetails rt path rootCausalId renderWidth `whenNothingM` throwError (EntityMissing (ErrorID "no-namespace") $ "No namespace found at " <> Path.toText path)
  where
    cacheParams = [tShow path]

getUserProfileEndpoint :: Maybe UserId -> UserHandle -> WebApp DescribeUserProfile
getUserProfileEndpoint callerUserId userHandle = do
  PG.runTransactionOrRespondError do
    User {user_id} <- UserQ.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
    displayInfo <- DisplayInfoQ.unifiedDisplayInfoForUserOf id user_id
    UserProfile {bio, website, location, twitterHandle, pronouns} <- UsersQ.userProfileById user_id `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
    permissions <- case displayInfo of
      UnifiedOrg (OrgDisplayInfo {orgId}) -> do
        permissionsWithinOrg <- AuthZQ.permissionsForOrg callerUserId orgId
        pure permissionsWithinOrg
      UnifiedUser _ -> pure mempty
    pure $
      DescribeUserProfile
        { bio = bio,
          website = website,
          location = location,
          twitterHandle = twitterHandle,
          pronouns = pronouns,
          permissions,
          displayInfo = displayInfo
        }

updateUserEndpoint :: UserHandle -> UserId -> UpdateUserRequest -> WebApp DescribeUserProfile
updateUserEndpoint userHandle callerUserId (UpdateUserRequest {name, avatarUrl, bio, website, location, twitterHandle, pronouns}) = do
  User {user_id = toUpdateUserId} <- PG.runTransactionOrRespondError $ do
    UserQ.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkUserUpdate callerUserId toUpdateUserId
  PG.runTransaction $ do
    UsersQ.updateUser toUpdateUserId name avatarUrl bio website location twitterHandle pronouns
  getUserProfileEndpoint (Just callerUserId) userHandle

-- | Gets the readme for a user.
-- This was separated from the user profile endpoint because fetching the readme from the
-- codebase is slow, and there are more places where we want the user profile than we need the
-- readme.
getUserReadmeEndpoint :: Maybe Session -> UserHandle -> WebApp (Cached JSON ReadmeResponse)
getUserReadmeEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle = do
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  let path = Codebase.publicRoot
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner path
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.Read codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "get-user-readme" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionMode PG.ReadCommitted PG.ReadWrite codebase $ do
      mayNamespaceDetails <- ND.namespaceDetails rt path rootCausalId Nothing
      let mayReadme = do
            NamespaceDetails {readme} <- mayNamespaceDetails
            readme
      pure $ ReadmeResponse {readMe = mayReadme}
  where
    cacheParams = [IDs.toText userHandle]

-- | TODO: Should allow users to see private users if they have access to them, but this will
-- likely change when codebases and users are no longer one-to-one, so for now we just hide
-- all private users in the PG query itself.
searchEndpoint :: Maybe Session -> Query -> Maybe Limit -> WebApp [SearchResult]
searchEndpoint _caller (Query "") _limit = pure []
searchEndpoint (MaybeAuthedUserID callerUserId) (Query query) (fromMaybe (Limit 20) -> limit) = do
  (userQuery :: Query, (projectUserFilter :: Maybe UserId, projectQuery :: Query)) <-
    fromMaybe query (Text.stripPrefix "@" query)
      & Text.splitOn "/"
      & \case
        (userQuery : projectQueryText : _rest) -> do
          mayUserId <- PG.runTransaction $ fmap User.user_id <$> UserQ.userByHandle (UserHandle userQuery)
          pure (Query query, (mayUserId, Query projectQueryText))
        [projectOrUserQuery] -> pure (Query projectOrUserQuery, (Nothing, Query projectOrUserQuery))
        -- This is impossible
        [] -> pure (Query query, (Nothing, Query query))
  -- We don't have a great way to order users and projects together, so we just limit to a max
  -- of 5 users (who match the query as a prefix), then return the rest of the results from
  -- projects.
  (userLikes, projects) <- PG.runTransaction $ do
    userLikes <- UserQ.searchUsersByNameOrHandlePrefix userQuery (Limit 5)
    userLikesWithInfo <- DisplayInfoQ.userLikeDisplayInfoOf traversed userLikes
    projects <- Q.searchProjects callerUserId projectUserFilter projectQuery limit
    pure (userLikesWithInfo, projects)
  let userResults = SearchResultUserLike <$> userLikes
  let projectResults =
        projects
          <&> \(Project {slug, summary, visibility}, ownerHandle) ->
            let projectShortHand = IDs.ProjectShortHand {userHandle = ownerHandle, projectSlug = slug}
             in SearchResultProject projectShortHand summary visibility
  pure $ userResults <> projectResults

searchDefinitionNamesEndpoint ::
  Maybe UserId ->
  Query ->
  Maybe Limit ->
  Maybe (IDs.PrefixedID "@" UserHandle) ->
  Maybe IDs.ProjectShortHand ->
  Maybe IDs.ReleaseVersion ->
  WebApp [DefinitionNameSearchResult]
searchDefinitionNamesEndpoint callerUserId query@(Query queryText) mayLimit userFilter projectFilter releaseFilter = do
  filter <- runMaybeT $ resolveProjectAndReleaseFilter projectFilter releaseFilter <|> resolveUserFilter (IDs.unPrefix <$> userFilter)
  matches <-
    (PG.runTransaction $ DDQ.defNameCompletionSearch callerUserId filter query limit)
      <&> ordNubOn (view _1) . (mapMaybe $ traverseOf _1 (rewriteMatches queryText))
  let response = matches <&> \(name, tag) -> DefinitionNameSearchResult name tag
  pure response
  where
    limit = fromMaybe (Limit 20) mayLimit

    -- Try to rewrite the name to only include the part of the name that matches the query,
    -- and any following suffix.
    -- >>> Name.toText <$> rewriteMatches "foo" (Name.unsafeParseText "Foo.Bar.baz")
    -- Just "Foo.Bar.baz"
    --
    -- >>> Name.toText <$>  rewriteMatches "list.ma" (Name.unsafeParseText "data.List.map")
    -- Just "List.map"
    --
    -- No match; we shouldn't get these back from the query, but if we do, just filter it out
    -- >>> Name.toText <$>  rewriteMatches "foo" (Name.unsafeParseText "bar.baz")
    -- Nothing
    rewriteMatches :: Text -> Name -> Maybe Name
    rewriteMatches q name
      | nameText <- Name.toText name,
        Text.isInfixOf (Text.toLower q) (Text.toLower nameText) =
          -- Find where the query starts in the name, and take the rest of the name from there
          let (_before, after) = Text.breakOnEnd (Text.toLower q) (Text.toLower nameText)
           in -- Use the length of the matching bits to get the right part of the name, but with the correct casing
              Name.parseText (Text.takeEnd (Text.length $ q <> after) nameText)
      | otherwise = Nothing

resolveProjectAndReleaseFilter :: Maybe IDs.ProjectShortHand -> Maybe IDs.ReleaseVersion -> MaybeT WebApp DDQ.DefnNameSearchFilter
resolveProjectAndReleaseFilter projectFilter releaseFilter = do
  projectShortHand <- hoistMaybe projectFilter
  Project {projectId} <- lift . PG.runTransactionOrRespondError $ Q.projectByShortHand projectShortHand `whenNothingM` throwError (EntityMissing (ErrorID "no-project-found") $ "No project found for short hand: " <> IDs.toText projectShortHand)
  case releaseFilter of
    Nothing -> pure $ DDQ.ProjectFilter projectId
    Just releaseVersion -> do
      Release {releaseId} <- lift . PG.runTransactionOrRespondError $ Q.releaseByProjectIdAndReleaseShortHand projectId (IDs.ReleaseShortHand releaseVersion) `whenNothingM` throwError (EntityMissing (ErrorID "no-release-found") $ "No release found for project: " <> IDs.toText projectShortHand <> " and version: " <> IDs.toText releaseVersion)
      pure $ DDQ.ReleaseFilter releaseId

resolveUserFilter :: Maybe UserHandle -> MaybeT WebApp DDQ.DefnNameSearchFilter
resolveUserFilter userFilter = do
  userHandle <- hoistMaybe userFilter
  User {user_id} <- lift $ PG.runTransactionOrRespondError $ UserQ.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
  pure $ DDQ.UserFilter user_id

searchDefinitionsEndpoint ::
  Maybe UserId ->
  Query ->
  Maybe Limit ->
  Maybe (IDs.PrefixedID "@" UserHandle) ->
  Maybe IDs.ProjectShortHand ->
  Maybe IDs.ReleaseVersion ->
  WebApp DefinitionSearchResults
searchDefinitionsEndpoint callerUserId (Query query) mayLimit userFilter projectFilter releaseFilter = do
  Logging.logInfoText $ "definition-search-query: " <> query
  filter <- runMaybeT $ resolveProjectAndReleaseFilter projectFilter releaseFilter <|> resolveUserFilter (IDs.unPrefix <$> userFilter)
  matches <- case Text.words query of
    [] -> pure $ []
    [":"] -> pure $ []
    -- If the query is a single word, and it doesn't contain "->", we treat it as a name search
    [name]
      | not (Text.isInfixOf "->" name) && not (Text.isInfixOf "#" name) ->
          PG.runTransactionMode PG.ReadCommitted PG.Read $
            DDQ.definitionNameSearch callerUserId filter limit (Query name)
    _ -> do
      case DefinitionSearch.queryToTokens query of
        Left _err -> do
          Logging.logErrorText $ "Failed to parse query: " <> query
          pure $ []
        Right (searchTokens, mayArity) -> do
          Logging.logInfoText $ "definition-search-tokens: " <> DSQ.searchTokensToTsQuery searchTokens
          PG.runTransactionMode PG.ReadCommitted PG.Read $
            DDQ.definitionTokenSearch callerUserId filter limit searchTokens mayArity
  results <-
    PG.runTransactionMode PG.ReadCommitted PG.Read $
      do
        PQ.expectProjectShortHandsOf (traversed . _1) matches
        >>= RQ.expectReleaseVersionsOf (traversed . _2)
        <&> over (traversed . _2) IDs.ReleaseShortHand
        <&> fmap
          ( \(project, release, fqn, summary) ->
              DefinitionSearchResult
                { fqn,
                  summary,
                  project,
                  release
                }
          )
  pure $ DefinitionSearchResults results
  where
    limit = fromMaybe (Limit 20) mayLimit

accountInfoEndpoint :: Session -> WebApp UserAccountInfo
accountInfoEndpoint Session {sessionUserId} = do
  User {user_email, user_id} <- PGO.expectUserById sessionUserId
  PG.runTransaction $ do
    completedTours <- Q.getCompletedToursForUser user_id
    organizationMemberships <- Q.organizationMemberships user_id
    isSuperadmin <- AuthZQ.isSuperadmin user_id
    displayInfo <- DisplayInfoQ.unifiedDisplayInfoForUserOf id user_id
    planTier <- UserQ.userSubscriptionTier user_id
    pure $
      UserAccountInfo
        { primaryEmail = user_email,
          completedTours,
          organizationMemberships,
          isSuperadmin,
          displayInfo,
          planTier
        }

completeToursEndpoint :: Session -> NonEmpty TourId -> WebApp NoContent
completeToursEndpoint Session {sessionUserId} flows = do
  PG.runTransaction $ Q.completeToursForUser sessionUserId flows
  pure NoContent
