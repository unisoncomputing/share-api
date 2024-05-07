{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Share.Web.Share.Impl where

import Share.Codebase qualified as Codebase
import Share.Codebase.Types qualified as Codebase
import Share.IDs (TourId, UserHandle)
import Share.IDs qualified as IDs
import Share.JWT qualified as JWT
import Share.OAuth.Session
import Share.OAuth.Types (UserId)
import Share.Postgres qualified as PG
import Share.Postgres.IDs (CausalHash)
import Share.Postgres.Ops qualified as PGO
import Share.Postgres.Queries qualified as Q
import Share.Postgres.Users.Queries qualified as UsersQ
import Share.Prelude
import Share.Project (Project (..))
import Share.User (User (..))
import Share.UserProfile (UserProfile (..))
import Share.Utils.API
import Share.Utils.Caching
import Share.Utils.Servant.Cookies qualified as Cookies
import Share.Web.App
import Share.Web.Authentication qualified as AuthN
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors
import Share.Web.Share.API qualified as Share
import Share.Web.Share.Branches.Impl qualified as Branches
import Share.Web.Share.CodeBrowsing.API (CodeBrowseAPI)
import Share.Web.Share.Contributions.Impl qualified as Contributions
import Share.Web.Share.Projects.Impl qualified as Projects
import Share.Web.Share.Types
import Servant
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Share.DefinitionSummary (TermSummary, TypeSummary, serveTermSummary, serveTypeSummary)
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
        :<|> getUserProfileEndpoint handle
        :<|> updateUserEndpoint handle
        :<|> Projects.projectServer session handle
        :<|> Branches.listBranchesByUserEndpoint session handle
        :<|> Contributions.listContributionsByUserEndpoint session handle
    )
  where
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
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "browse" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionOrRespondError codebase $ do
      NL.serve rootCausalId relativeTo namespace `whenNothingM` throwError (EntityMissing (ErrorID "no-namespace") $ "No namespace found at " <> Path.toText namespacePrefix)
  where
    cacheParams = [tShow $ fromMaybe Path.empty relativeTo, tShow $ fromMaybe Path.empty namespace]
    namespacePrefix :: Path.Path
    namespacePrefix =
      fromMaybe Path.empty relativeTo <> fromMaybe Path.empty namespace

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
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "definitions-by-name" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransaction codebase $ do
      ShareBackend.definitionForHQName (fromMaybe Path.empty relativeTo) rootCausalId renderWidth (Suffixify False) rt query
  where
    cacheParams = [HQ.toTextWith Name.toText name, tShow $ fromMaybe Path.empty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe Path.empty relativeTo
          suffix = maybe Path.empty Path.fromName $ HQ.toName name
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
  let authPath = fromMaybe Path.empty relativeTo
  let shortHash = Referent.toShortHash referent
  let query = HQ.HashOnly shortHash
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "definitions-by-hash" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransaction codebase $ do
      ShareBackend.definitionForHQName (fromMaybe Path.empty relativeTo) rootCausalId renderWidth (Suffixify False) rt query
  where
    cacheParams = [toUrlPiece referent, tShow $ fromMaybe Path.empty relativeTo, foldMap toUrlPiece renderWidth]

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
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "term-summary" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransaction codebase $ do
      serveTermSummary ref mayName rootCausalId relativeTo renderWidth
  where
    cacheParams = [toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe Path.empty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe Path.empty relativeTo
          suffix = maybe Path.empty Path.fromName mayName
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
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "type-summary" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransaction codebase $ do
      serveTypeSummary ref mayName rootCausalId relativeTo renderWidth
  where
    cacheParams = [toUrlPiece ref, maybe "" Name.toText mayName, tShow $ fromMaybe Path.empty relativeTo, foldMap toUrlPiece renderWidth]
    authPath :: Path.Path
    authPath =
      let prefix = fromMaybe Path.empty relativeTo
          suffix = maybe Path.empty Path.fromName mayName
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
  let relativeTo = fromMaybe Path.empty mayRelativeTo
  let authPath = relativeTo
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner authPath
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.runCodebaseTransaction codebase $ do
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
namespacesByNameEndpoint (AuthN.MaybeAuthedUserID callerUserId) userHandle (fromMaybe Path.Empty -> path) renderWidth rootHash = do
  whenJust rootHash $ \ch -> respondError (InvalidParam "rootHash" (into @Text ch) "Specifying a rootHash is not supported within loose-code")
  codebaseOwner@(User {user_id = codebaseOwnerUserId}) <- PGO.expectUserByHandle userHandle
  let codebaseLoc = Codebase.codebaseLocationForUserCodebase codebaseOwnerUserId
  authZReceipt <- AuthZ.permissionGuard $ AuthZ.checkReadUserCodebase callerUserId codebaseOwner path
  let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
  rt <- Codebase.codebaseRuntime codebase
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "namespaces-by-name" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransactionOrRespondError codebase $ do
      ND.namespaceDetails rt path rootCausalId renderWidth `whenNothingM` throwError (EntityMissing (ErrorID "no-namespace") $ "No namespace found at " <> Path.toText path)
  where
    cacheParams = [tShow path]

getUserProfileEndpoint :: UserHandle -> WebApp DescribeUserProfile
getUserProfileEndpoint userHandle = do
  UserProfile {user_name, avatar_url, bio, website, location, twitterHandle, pronouns} <- PG.runTransactionOrRespondError do
    User {user_id} <- Q.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
    UsersQ.userProfileById user_id `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
  pure $
    DescribeUserProfile
      { handle = userHandle,
        name = user_name,
        avatarUrl = avatar_url,
        bio = bio,
        website = website,
        location = location,
        twitterHandle = twitterHandle,
        pronouns = pronouns
      }

updateUserEndpoint :: UserHandle -> UserId -> UpdateUserRequest -> WebApp DescribeUserProfile
updateUserEndpoint userHandle callerUserId (UpdateUserRequest {name, avatarUrl, bio, website, location, twitterHandle, pronouns}) = do
  User {user_id = toUpdateUserId} <- PG.runTransactionOrRespondError $ do
    Q.userByHandle userHandle `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
  _authReceipt <- AuthZ.permissionGuard $ AuthZ.checkUserUpdate callerUserId toUpdateUserId
  UserProfile {user_name, avatar_url, bio, website, location, twitterHandle, pronouns} <- PG.runTransactionOrRespondError $ do
    UsersQ.updateUser toUpdateUserId name avatarUrl bio website location twitterHandle pronouns
    UsersQ.userProfileById toUpdateUserId `whenNothingM` throwError (EntityMissing (ErrorID "no-user-for-handle") $ "User not found for handle: " <> IDs.toText userHandle)
  pure $
    DescribeUserProfile
      { handle = userHandle,
        name = user_name,
        avatarUrl = avatar_url,
        bio = bio,
        website = website,
        location = location,
        twitterHandle = twitterHandle,
        pronouns = pronouns
      }

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
  (rootCausalId, _rootCausalHash) <- Codebase.runCodebaseTransaction codebase Codebase.expectLooseCodeRoot
  Codebase.cachedCodebaseResponse authZReceipt codebaseLoc "get-user-readme" cacheParams rootCausalId $ do
    Codebase.runCodebaseTransaction codebase $ do
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
searchEndpoint (MaybeAuthedUserID callerUserId) query (fromMaybe (Limit 20) -> limit) = do
  -- We don't have a great way to order users and projects together, so we just limit to a max
  -- of 5 users (who match the query as a prefix), then return the rest of the results from
  -- projects.
  (users, projects) <- PG.runTransaction $ do
    users <- Q.searchUsersByNameOrHandlePrefix query (Limit 5)
    projects <- Q.searchProjectsByUserQuery callerUserId query limit
    pure (users, projects)
  let userResults =
        users
          <&> \User {user_name, avatar_url, handle} ->
            SearchResultUser handle user_name avatar_url
  let projectResults =
        projects
          <&> \(Project {slug, summary, visibility}, ownerHandle) ->
            let projectShortHand = IDs.ProjectShortHand {userHandle = ownerHandle, projectSlug = slug}
             in SearchResultProject projectShortHand summary visibility
  pure $ userResults <> projectResults

accountInfoEndpoint :: Session -> WebApp UserAccountInfo
accountInfoEndpoint Session {sessionUserId} = do
  User {user_name, avatar_url, user_email, handle, user_id} <- PGO.expectUserById sessionUserId
  (completedTours, organizationMemberships) <- PG.runTransaction $ do
    tours <- Q.getCompletedToursForUser user_id
    memberships <- Q.organizationMemberships user_id
    pure (tours, memberships)
  pure $
    UserAccountInfo
      { handle = handle,
        name = user_name,
        avatarUrl = avatar_url,
        userId = user_id,
        primaryEmail = user_email,
        completedTours,
        organizationMemberships
      }

completeToursEndpoint :: Session -> NonEmpty TourId -> WebApp NoContent
completeToursEndpoint Session {sessionUserId} flows = do
  PG.runTransaction $ Q.completeToursForUser sessionUserId flows
  pure NoContent
