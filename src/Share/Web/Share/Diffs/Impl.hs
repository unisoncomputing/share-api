module Share.Web.Share.Diffs.Impl
  ( diffCausals,
    diffTerms,
    diffTypes,
  )
where

import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Aeson (ToJSON (..), Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (object)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Share.App (AppM)
import Share.Codebase qualified as Codebase
import Share.NamespaceDiffs (DefinitionDiff (..), DefinitionDiffKind (..), DiffAtPath (..), NamespaceDiffError (..), NamespaceTreeDiff)
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.NamespaceDiffs2 qualified as NamespaceDiffs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Aeson (PreEncoded (PreEncoded))
import Share.Web.Authorization (AuthZReceipt)
import Share.Web.Errors
import U.Codebase.Reference qualified as V2Reference
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions (referent1to2)
import Unison.Merge (TwoOrThreeWay (..), TwoWay (..))
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Referent qualified as Referent
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.NameSearch.Postgres qualified as PGNameSearch
import Unison.Server.Share.Definitions qualified as Definitions
import Unison.Server.Types
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)
import UnliftIO qualified

diffCausals ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, CausalId) ->
  (Codebase.CodebaseEnv, CausalId) ->
  Maybe CausalId ->
  ExceptT
    NamespaceDiffs.NamespaceDiffError
    (AppM r)
    ( PreEncoded
        ( NamespaceDiffs.NamespaceTreeDiff
            (TermTag, ShortHash)
            (TypeTag, ShortHash)
            TermDefinition
            TypeDefinition
            TermDefinitionDiff
            TypeDefinitionDiff
        )
    )
diffCausals !authZReceipt (oldCodebase, oldCausalId) (newCodebase, newCausalId) maybeLcaCausalId = do
  -- Ensure name lookups for the things we're diffing.
  -- We do this in separate transactions to ensure we can still make progress even if we need to build name lookups.
  let getBranch :: CausalId -> ExceptT NamespaceDiffs.NamespaceDiffError (AppM r) (BranchHashId, NameLookupReceipt)
      getBranch causalId =
        PG.runTransaction do
          branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
          nameLookupReceipt <- NLOps.ensureNameLookupForBranchId branchHashId
          pure (branchHashId, nameLookupReceipt)
  (((oldBranchHashId, oldBranchNLReceipt), (newBranchHashId, newBranchNLReceipt))) <-
    getBranch oldCausalId `concurrentExceptT` getBranch newCausalId
  PG.runTransaction (ContributionQ.getPrecomputedNamespaceDiff (oldCodebase, oldBranchHashId) (newCodebase, newBranchHashId)) >>= \case
    Just diff -> pure $ PreEncoded $ TL.encodeUtf8 $ TL.fromStrict diff
    Nothing -> do
      (maybeLcaBranchHashId, maybeLcaBranchNLReceipt) <-
        case maybeLcaCausalId of
          Just lcaCausalId -> do
            (lcaBranchHashId, lcaBranchNLReceipt) <- getBranch lcaCausalId
            pure (Just lcaBranchHashId, Just lcaBranchNLReceipt)
          Nothing -> pure (Nothing, Nothing)
      diffWithTags <-
        ExceptT do
          PG.tryRunTransaction do
            diff <-
              NamespaceDiffs.computeThreeWayNamespaceDiff
                TwoWay {alice = oldCodebase, bob = newCodebase}
                TwoOrThreeWay {alice = oldBranchHashId, bob = newBranchHashId, lca = maybeLcaBranchHashId}
                TwoOrThreeWay {alice = oldBranchNLReceipt, bob = newBranchNLReceipt, lca = maybeLcaBranchNLReceipt}
            withTermTags <-
              diff
                & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferents_
                  %%~ \refs -> do
                    termTags <- Codebase.termTagsByReferentsOf (\f -> traverse (f . referent1to2)) refs
                    pure $ zip termTags (refs <&> Referent.toShortHash)
            withTermTags
              & unsafePartsOf NamespaceDiffs.namespaceTreeDiffReferences_
                %%~ \refs -> do
                  typeTags <- Codebase.typeTagsByReferencesOf traversed refs
                  pure $ zip typeTags (refs <&> V2Reference.toShortHash)
      diff <- computeUpdatedDefinitionDiffs authZReceipt (oldCodebase, oldBranchHashId) (newCodebase, newBranchHashId) diffWithTags
      let encoded = Aeson.encode . RenderedNamespaceDiff $ diff
      PG.runTransaction $ ContributionQ.savePrecomputedNamespaceDiff (oldCodebase, oldBranchHashId) (newCodebase, newBranchHashId) (TL.toStrict $ TL.decodeUtf8 encoded)
      pure $ PreEncoded encoded

computeUpdatedDefinitionDiffs ::
  (Ord a, Ord b) =>
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId) ->
  (Codebase.CodebaseEnv, BranchHashId) ->
  (NamespaceDiffs.NamespaceTreeDiff a b Name Name Name Name) ->
  ExceptT NamespaceDiffError (AppM r) (NamespaceDiffs.NamespaceTreeDiff a b TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs !authZReceipt (fromCodebase, fromBHId) (toCodebase, toBHId) diff = do
  withTermDiffs <-
    diff
      & NamespaceDiffs.namespaceTreeDiffTermDiffs_
        %%~ ( \name ->
                diffTerms authZReceipt (fromCodebase, fromBHId, name) (toCodebase, toBHId, name)
            )
      >>= NamespaceDiffs.namespaceTreeTermDiffKinds_ %%~ renderDiffKind getTermDefinition
  withTermDiffs
    & NamespaceDiffs.namespaceTreeDiffTypeDiffs_
      %%~ ( \name ->
              diffTypes authZReceipt (fromCodebase, fromBHId, name) (toCodebase, toBHId, name)
          )
    >>= NamespaceDiffs.namespaceTreeTypeDiffKinds_ %%~ renderDiffKind getTypeDefinition
  where
    notFound name t = MissingEntityError $ EntityMissing (ErrorID "definition-not-found") (t <> ": Definition not found: " <> Name.toText name)
    renderDiffKind getter = \case
      Added r name -> Added r <$> (lift (getter (toCodebase, toBHId, name)) `whenNothingM` throwError (notFound name "Added"))
      NewAlias r existingNames name -> NewAlias r existingNames <$> (lift (getter (toCodebase, toBHId, name)) `whenNothingM` throwError (notFound name "NewAlias"))
      Removed r name -> Removed r <$> (lift (getter (fromCodebase, fromBHId, name)) `whenNothingM` throwError (notFound name "Removed"))
      Updated oldRef newRef diff -> pure $ Updated oldRef newRef diff
      Propagated oldRef newRef diff -> pure $ Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names <$> (lift (getter (fromCodebase, fromBHId, name)) `whenNothingM` throwError (notFound name "RenamedTo"))
      RenamedFrom r names name -> RenamedFrom r names <$> (lift (getter (toCodebase, toBHId, name)) `whenNothingM` throwError (notFound name "RenamedFrom"))

diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  ExceptT NamespaceDiffError (AppM r) TermDefinitionDiff
diffTerms !_authZReceipt old@(_, _, oldName) new@(_, _, newName) = do
  let getOldTerm = lift (getTermDefinition old) `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
  let getNewTerm = lift (getTermDefinition new) `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
  (oldTerm, newTerm) <- getOldTerm `concurrentExceptT` getNewTerm
  let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
  pure $ TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject}

getTermDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> AppM r (Maybe TermDefinition)
getTermDefinition (codebase, bhId, name) = do
  let perspective = Path.empty
  (namesPerspective, Identity relocatedName) <- PG.runTransactionMode PG.ReadCommitted PG.Read $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  rt <- Codebase.codebaseRuntime codebase
  Codebase.runCodebaseTransactionMode PG.ReadCommitted codebase do
    Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, BranchHashId, Name) ->
  ExceptT NamespaceDiffError (AppM r) TypeDefinitionDiff
diffTypes !_authZReceipt old@(_, _, oldTypeName) new@(_, _, newTypeName) = do
  let getOldType =
        lift (getTypeDefinition old)
          `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  let getNewType =
        lift (getTypeDefinition new)
          `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  (sourceType, newType) <- getOldType `concurrentExceptT` getNewType
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition sourceType) (typeDefinition newType)
  pure $ TypeDefinitionDiff {left = sourceType, right = newType, diff = typeDiffDisplayObject}

getTypeDefinition :: (Codebase.CodebaseEnv, BranchHashId, Name) -> AppM r (Maybe TypeDefinition)
getTypeDefinition (codebase, bhId, name) = do
  let perspective = Path.empty
  (namesPerspective, Identity relocatedName) <- PG.runTransactionMode PG.ReadCommitted PG.Read $ NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  rt <- Codebase.codebaseRuntime codebase
  Codebase.runCodebaseTransactionMode PG.ReadCommitted codebase do
    Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

newtype RenderedNamespaceDiff = RenderedNamespaceDiff (NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)

instance ToJSON RenderedNamespaceDiff where
  toJSON (RenderedNamespaceDiff diffs) = namespaceTreeDiffJSON diffs
    where
      text :: Text -> Text
      text t = t
      hqNameJSON :: Name -> NameSegment -> ShortHash -> Value -> Value
      hqNameJSON fqn name sh rendered = object ["hash" .= sh, "shortName" .= name, "fullName" .= fqn, "rendered" .= rendered]
      -- The preferred frontend format is a bit clunky to calculate here:
      diffDataJSON :: (ToJSON tag) => NameSegment -> DefinitionDiff (tag, ShortHash) Value Value -> (tag, Value)
      diffDataJSON shortName (DefinitionDiff {fqn, kind}) = case kind of
        Added (defnTag, r) rendered -> (defnTag, object ["tag" .= text "Added", "contents" .= hqNameJSON fqn shortName r rendered])
        NewAlias (defnTag, r) existingNames rendered ->
          let contents = object ["hash" .= r, "aliasShortName" .= shortName, "aliasFullName" .= fqn, "otherNames" .= toList existingNames, "rendered" .= rendered]
           in (defnTag, object ["tag" .= text "Aliased", "contents" .= contents])
        Removed (defnTag, r) rendered -> (defnTag, object ["tag" .= text "Removed", "contents" .= hqNameJSON fqn shortName r rendered])
        Updated (oldTag, oldRef) (newTag, newRef) diffVal ->
          let contents = object ["oldHash" .= oldRef, "newHash" .= newRef, "shortName" .= shortName, "fullName" .= fqn, "oldTag" .= oldTag, "newTag" .= newTag, "diff" .= diffVal]
           in (newTag, object ["tag" .= text "Updated", "contents" .= contents])
        Propagated (oldTag, oldRef) (newTag, newRef) diffVal ->
          let contents = object ["oldHash" .= oldRef, "newHash" .= newRef, "shortName" .= shortName, "fullName" .= fqn, "oldTag" .= oldTag, "newTag" .= newTag, "diff" .= diffVal]
           in (newTag, object ["tag" .= text "Propagated", "contents" .= contents])
        RenamedTo (defnTag, r) newNames rendered ->
          let contents = object ["oldShortName" .= shortName, "oldFullName" .= fqn, "newNames" .= newNames, "hash" .= r, "rendered" .= rendered]
           in (defnTag, object ["tag" .= text "RenamedTo", "contents" .= contents])
        RenamedFrom (defnTag, r) oldNames rendered ->
          let contents = object ["oldNames" .= oldNames, "newShortName" .= shortName, "newFullName" .= fqn, "hash" .= r, "rendered" .= rendered]
           in (defnTag, object ["tag" .= text "RenamedFrom", "contents" .= contents])
      displayObjectDiffToJSON :: DisplayObjectDiff -> Value
      displayObjectDiffToJSON = \case
        DisplayObjectDiff dispDiff ->
          object ["diff" .= dispDiff, "diffKind" .= ("diff" :: Text)]
        MismatchedDisplayObjects {} ->
          object ["diffKind" .= ("mismatched" :: Text)]

      termDefinitionDiffToJSON :: TermDefinitionDiff -> Value
      termDefinitionDiffToJSON (TermDefinitionDiff {left, right, diff}) = object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]

      typeDefinitionDiffToJSON :: TypeDefinitionDiff -> Value
      typeDefinitionDiffToJSON (TypeDefinitionDiff {left, right, diff}) = object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]
      namespaceTreeDiffJSON :: NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff -> Value
      namespaceTreeDiffJSON (diffs Cofree.:< children) =
        let changesJSON =
              diffs
                & Map.toList
                & foldMap
                  ( \(name, DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) ->
                      ( Foldable.toList termDiffsAtPath
                          <&> over NamespaceDiffs.definitionDiffDiffs_ termDefinitionDiffToJSON
                          <&> over NamespaceDiffs.definitionDiffRendered_ toJSON
                          & fmap (diffDataJSON name)
                          & fmap (\(tag, dJSON) -> object ["tag" .= tag, "contents" .= dJSON])
                      )
                        <> ( Foldable.toList typeDiffsAtPath
                               <&> over NamespaceDiffs.definitionDiffDiffs_ typeDefinitionDiffToJSON
                               <&> over NamespaceDiffs.definitionDiffRendered_ toJSON
                               & fmap (diffDataJSON name)
                               & fmap (\(tag, dJSON) -> object ["tag" .= tag, "contents" .= dJSON])
                           )
                  )
                & toJSON @([Value])
            childrenJSON =
              children
                & Map.toList
                & fmap
                  ( \(path, childNode) ->
                      object ["path" .= path, "contents" .= namespaceTreeDiffJSON childNode]
                  )
         in object
              [ "changes" .= changesJSON,
                "children" .= childrenJSON
              ]

concurrentExceptT :: (MonadUnliftIO m) => ExceptT e m a -> ExceptT e m b -> ExceptT e m (a, b)
concurrentExceptT a b = do
  (ea, eb) <- lift $ UnliftIO.concurrently (runExceptT a) (runExceptT b)
  ra <- except ea
  rb <- except eb
  pure (ra, rb)
