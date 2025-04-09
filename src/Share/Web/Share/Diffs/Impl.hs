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
import Share.NamespaceDiffs (DefinitionDiff (..), DefinitionDiffKind (..), DiffAtPath (..), GNamespaceAndLibdepsDiff, GNamespaceTreeDiff, NamespaceAndLibdepsDiff, NamespaceDiffError (..), NamespaceTreeDiff)
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHash, BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Aeson (PreEncoded (PreEncoded))
import Share.Web.Authorization (AuthZReceipt)
import Share.Web.Errors
import U.Codebase.Reference qualified as V2Reference
import Unison.Codebase.SqliteCodebase.Conversions (referent1to2)
import Unison.ConstructorReference (ConstructorReference)
import Unison.Merge (DiffOp (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
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
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, CausalId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, CausalId) ->
  Maybe CausalId ->
  ExceptT
    NamespaceDiffs.NamespaceDiffError
    (AppM r)
    ( PreEncoded
        ( NamespaceDiffs.NamespaceAndLibdepsDiff
            (TermTag, ShortHash)
            (TypeTag, ShortHash)
            TermDefinition
            TypeDefinition
            TermDefinitionDiff
            TypeDefinitionDiff
            BranchHash
        )
    )
diffCausals !authZReceipt (oldCodebase, oldRuntime, oldCausalId) (newCodebase, newRuntime, newCausalId) maybeLcaCausalId = do
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
  PG.runTransaction (ContributionQ.getPrecomputedNamespaceDiff (oldCodebase, oldCausalId) (newCodebase, newCausalId)) >>= \case
    Just diff -> pure $ PreEncoded $ TL.encodeUtf8 $ TL.fromStrict diff
    Nothing -> do
      (maybeLcaBranchHashId, maybeLcaBranchNLReceipt) <-
        case maybeLcaCausalId of
          Just lcaCausalId -> do
            (lcaBranchHashId, lcaBranchNLReceipt) <- getBranch lcaCausalId
            pure (Just lcaBranchHashId, Just lcaBranchNLReceipt)
          Nothing -> pure (Nothing, Nothing)
      diff0 <-
        ExceptT do
          PG.tryRunTransaction do
            -- Do the initial 3-way namespace diff
            diff ::
              GNamespaceAndLibdepsDiff
                NameSegment
                Referent
                TypeReference
                Name
                Name
                Name
                Name
                BranchHashId <-
              NamespaceDiffs.computeThreeWayNamespaceDiff
                TwoWay {alice = oldCodebase, bob = newCodebase}
                TwoOrThreeWay {alice = oldBranchHashId, bob = newBranchHashId, lca = maybeLcaBranchHashId}
                TwoOrThreeWay {alice = oldBranchNLReceipt, bob = newBranchNLReceipt, lca = maybeLcaBranchNLReceipt}
            -- Resolve the term referents to tag + hash
            diff1 ::
              GNamespaceAndLibdepsDiff
                NameSegment
                (TermTag, ShortHash)
                TypeReference
                Name
                Name
                Name
                Name
                BranchHashId <-
              diff
                & unsafePartsOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferents_)
                  %%~ \refs -> do
                    termTags <- Codebase.termTagsByReferentsOf (\f -> traverse (f . referent1to2)) refs
                    pure $ zip termTags (refs <&> Referent.toShortHash)
            -- Resolve the type references to tag + hash
            diff2 ::
              GNamespaceAndLibdepsDiff
                NameSegment
                (TermTag, ShortHash)
                (TypeTag, ShortHash)
                Name
                Name
                Name
                Name
                BranchHashId <-
              diff1
                & unsafePartsOf (NamespaceDiffs.namespaceAndLibdepsDiffDefns_ . NamespaceDiffs.namespaceTreeDiffReferences_)
                  %%~ \refs -> do
                    typeTags <- Codebase.typeTagsByReferencesOf traversed refs
                    pure $ zip typeTags (refs <&> V2Reference.toShortHash)
            -- Resolve libdeps branch hash ids to branch hashes
            diff3 ::
              GNamespaceAndLibdepsDiff
                NameSegment
                (TermTag, ShortHash)
                (TypeTag, ShortHash)
                Name
                Name
                Name
                Name
                BranchHash <-
              HashQ.expectNamespaceHashesByNamespaceHashIdsOf
                (NamespaceDiffs.namespaceAndLibdepsDiffLibdeps_ . traversed . traversed)
                diff2
            pure diff3
      -- Resolve the actual term/type definitions. Use the LCA as the "old" (because that's what we're rendering the
      -- diff relative to, unless there isn't an LCA (unlikely), in which case we fall back on the other branch (we
      -- won't have anything classified as an "update" in this case so it doesn't really matter).
      diff1 <-
        ExceptT do
          PG.tryRunTransaction do
            diff0
              & NamespaceDiffs.namespaceAndLibdepsDiffDefns_
                %%~ computeUpdatedDefinitionDiffs
                  authZReceipt
                  (oldCodebase, oldRuntime, fromMaybe oldBranchHashId maybeLcaBranchHashId)
                  (newCodebase, newRuntime, newBranchHashId)
      let encoded = Aeson.encode (RenderedNamespaceAndLibdepsDiff diff1)
      PG.runTransaction $
        ContributionQ.savePrecomputedNamespaceDiff
          (oldCodebase, oldCausalId)
          (newCodebase, newCausalId)
          (TL.toStrict $ TL.decodeUtf8 encoded)
      pure $ PreEncoded encoded

computeUpdatedDefinitionDiffs ::
  forall a b.
  (Ord a, Ord b) =>
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId) ->
  GNamespaceTreeDiff NameSegment a b Name Name Name Name ->
  PG.Transaction
    NamespaceDiffError
    (NamespaceDiffs.NamespaceTreeDiff a b TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff)
computeUpdatedDefinitionDiffs !authZReceipt (fromCodebase, fromRuntime, fromBHId) (toCodebase, toRuntime, toBHId) diff0 = do
  diff1 <-
    NamespaceDiffs.witherNamespaceTreeDiffTermDiffs
      (\name -> diffTerms authZReceipt (fromCodebase, fromRuntime, fromBHId, name) (toCodebase, toRuntime, toBHId, name))
      diff0
  diff2 <-
    NamespaceDiffs.witherNamespaceTreeTermDiffKinds
      (fmap throwAwayConstructorDiffs . renderDiffKind getTermDefinition)
      diff1
  diff3 <-
    NamespaceDiffs.namespaceTreeDiffTypeDiffs_
      (\name -> diffTypes authZReceipt (fromCodebase, fromRuntime, fromBHId, name) (toCodebase, toRuntime, toBHId, name))
      diff2
  diff4 <-
    NamespaceDiffs.namespaceTreeTypeDiffKinds_
      (renderDiffKind getTypeDefinition)
      diff3
  pure (NamespaceDiffs.compressNameTree diff4)
  where
    notFound name t = MissingEntityError $ EntityMissing (ErrorID "definition-not-found") (t <> ": Definition not found: " <> Name.toText name)

    renderDiffKind ::
      forall diff r x.
      ((Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) -> PG.Transaction NamespaceDiffError (Maybe x)) ->
      DefinitionDiffKind r Name diff ->
      PG.Transaction NamespaceDiffError (DefinitionDiffKind r x diff)
    renderDiffKind getter = \case
      Added r name -> Added r <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "Added"))
      NewAlias r existingNames name -> NewAlias r existingNames <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "NewAlias"))
      Removed r name -> Removed r <$> (getter (fromCodebase, fromRuntime, fromBHId, name) `whenNothingM` throwError (notFound name "Removed"))
      Updated oldRef newRef diff -> pure $ Updated oldRef newRef diff
      Propagated oldRef newRef diff -> pure $ Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names <$> (getter (fromCodebase, fromRuntime, fromBHId, name) `whenNothingM` throwError (notFound name "RenamedTo"))
      RenamedFrom r names name -> RenamedFrom r names <$> (getter (toCodebase, toRuntime, toBHId, name) `whenNothingM` throwError (notFound name "RenamedFrom"))

    throwAwayConstructorDiffs ::
      DefinitionDiffKind a (Either ConstructorReference TermDefinition) diff -> Maybe (DefinitionDiffKind a TermDefinition diff)
    throwAwayConstructorDiffs = \case
      Added ref (Right term) -> Just (Added ref term)
      NewAlias ref names (Right term) -> Just (NewAlias ref names term)
      Removed ref (Right term) -> Just (Removed ref term)
      Updated old new diff -> Just (Updated old new diff)
      Propagated old new diff -> Just (Propagated old new diff)
      RenamedTo ref names (Right term) -> Just (RenamedTo ref names term)
      RenamedFrom ref names (Right term) -> Just (RenamedFrom ref names term)
      --
      Added _ (Left _) -> Nothing
      NewAlias _ _ (Left _) -> Nothing
      Removed _ (Left _) -> Nothing
      RenamedFrom _ _ (Left _) -> Nothing
      RenamedTo _ _ (Left _) -> Nothing

diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) ->
  PG.Transaction NamespaceDiffError (Maybe TermDefinitionDiff)
diffTerms !_authZReceipt old@(_, _, _, oldName) new@(_, _, _, newName) = do
  oldTerm <- getTermDefinition old `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'From' term not found: " <> Name.toText oldName))
  newTerm <- getTermDefinition new `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("'To' term not found: " <> Name.toText newName))
  case (oldTerm, newTerm) of
    (Right oldTerm, Right newTerm) -> do
      let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
      pure (Just TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject})
    -- For later: decide how to render a constructor-to-term or constructor-to-constructor diff
    -- Just dropping them from the diff for now
    _ -> pure Nothing

getTermDefinition :: (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) -> PG.Transaction e (Maybe (Either ConstructorReference TermDefinition))
getTermDefinition (codebase, rt, bhId, name) = do
  let perspective = mempty
  (namesPerspective, Identity relocatedName) <- NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = PPED.biasTo [name] <$> PPEPostgres.ppedForReferences namesPerspective deps
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Codebase.codebaseMToTransaction codebase do
    Definitions.termDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) ->
  PG.Transaction NamespaceDiffError TypeDefinitionDiff
diffTypes !_authZReceipt old@(_, _, _, oldTypeName) new@(_, _, _, newTypeName) = do
  oldType <-
    getTypeDefinition old
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  newType <-
    getTypeDefinition new
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition oldType) (typeDefinition newType)
  pure $ TypeDefinitionDiff {left = oldType, right = newType, diff = typeDiffDisplayObject}

getTypeDefinition :: (Codebase.CodebaseEnv, Codebase.CodebaseRuntime IO, BranchHashId, Name) -> PG.Transaction e (Maybe TypeDefinition)
getTypeDefinition (codebase, rt, bhId, name) = do
  let perspective = mempty
  (namesPerspective, Identity relocatedName) <- NameLookupOps.relocateToNameRoot perspective (Identity name) bhId
  let ppedBuilder deps = (PPED.biasTo [name]) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  Codebase.codebaseMToTransaction codebase do
    Definitions.typeDefinitionByName ppedBuilder nameSearch renderWidth rt relocatedName
  where
    renderWidth :: Width
    renderWidth = 80

newtype RenderedNamespaceAndLibdepsDiff
  = RenderedNamespaceAndLibdepsDiff
      (NamespaceAndLibdepsDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff BranchHash)

instance ToJSON RenderedNamespaceAndLibdepsDiff where
  toJSON (RenderedNamespaceAndLibdepsDiff diff) =
    object
      [ "defns" .= namespaceTreeDiffJSON diff.defns,
        "libdeps" .= libdepsDiffJSON diff.libdeps
      ]
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

      namespaceTreeDiffJSON ::
        NamespaceTreeDiff
          (TermTag, ShortHash)
          (TypeTag, ShortHash)
          TermDefinition
          TypeDefinition
          TermDefinitionDiff
          TypeDefinitionDiff ->
        Value
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
                & toJSON @[Value]
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

      libdepsDiffJSON :: Map NameSegment (DiffOp BranchHash) -> Value
      libdepsDiffJSON =
        Map.toList
          >>> map
            ( \(name, op) ->
                case op of
                  DiffOp'Add hash ->
                    object
                      [ "hash" .= hash,
                        "name" .= name,
                        "tag" .= ("Added" :: Text)
                      ]
                  DiffOp'Delete hash ->
                    object
                      [ "hash" .= hash,
                        "name" .= name,
                        "tag" .= ("Removed" :: Text)
                      ]
                  DiffOp'Update Merge.Updated {old, new} ->
                    object
                      [ "name" .= name,
                        "newHash" .= new,
                        "oldHash" .= old,
                        "tag" .= ("Updated" :: Text)
                      ]
            )
          >>> toJSON @[Value]

concurrentExceptT :: (MonadUnliftIO m) => ExceptT e m a -> ExceptT e m b -> ExceptT e m (a, b)
concurrentExceptT a b = do
  (ea, eb) <- lift $ UnliftIO.concurrently (runExceptT a) (runExceptT b)
  ra <- except ea
  rb <- except eb
  pure (ra, rb)
