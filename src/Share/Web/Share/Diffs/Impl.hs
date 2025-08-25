module Share.Web.Share.Diffs.Impl
  ( computeAndStoreCausalDiff,
    diffTerms,
    diffTypes,
  )
where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Share.Codebase qualified as Codebase
import Share.NamespaceDiffs (DefinitionDiffKind (..), NamespaceDiffError (..))
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.NamespaceDiffs.Types (GNamespaceTreeDiff)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Contributions.Queries qualified as ContributionQ
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHash, BranchHashId, CausalId)
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NamesPerspective.Ops qualified as NLOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective (..))
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Aeson (PreEncoded (PreEncoded))
import Share.Web.Authorization (AuthZReceipt)
import Share.Web.Errors
import U.Codebase.Reference qualified as V2Reference
import Unison.Codebase.SqliteCodebase.Conversions (referent1to2)
import Unison.ConstructorReference (ConstructorReference)
import Unison.Merge (TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.DiffOp qualified as DiffOp
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.Share.Definitions qualified as Definitions
import Unison.Server.Types
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.UnconflictedLocalDefnsView qualified
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns qualified
import Unison.Util.Pretty (Width)

-- | Diff two causals and store the diff in the database.
computeAndStoreCausalDiff ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, CausalId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, CausalId) ->
  Maybe CausalId ->
  PG.Transaction e (PreEncoded NamespaceDiffs.NamespaceDiffResult)
computeAndStoreCausalDiff authZReceipt old@(oldCodebase, _, oldCausalId) new@(newCodebase, _, newCausalId) lca = PG.transactionSpan "computeAndStoreCausalDiff" mempty do
  result <-
    PG.catchTransaction (tryComputeCausalDiff authZReceipt old new lca) <&> \case
      Right diff -> NamespaceDiffs.NamespaceDiffResult'Ok diff
      Left err -> NamespaceDiffs.NamespaceDiffResult'Err err
  let encoded = Aeson.encode result
  PG.transactionSpan "savePrecomputedNamespaceDiff" mempty do
    ContributionQ.savePrecomputedNamespaceDiff
      (oldCodebase, oldCausalId)
      (newCodebase, newCausalId)
      (TL.toStrict $ TL.decodeUtf8 encoded)
  pure (PreEncoded encoded)

tryComputeCausalDiff ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime from IO, CausalId) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime to IO, CausalId) ->
  Maybe CausalId ->
  PG.Transaction
    NamespaceDiffError
    ( NamespaceDiffs.NamespaceAndLibdepsDiff
        (TermTag, ShortHash)
        (TypeTag, ShortHash)
        TermDefinition
        TypeDefinition
        TermDefinitionDiff
        TypeDefinitionDiff
        BranchHash
    )
tryComputeCausalDiff !_authZReceipt (oldCodebase, oldRuntime, oldCausalId) (newCodebase, newRuntime, newCausalId) maybeLcaCausalId = PG.transactionSpan "tryComputeCausalDiff" mempty do
  -- Ensure name lookups for the things we're diffing.
  let getBranch :: CausalId -> PG.Transaction NamespaceDiffError (BranchHashId, NameLookupReceipt, NamesPerspective (PG.Transaction NamespaceDiffError))
      getBranch causalId = do
        branchHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id causalId
        np@NamesPerspective {nameLookupReceipt} <- NLOps.namesPerspectiveForRoot branchHashId
        pure (branchHashId, nameLookupReceipt, np)
  (oldBranchHashId, oldBranchNLReceipt, oldPerspective) <- PG.transactionSpan "getOldBranch" mempty $ getBranch oldCausalId
  (newBranchHashId, newBranchNLReceipt, newPerspective) <- PG.transactionSpan "getNewBranch" mempty $ getBranch newCausalId
  (maybeLcaBranchHashId, maybeLcaBranchNLReceipt, maybeLcaPerspective) <-
    case maybeLcaCausalId of
      Just lcaCausalId -> do
        (lcaBranchHashId, lcaBranchNLReceipt, lcaPerspective) <- PG.transactionSpan "getNewBranch" mempty $ getBranch lcaCausalId
        pure (Just lcaBranchHashId, Just lcaBranchNLReceipt, Just lcaPerspective)
      Nothing -> pure (Nothing, Nothing, Nothing)

  -- Do the initial 3-way namespace diff
  diffblob <-
    NamespaceDiffs.computeThreeWayNamespaceDiff
      TwoWay {alice = oldCodebase, bob = newCodebase}
      oldRuntime.codeCache
      newRuntime.codeCache
      TwoOrThreeWay {alice = oldBranchHashId, bob = newBranchHashId, lca = maybeLcaBranchHashId}
      TwoOrThreeWay {alice = oldBranchNLReceipt, bob = newBranchNLReceipt, lca = maybeLcaBranchNLReceipt}

  -- Boilerplate conversion: make a "DefinitionDiffs" from the info in a "Mergeblob1".
  --
  -- We start focusing only on Bob here, the contributor, even though Alice could have a diff as well of course (since
  -- the LCA is arbitrarily behind Alice).
  let defns0 :: GNamespaceTreeDiff NameSegment Referent TypeReference Name Name Name Name
      defns0 =
        NamespaceDiffs.makeNamespaceDiffTree
          diffblob.defns.lca.defns
          diffblob.diffsFromLCA.bob
          diffblob.propagatedUpdates.bob
          diffblob.simpleRenames.bob

  let oldPerspective1 =
        fromMaybe oldPerspective maybeLcaPerspective

  let termNamesToRender =
        defns0 ^.. NamespaceDiffs.namespaceTreeTermDiffKinds_ . to partitionDiffKind . NamespaceDiffs.definitionDiffKindRendered_

  let (oldTermNamesToRender, newTermNamesToRender) =
        bimap Set.fromList Set.fromList (partitionEithers termNamesToRender)

  let termNamesToDiff =
        setOf NamespaceDiffs.namespaceTreeDiffTermDiffs_ defns0

  let typeNamesToRender =
        defns0 ^.. NamespaceDiffs.namespaceTreeTypeDiffKinds_ . to partitionDiffKind . NamespaceDiffs.definitionDiffKindRendered_

  let (oldTypeNamesToRender, newTypeNamesToRender) =
        bimap Set.fromList Set.fromList (partitionEithers typeNamesToRender)

  let typeNamesToDiff =
        setOf NamespaceDiffs.namespaceTreeDiffTypeDiffs_ defns0

  let newNameToReferent :: Name -> Referent
      newNameToReferent =
        (BiMultimap.range diffblob.defns.bob.defns.terms Map.!)

  let oldNameToReferent :: Name -> Referent
      oldNameToReferent =
        case maybeLcaPerspective of
          Just _ -> (BiMultimap.range diffblob.defns.lca.defns.terms Map.!)
          Nothing -> newNameToReferent

  getOldTermDefinitionByName <- do
    oldTermDefinitionsByName <-
      PG.transactionSpan "load old terms" mempty do
        deriveMapOf
          (expectTermDefinitionsByNamedRefsOf oldCodebase oldRuntime oldPerspective1 oldNameToReferent)
          (Set.toList (Set.union oldTermNamesToRender termNamesToDiff))
    pure (oldTermDefinitionsByName Map.!)

  getNewTermDefinitionByName <- do
    newTermDefinitionsByName <- do
      PG.transactionSpan "load new terms" mempty do
        deriveMapOf
          (expectTermDefinitionsByNamedRefsOf newCodebase newRuntime newPerspective newNameToReferent)
          (Set.toList (Set.union newTermNamesToRender termNamesToDiff))
    pure (newTermDefinitionsByName Map.!)

  getOldTypeDefinitionByName <- do
    oldTypeDefinitionsByName <- do
      PG.transactionSpan "load old types" mempty do
        deriveMapOf
          (expectTypeDefinitionsOf oldCodebase oldRuntime oldPerspective1)
          (Set.toList (Set.union oldTypeNamesToRender typeNamesToDiff))
    pure (oldTypeDefinitionsByName Map.!)

  getNewTypeDefinitionByName <- do
    newTypeDefinitionsByName <- do
      PG.transactionSpan "load new types" mempty do
        deriveMapOf
          (expectTypeDefinitionsOf newCodebase newRuntime newPerspective)
          (Set.toList (Set.union newTypeNamesToRender typeNamesToDiff))
    pure (newTypeDefinitionsByName Map.!)

  -- Resolve the term referents to tag + hash
  defns1 :: NamespaceDiffs.GNamespaceTreeDiff NameSegment (TermTag, ShortHash) TypeReference Name Name Name Name <-
    PG.transactionSpan "load term tags" mempty do
      defns0
        & asListOf NamespaceDiffs.namespaceTreeDiffReferents_ %%~ \refs -> do
          termTags <- Codebase.termTagsByReferentsOf traversed (referent1to2 <$> refs)
          pure $ zip termTags (refs <&> Referent.toShortHash)

  -- Resolve the type references to tag + hash
  defns2 :: NamespaceDiffs.GNamespaceTreeDiff NameSegment (TermTag, ShortHash) (TypeTag, ShortHash) Name Name Name Name <-
    PG.transactionSpan "load type tags" mempty do
      defns1
        & asListOf NamespaceDiffs.namespaceTreeDiffReferences_ %%~ \refs -> do
          typeTags <- Codebase.typeTagsByReferencesOf traversed refs
          pure $ zip typeTags (refs <&> V2Reference.toShortHash)

  -- Resolve the actual term/type definitions. Use the LCA as the "old" (because that's what we're rendering the diff
  -- relative to, unless there isn't an LCA (unlikely), in which case we fall back on the other branch (we won't have
  -- anything classified as an "update" in this case so it doesn't really matter).

  let defns3 :: GNamespaceTreeDiff NameSegment (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff
      defns3 =
        defns2
          & NamespaceDiffs.mapMaybeNamespaceTreeDiffTermDiffs (\name -> diffTermsPure (getOldTermDefinitionByName name) (getNewTermDefinitionByName name))
          & NamespaceDiffs.mapMaybeNamespaceTreeTermDiffKinds
            ( NamespaceDiffs.definitionDiffKindRenderedOldNew_
                ( either
                    (eitherToMaybe . getOldTermDefinitionByName)
                    (eitherToMaybe . getNewTermDefinitionByName)
                )
            )
          & NamespaceDiffs.namespaceTreeDiffTypeDiffs_ %~ (\name -> diffTypesPure (getOldTypeDefinitionByName name) (getNewTypeDefinitionByName name))
          & NamespaceDiffs.namespaceTreeTypeDiffKinds_ . NamespaceDiffs.definitionDiffKindRenderedOldNew_
            %~ either getOldTypeDefinitionByName getNewTypeDefinitionByName

  -- Resolve libdeps branch hash ids to branch hashes
  libdeps <-
    PG.transactionSpan "load libdeps branch hashes" mempty do
      HashQ.expectNamespaceHashesByNamespaceHashIdsOf
        (traversed . DiffOp.traverse)
        diffblob.libdepsDiffs.bob

  pure
    NamespaceDiffs.NamespaceAndLibdepsDiff
      { defns = NamespaceDiffs.compressNameTree defns3,
        libdeps
      }
  where
    -- Splits the diff kind into the parts from the 'from' and 'to' sections of a diff.
    --
    -- Left = From, Right = To
    partitionDiffKind :: DefinitionDiffKind r rendered diff -> DefinitionDiffKind r (Either rendered rendered) diff
    partitionDiffKind = \case
      Added r name -> Added r (Right name)
      NewAlias r existingNames name -> NewAlias r existingNames (Right name)
      Removed r name -> Removed r (Left name)
      Updated oldRef newRef diff -> Updated oldRef newRef diff
      Propagated oldRef newRef diff -> Propagated oldRef newRef diff
      RenamedTo r names name -> RenamedTo r names (Left name)
      RenamedFrom r names name -> RenamedFrom r names (Right name)

-- | Note: Only use this if you're diffing a single definition, otherwise batch operations are
-- more efficient.
diffTerms ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError (Maybe TermDefinitionDiff)
diffTerms !_authZReceipt (oldCodebase, oldRt, oldNp, oldName) (newCodebase, newRt, newNp, newName) = do
  oldTerm <- expectTermDefinitionsOf oldCodebase oldRt oldNp id oldName
  newTerm <- expectTermDefinitionsOf newCodebase newRt newNp id newName
  pure $ diffTermsPure oldTerm newTerm

diffTermsPure :: Either ConstructorReference TermDefinition -> Either ConstructorReference TermDefinition -> Maybe TermDefinitionDiff
diffTermsPure (Right oldTerm) (Right newTerm) =
  let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
   in (Just TermDefinitionDiff {left = oldTerm, right = newTerm, diff = termDiffDisplayObject})
-- For later: decide how to render a constructor-to-term or constructor-to-constructor diff
-- Just dropping them from the diff for now
diffTermsPure _ _ = Nothing

-- | Get definitions for a batch of terms within a codebase and perspective.
-- NOTE: The names should already be properly scoped to the names perspective.
getTermDefinitionsOf ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective m ->
  Traversal s t Name (Maybe (Either ConstructorReference TermDefinition)) ->
  s ->
  m t
getTermDefinitionsOf codebase rt namesPerspective trav s = do
  s
    & asListOf trav %%~ \names -> do
      Definitions.termDefinitionByNamesOf codebase ppedBuilder namesPerspective renderWidth rt includeDocs traversed names
  where
    includeDocs = False
    ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
    renderWidth :: Width
    renderWidth = 80

expectTermDefinitionsOf ::
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective (PG.Transaction NamespaceDiffError) ->
  Traversal s t Name (Either ConstructorReference TermDefinition) ->
  s ->
  PG.Transaction NamespaceDiffError t
expectTermDefinitionsOf codebase rt np trav s =
  s
    & asListOf trav %%~ \names -> do
      results <- getTermDefinitionsOf codebase rt np traversed names
      for (zip names results) \case
        (name, Nothing) -> throwError (MissingEntityError $ EntityMissing (ErrorID "term-not-found") ("Term not found: " <> Name.toText name <> ", in names perspective: " <> tShow np))
        (_, Just termDef) -> pure termDef

expectTermDefinitionsByNamedRefsOf ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective m ->
  (Name -> Referent) ->
  Traversal s t Name (Either ConstructorReference TermDefinition) ->
  s ->
  m t
expectTermDefinitionsByNamedRefsOf codebase rt namesPerspective toReferent trav s = do
  s
    & asListOf trav %%~ \names ->
      Definitions.termDefinitionByNamedRefsOf
        codebase
        ppedBuilder
        namesPerspective
        renderWidth
        rt
        includeDocs
        traverse
        (map (\name -> (name, toReferent name)) names)
  where
    includeDocs = False
    ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
    renderWidth :: Width
    renderWidth = 80

diffTypes ::
  AuthZReceipt ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime old IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  (Codebase.CodebaseEnv, Codebase.CodebaseRuntime new IO, NamesPerspective (PG.Transaction NamespaceDiffError), Name) ->
  PG.Transaction NamespaceDiffError TypeDefinitionDiff
diffTypes !_authZReceipt (oldCodebase, oldRt, oldNp, oldTypeName) (newCodebase, newRt, newNp, newTypeName) = do
  oldType <-
    getTypeDefinitionsOf oldCodebase oldRt oldNp id oldTypeName
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'From' Type not found: " <> Name.toText oldTypeName))
  newType <-
    getTypeDefinitionsOf newCodebase newRt newNp id newTypeName
      `whenNothingM` throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("'To' Type not found: " <> Name.toText newTypeName))
  pure $ diffTypesPure oldType newType

diffTypesPure :: TypeDefinition -> TypeDefinition -> TypeDefinitionDiff
diffTypesPure oldType newType =
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects (typeDefinition oldType) (typeDefinition newType)
   in TypeDefinitionDiff {left = oldType, right = newType, diff = typeDiffDisplayObject}

-- | Get definitions for a batch of types within a codebase and perspective.
-- NOTE: The names should already be properly scoped to the names perspective.
getTypeDefinitionsOf ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective m ->
  Traversal s t Name (Maybe TypeDefinition) ->
  s ->
  m t
getTypeDefinitionsOf codebase rt namesPerspective trav s = do
  s
    & asListOf trav %%~ \names -> do
      Definitions.typeDefinitionsByNamesOf codebase ppedBuilder namesPerspective renderWidth rt includeDocs traversed names
  where
    includeDocs = False
    ppedBuilder deps = PPEPostgres.ppedForReferences namesPerspective deps
    renderWidth :: Width
    renderWidth = 80

expectTypeDefinitionsOf ::
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime sym IO ->
  NamesPerspective (PG.Transaction NamespaceDiffError) ->
  Traversal s t Name TypeDefinition ->
  s ->
  PG.Transaction NamespaceDiffError t
expectTypeDefinitionsOf codebase rt np trav s =
  s
    & asListOf trav %%~ \names -> do
      results <- getTypeDefinitionsOf codebase rt np traversed names
      for (zip names results) \case
        (name, Nothing) -> throwError (MissingEntityError $ EntityMissing (ErrorID "type-not-found") ("Type not found: " <> Name.toText name <> ", in names perspective: " <> tShow np))
        (_, Just typeDef) -> pure typeDef

-- Quick helper that makes a `Map k v` from an input list of `k` and a monadic `v`-fetching function
--
-- A simpler version of this might elide the traversal and instead just be:
--
--   deriveMapOf :: (Ord k, Functor m) => ([k] -> m [v]) -> [k] -> m (Map k v)
--
-- However, that'd require the caller to provide a `v`-fetching function that returns the same number of `v` as there
-- were `k` input. That's not hard to do, the traversal just does that for us (assuming it's law-abiding).
deriveMapOf :: (Ord k, Functor m) => (forall s t. Traversal s t k v -> s -> m t) -> [k] -> m (Map k v)
deriveMapOf f ks = do
  Map.fromList . zip ks <$> f traverse ks
