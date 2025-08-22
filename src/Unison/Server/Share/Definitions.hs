module Unison.Server.Share.Definitions
  ( definitionForHQName,
    termDefinitionByNamesOf,
    typeDefinitionsByNamesOf,
  )
where

import Control.Lens hiding ((??))
import Data.Bifoldable (bifoldMap)
import Data.Either (partitionEithers)
import Data.List (zip4)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Share.Backend qualified as Backend
import Share.Codebase (CodebaseEnv, CodebaseRuntime)
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodeCache, CodebaseEnv (..))
import Share.Postgres (QueryM, transactionSpan)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.NameLookups.Types (pathToPathSegments)
import Share.Postgres.NamesPerspective.Ops qualified as NPOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Utils.Caching.JSON qualified as Caching
import Share.Utils.Data qualified as Data
import Share.Utils.Lens (asListOfDeduped)
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Dependencies qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Reference qualified as V1
import Unison.Referent qualified as Referent
import Unison.Server.Doc qualified as Doc
import Unison.Server.NameSearch (NameSearch (..))
import Unison.Server.NameSearch qualified as NameSearch
import Unison.Server.NameSearch.Postgres qualified as PGNameSearch
import Unison.Server.QueryResult (QueryResult (..))
import Unison.Server.SearchResult qualified as SR
import Unison.Server.Share.Docs qualified as Docs
import Unison.Server.Types
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.List qualified as List
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (Width)

-- | Renders a definition for the given name or hash alongside its documentation.
definitionForHQName ::
  forall m sym.
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  -- | The path representing the user's current namesRoot.
  -- Searches will be limited to definitions within this path, and names will be relative to
  -- this path.
  Path ->
  -- | The root causal to use
  CausalId ->
  Maybe Width ->
  -- | Whether to suffixify bindings in the rendered syntax
  Suffixify ->
  -- | Runtime used to evaluate docs. This should be sandboxed if run on the server.
  CodebaseRuntime sym IO ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  m DefinitionDisplayResults
definitionForHQName codebase@(CodebaseEnv {codebaseOwner}) perspective rootCausalId renderWidth suffixifyBindings rt perspectiveQuery = do
  let cacheKey =
        Caching.CacheKey
          { cacheTopic = "definitionForHQName",
            key = [("perspective", Path.toText perspective), ("suffixify", tShow $ suffixified (suffixifyBindings)), ("hqName", HQ.toText perspectiveQuery), ("width", tShow renderWidth)],
            rootCausalId = Just rootCausalId,
            sandbox = Just codebaseOwner
          }
  Caching.usingJSONCache cacheKey go
  where
    go :: m DefinitionDisplayResults
    go = do
      rootBranchNamespaceHashId <- CausalQ.expectNamespaceIdsByCausalIdsOf id rootCausalId
      initialNP <- NPOps.namesPerspectiveForRootAndPath rootBranchNamespaceHashId (pathToPathSegments perspective)
      (perspectiveNP, query) <-
        NPOps.relocateNamesToMountsOf initialNP traversed perspectiveQuery
          <&> \case
            HQ.NameOnly (perspectiveNP', n) -> (perspectiveNP', HQ.NameOnly n)
            HQ.HashOnly sh -> (initialNP, HQ.HashOnly sh)
            HQ.HashQualified (perspectiveNP', n) sh ->
              (perspectiveNP', HQ.HashQualified n sh)

      -- Bias towards both relative and absolute path to queries,
      -- This allows us to still bias towards definitions outside our namesRoot but within the
      -- same tree;
      -- e.g. if the query is `map` and we're in `base.trunk.List`,
      -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
      -- `trunk` over those in other releases.
      -- ppe which returns names fully qualified to the current namesRoot,  not to the codebase root.
      let biases = maybeToList $ HQ.toName query
      let ppedBuilder deps = (PPED.biasTo biases) <$> (PPEPostgres.ppedForReferences perspectiveNP deps)
      let nameSearch = PGNameSearch.nameSearchForPerspective perspectiveNP
      dr@(Backend.DefinitionResults terms types misses) <- mkDefinitionsForQuery rt.codeCache nameSearch [query]
      let width = mayDefaultWidth renderWidth
      let docResultsOf :: forall s t. Traversal s t Name [(HashQualifiedName, UnisonHash, Doc.Doc)] -> s -> m t
          docResultsOf trav s = do
            s
              & asListOf trav %%~ \names -> do
                allDocRefs <- Docs.docsForDefinitionNamesOf codebase perspectiveNP traversed names
                -- TODO: properly batchify this
                for allDocRefs \drs -> renderDocRefs codebase ppedBuilder width rt drs

      let drDeps = Backend.definitionResultsDependencies dr
      termAndTypePPED <- ppedBuilder drDeps
      typeDefinitions <- do
        let fqnTermAndTypePPE = PPED.unsuffixifiedPPE termAndTypePPED
        let typeDisplayObjs = Backend.typesToSyntaxOf suffixifyBindings width termAndTypePPED (Map.asList_ . traversed) types
        let typeRefs = types & imap \ref _ -> ref
        let typeMayNames = typeDisplayObjs & imap \ref _ -> HQ.toName $ PPE.typeNameOrHashOnly fqnTermAndTypePPE ref
        allDocs <- fmap (fromMaybe []) <$> docResultsOf (traversed . _Just) typeMayNames
        let typeInfos = Data.zip4 typeMayNames typeRefs allDocs typeDisplayObjs
        Backend.mkTypeDefinitionsOf termAndTypePPED width traversed typeInfos

      termDefinitions <- do
        let fqnTermAndTypePPE = PPED.unsuffixifiedPPE termAndTypePPED
        let termDisplayObjs = Backend.termsToSyntaxOf suffixifyBindings width termAndTypePPED (Map.asList_ . traversed) terms
        let termRefs = terms & imap \ref _ -> ref
        let termMayNames = termDisplayObjs & imap \ref _ -> HQ.toName $ PPE.termNameOrHashOnly fqnTermAndTypePPE (Referent.Ref ref)
        allDocs <- fmap (fromMaybe []) <$> docResultsOf (traversed . _Just) termMayNames
        let termInfos = Data.zip4 termMayNames termRefs allDocs termDisplayObjs
        Backend.mkTermDefinitionsOf codebase termAndTypePPED width traversed termInfos
      let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
          renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
          renderedMisses = fmap HQ.toText misses
      pure $
        DefinitionDisplayResults
          renderedDisplayTerms
          renderedDisplayTypes
          renderedMisses

renderDocRefs ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  PPEDBuilder (m) ->
  Width ->
  CodebaseRuntime sym IO ->
  [TermReference] ->
  m [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDocRefs _codebase _ppedBuilder _width _rt [] = pure []
renderDocRefs codebase ppedBuilder width rt docRefs = transactionSpan "" mempty do
  eDocs <- for docRefs \ref -> (ref,) <$> (Backend.evalDocRef codebase rt ref)
  let docDeps = foldMap (Doc.dependencies . snd) eDocs <> Set.fromList (LD.TermReference <$> docRefs)
  docsPPED <- ppedBuilder docDeps
  for eDocs \(ref, eDoc) -> do
    let name = Backend.bestNameForTerm @Symbol (PPED.suffixifiedPPE docsPPED) width (Referent.Ref ref)
    let hash = Reference.toText ref
    let renderedDoc = Doc.renderDoc docsPPED eDoc
    pure (name, hash, renderedDoc)

type PPEDBuilder m = Set LD.LabeledDependency -> m PPED.PrettyPrintEnvDecl

-- | Mirrors Backend.definitionsBySuffixes but without doing a suffix search.
mkDefinitionsForQuery ::
  (QueryM m) =>
  CodeCache scope ->
  NameSearch m ->
  [HQ.HashQualified Name] ->
  m Backend.DefinitionResults
mkDefinitionsForQuery codeCache nameSearch query = do
  QueryResult misses results <- hqNameQuery nameSearch query
  let termRefs = Set.toList $ searchResultsToTermRefs results
  -- todo: remember to replace this with getting components directly,
  -- and maybe even remove getComponentLength from Codebase interface altogether
  displayedTerms <- Backend.displayTermsOf codeCache traversed termRefs
  let termsMap = Map.fromList (zip termRefs displayedTerms)
  let typeRefsMap = Data.mapFromSelf . Set.toList $ searchResultsToTypeRefs results
  typesMap <- Backend.displayTypesOf codeCache traversed typeRefsMap
  pure (Backend.DefinitionResults termsMap typesMap misses)
  where
    searchResultsToTermRefs :: [SR.SearchResult] -> Set V1.Reference
    searchResultsToTermRefs results =
      Set.fromList [r | SR.Tm' _ (Referent.Ref r) _ <- results]
    searchResultsToTypeRefs :: [SR.SearchResult] -> Set V1.Reference
    searchResultsToTypeRefs results =
      Set.fromList (mapMaybe f results)
      where
        f :: SR.SearchResult -> Maybe V1.Reference
        f = \case
          SR.Tm' _ (Referent.Con r _) _ -> Just (r ^. ConstructorReference.reference_)
          SR.Tp' _ r _ -> Just r
          _ -> Nothing

-- Nothing means not found
-- Just Left means constructor
-- Just Right means term
termDisplayObjectsByNamesOf ::
  (QueryM m) =>
  CodeCache scope ->
  NamesPerspective m ->
  Traversal s t Name (Maybe (Either ConstructorReference (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)))) ->
  s ->
  m t
termDisplayObjectsByNamesOf codeCache namesPerspective trav s = do
  s
    & asListOfDeduped trav %%~ \names -> do
      allRefs <- PGNameSearch.termRefsByHQNamesOf namesPerspective traversed (HQ'.NameOnly <$> names)
      let partitionedRefs =
            allRefs <&> \refs ->
              do
                (NESet.nonEmptySet refs)
                <&> NESet.findMin
                <&> \case
                  (Referent.Ref r) -> Right $ (r, r)
                  (Referent.Con r _) -> (Left r)
      Backend.displayTermsOf codeCache (traversed . _Just . _Right . _2) partitionedRefs

termDefinitionByNamesOf ::
  (QueryM m) =>
  CodebaseEnv ->
  PPEDBuilder m ->
  NamesPerspective m ->
  Width ->
  CodebaseRuntime sym IO ->
  Bool ->
  Traversal s t Name (Maybe (Either ConstructorReference TermDefinition)) ->
  s ->
  m t
termDefinitionByNamesOf codebase ppedBuilder namesPerspective width rt includeDocs trav s = do
  s
    & asListOf trav %%~ \allNames -> do
      constructorsAndRendered <- termDisplayObjectsByNamesOf rt.codeCache namesPerspective traversed allNames
      let addName name = \case
            Just (Right (termRef, displayObject)) -> Just (Right (name, termRef, displayObject))
            Just (Left constructorRef) -> Just (Left constructorRef)
            Nothing -> Nothing
      let withNames = zipWith addName allNames constructorsAndRendered
      -- Only the Right values are terms which we're concerned with, the Left values are constructors
      withNames
        & asListOf (traversed . _Just . _Right) %%~ \(refsDO :: [(Name, TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))]) -> do
          let allDeps = refsDO & foldMap \(_name, ref, displayObject) -> termDisplayObjectLabeledDependencies ref displayObject
          pped <- ppedBuilder allDeps
          let (names, refs, dos) = unzip3 refsDO
          allRenderedDocs <-
            if includeDocs
              then do
                allDocRefs <- Docs.docsForDefinitionNamesOf codebase namesPerspective traversed names
                -- TODO: properly batchify this
                for allDocRefs $ renderDocRefs codebase ppedBuilder width rt
              else pure (names $> [])
          let syntaxDOs = snd <$> Backend.termsToSyntaxOf (Suffixify False) width pped traversed (zip refs dos)
          Backend.mkTermDefinitionsOf codebase pped width traversed (zip4 (Just <$> names) refs allRenderedDocs syntaxDOs)

termDisplayObjectLabeledDependencies :: TermReference -> DisplayObject (Type Symbol Ann) (Term Symbol Ann) -> Set LD.LabeledDependency
termDisplayObjectLabeledDependencies termRef displayObject = do
  displayObject
    & bifoldMap (Type.labeledDependencies) (Term.labeledDependencies)
    & Set.insert (LD.TermReference termRef)

typeDisplayObjectsByNamesOf :: (QueryM m) => CodeCache scope -> NamesPerspective m -> Traversal s t Name (Maybe (TypeReference, DisplayObject () (DD.Decl Symbol Ann))) -> s -> m t
typeDisplayObjectsByNamesOf codeCache namesPerspective trav s = do
  s
    & asListOf trav %%~ \names -> do
      foundRefs <- PGNameSearch.typeRefsByHQNamesOf namesPerspective traversed (HQ'.NameOnly <$> names)
      let refs = fmap (\ref -> (ref, ref)) . Set.lookupMin <$> foundRefs
      Backend.displayTypesOf codeCache (traversed . _Just . _2) refs

-- | NOTE: If you're displaying many definitions you should probably generate a single PPED to
-- share among all of them, it would be more efficient than generating a PPED per definition.
typeDefinitionsByNamesOf ::
  (QueryM m) =>
  CodebaseEnv ->
  PPEDBuilder m ->
  NamesPerspective m ->
  Width ->
  CodebaseRuntime sym IO ->
  Bool ->
  Traversal s t Name (Maybe TypeDefinition) ->
  s ->
  m t
typeDefinitionsByNamesOf codebase ppedBuilder namesPerspective width rt includeDocs trav s = do
  s
    & asListOf trav %%~ \allNames -> do
      typeDisplayObjs <- typeDisplayObjectsByNamesOf rt.codeCache namesPerspective traversed allNames
      let addName name = \case
            Just (ref, displayObject) -> Just (name, ref, displayObject)
            Nothing -> Nothing
      let withNames = zipWith addName allNames typeDisplayObjs
      withNames
        & asListOf (traversed . _Just) %%~ \displayInfos -> do
          let (names, refs, displayObjects) = unzip3 displayInfos
          let allDeps = displayInfos & foldMap \(_name, ref, dispObj) -> typeDisplayObjectLabeledDependencies ref dispObj
          pped <- ppedBuilder allDeps
          allRenderedDocs <-
            if includeDocs
              then do
                allDocRefs <- Docs.docsForDefinitionNamesOf codebase namesPerspective traversed names
                -- TODO: properly batchify this
                for allDocRefs $ renderDocRefs codebase ppedBuilder width rt
              else pure (names $> [])
          let syntaxDOs = snd <$> Backend.typesToSyntaxOf (Suffixify False) width pped traversed (zip refs displayObjects)
          Backend.mkTypeDefinitionsOf pped width traversed (zip4 (Just <$> names) refs allRenderedDocs syntaxDOs)

typeDisplayObjectLabeledDependencies :: TypeReference -> DisplayObject () (DD.Decl Symbol Ann) -> Set LD.LabeledDependency
typeDisplayObjectLabeledDependencies typeRef displayObject = do
  displayObject
    & foldMap (DD.labeledDeclDependenciesIncludingSelfAndFieldAccessors typeRef)

hqNameQuery ::
  (QueryM m) =>
  NameSearch m ->
  [HQ.HashQualified Name] ->
  m QueryResult
hqNameQuery NameSearch {typeSearch, termSearch} hqs = do
  -- Split the query into hash-only and hash-qualified-name queries.
  let (hashes, hqnames) = partitionEithers (map HQ'.fromHQ hqs)
  -- Find the terms with those hashes.
  termRefs <-
    filter (not . Set.null . snd) . zip hashes
      <$> traverse
        Codebase.termReferentsByShortHash
        hashes
  -- Find types with those hashes.
  typeRefs <-
    filter (not . Set.null . snd) . zip hashes
      <$> traverse
        Codebase.typeReferencesByShortHash
        hashes
  -- Now do the name queries.
  let mkTermResult sh r = SR.termResult (HQ.HashOnly sh) r Set.empty
      mkTypeResult sh r = SR.typeResult (HQ.HashOnly sh) r Set.empty
      -- Transform the hash results a bit
      termResults =
        (\(sh, tms) -> mkTermResult sh <$> toList tms) <$> termRefs
      typeResults =
        (\(sh, tps) -> mkTypeResult sh <$> toList tps) <$> typeRefs

  -- Now do the actual name query
  resultss <- for hqnames (\name -> liftA2 (<>) (NameSearch.applySearch typeSearch NameSearch.ExactName name) (NameSearch.applySearch termSearch NameSearch.ExactName name))
  let (misses, hits) =
        zipWith
          ( \hqname results ->
              (if null results then Left hqname else Right results)
          )
          hqnames
          resultss
          & partitionEithers
      -- Handle query misses correctly
      missingRefs =
        [ HQ.HashOnly x
          | x <- hashes,
            isNothing (lookup x termRefs) && isNothing (lookup x typeRefs)
        ]
      -- Gather the results
      results =
        List.sort
          . List.uniqueBy SR.toReferent
          . concat
          $ (hits ++ termResults ++ typeResults)
  pure
    QueryResult
      { misses = missingRefs ++ map HQ'.toHQ misses,
        hits = results
      }
