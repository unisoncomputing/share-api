-- | This module contains implementations of Backend methods which are specialized for Share.
-- We should likely move them to the Share repository eventually, but for now it's much easier
-- to ensure they're resilient to refactors and changes in the Backend API if they live here.
--
-- Perhaps we'll move them when the backing implementation switches to postgres.
module Unison.Server.Share.Definitions (definitionForHQName) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Enlil.Backend qualified as Backend
import Enlil.Codebase (CodebaseRuntime)
import Enlil.Codebase qualified as Codebase
import Enlil.Postgres qualified as PG
import Enlil.Postgres.Causal.Queries qualified as CausalQ
import Enlil.Postgres.IDs (CausalId)
import Enlil.Postgres.NameLookups.Ops qualified as NameLookupOps
import Enlil.Prelude
import Unison.Codebase.Path (Path)
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Reference (TermReference)
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
import Unison.Util.List qualified as List
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (Width)

-- | Renders a definition for the given name or hash alongside its documentation.
definitionForHQName ::
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
  CodebaseRuntime ->
  -- | The name, hash, or both, of the definition to display.
  HQ.HashQualified Name ->
  Codebase.CodebaseM e DefinitionDisplayResults
definitionForHQName perspective rootCausalId renderWidth suffixifyBindings rt perspectiveQuery = do
  rootBranchNamespaceHashId <- CausalQ.expectNamespaceIdForCausal rootCausalId
  (namesPerspective, query) <- NameLookupOps.relocateToNameRoot perspective perspectiveQuery rootBranchNamespaceHashId
  Debug.debugM Debug.Server "definitionForHQName: (namesPerspective, query)" (namesPerspective, query)
  -- Bias towards both relative and absolute path to queries,
  -- This allows us to still bias towards definitions outside our namesRoot but within the
  -- same tree;
  -- e.g. if the query is `map` and we're in `base.trunk.List`,
  -- we bias towards `map` and `.base.trunk.List.map` which ensures we still prefer names in
  -- `trunk` over those in other releases.
  -- ppe which returns names fully qualified to the current namesRoot,  not to the codebase root.
  let biases = maybeToList $ HQ.toName query
  let ppedBuilder deps = (PPED.biasTo biases) <$> lift (PPEPostgres.ppedForReferences namesPerspective deps)
  let nameSearch = PGNameSearch.nameSearchForPerspective namesPerspective
  dr@(Backend.DefinitionResults terms types misses) <- mkDefinitionsForQuery nameSearch [query]
  Debug.debugM Debug.Server "definitionForHQName: found definitions" dr
  let width = mayDefaultWidth renderWidth
  let docResults :: Name -> Codebase.CodebaseM e [(HashQualifiedName, UnisonHash, Doc.Doc)]
      docResults name = do
        Debug.debugM Debug.Server "definitionForHQName: looking up docs for name" name
        docRefs <- Docs.docsForDefinitionName nameSearch name
        Debug.debugM Debug.Server "definitionForHQName: Found these docs" docRefs
        renderDocRefs ppedBuilder width rt docRefs

  let drDeps = Backend.definitionResultsDependencies dr
  termAndTypePPED <- ppedBuilder drDeps
  let fqnTermAndTypePPE = PPED.unsuffixifiedPPE termAndTypePPED
  typeDefinitions <-
    ifor (Backend.typesToSyntax suffixifyBindings width termAndTypePPED types) \ref tp -> do
      let hqTypeName = PPE.typeNameOrHashOnly fqnTermAndTypePPE ref
      docs <- maybe (pure []) docResults (HQ.toName hqTypeName)
      lift $ Backend.mkTypeDefinition termAndTypePPED width ref docs tp
  termDefinitions <-
    ifor (Backend.termsToSyntax suffixifyBindings width termAndTypePPED terms) \reference trm -> do
      let referent = Referent.Ref reference
      let hqTermName = PPE.termNameOrHashOnly fqnTermAndTypePPE referent
      docs <- maybe (pure []) docResults (HQ.toName hqTermName)
      Backend.mkTermDefinition termAndTypePPED width reference docs trm
  let renderedDisplayTerms = Map.mapKeys Reference.toText termDefinitions
      renderedDisplayTypes = Map.mapKeys Reference.toText typeDefinitions
      renderedMisses = fmap HQ.toText misses
  pure $
    DefinitionDisplayResults
      renderedDisplayTerms
      renderedDisplayTypes
      renderedMisses

renderDocRefs ::
  PPEDBuilder (Codebase.CodebaseM e) ->
  Width ->
  CodebaseRuntime ->
  [TermReference] ->
  Codebase.CodebaseM e [(HashQualifiedName, UnisonHash, Doc.Doc)]
renderDocRefs _ppedBuilder _width _rt [] = pure []
renderDocRefs ppedBuilder width rt docRefs = do
  eDocs <- for docRefs \ref -> (ref,) <$> (Backend.evalDocRef rt ref)
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
  NameSearch (PG.Transaction e) ->
  [HQ.HashQualified Name] ->
  Codebase.CodebaseM e Backend.DefinitionResults
mkDefinitionsForQuery nameSearch query = do
  QueryResult misses results <- lift $ hqNameQuery nameSearch query
  -- todo: remember to replace this with getting components directly,
  -- and maybe even remove getComponentLength from Codebase interface altogether
  terms <- Map.foldMapM (\ref -> (ref,) <$> Backend.displayTerm ref) (searchResultsToTermRefs results)
  types <- do
    typeRefs <- pure $ searchResultsToTypeRefs results
    Map.foldMapM (\ref -> (ref,) <$> Backend.displayType ref) typeRefs
  pure (Backend.DefinitionResults terms types misses)
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

hqNameQuery ::
  NameSearch (PG.Transaction e) ->
  [HQ.HashQualified Name] ->
  PG.Transaction e QueryResult
hqNameQuery NameSearch {typeSearch, termSearch} hqs = do
  -- Split the query into hash-only and hash-qualified-name queries.
  let (hashes, hqnames) = partitionEithers (map HQ'.fromHQ2 hqs)
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
