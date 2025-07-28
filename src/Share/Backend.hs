{-# LANGUAGE MultiWayIf #-}

module Share.Backend
  ( mkTypeDefinitionsOf,
    mkTermDefinitionsOf,
    typeListEntry,
    termListEntry,
    displayTermsOf,
    displayTypesOf,
    evalDocRef,
    lsBranch,
    getTermTagsOf,
    getTypeTagsOf,
    typeDeclHeader,

    -- * Re-exports, mostly code with no SQLite dependencies
    Backend.bestNameForTerm,
    Backend.bestNameForType,
    Backend.termsToSyntax,
    Backend.termsToSyntaxOf,
    Backend.typesToSyntax,
    Backend.typesToSyntaxOf,
    Backend.definitionResultsDependencies,
    Backend.DefinitionResults (..),
    Backend.IncludeCycles (..),
    Backend.findDocInBranch,
    Backend.ShallowListEntry (..),
    Backend.termEntryToNamedTerm,
    Backend.typeEntryToNamedType,
    Backend.formatSuffixedType,
    Backend.typeToSyntaxHeader,
    Backend.mungeSyntaxText,
    Backend.FoundRef (..),
  )
where

import Control.Lens hiding ((??))
import Data.List (unzip4, zipWith4, zipWith5)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Share.Codebase qualified as Codebase
import Share.Codebase.CodeCache qualified as CC
import Share.Codebase.Types (CodebaseRuntime (CodebaseRuntime, cachedEvalResult))
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Conversions (namespaceStatsPgToV2)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Definitions.Queries qualified as DefnQ
import Share.Prelude
import Share.Utils.Lens (asListOfDeduped)
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Reference qualified as V2
import U.Codebase.Referent qualified as V2Referent
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as B
import Unison.Builtin.Decls qualified as Decls
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Editor.DisplayObject qualified as DisplayObject
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.IOSource qualified as DD
import Unison.Server.Backend qualified as Backend
import Unison.Server.Doc qualified as Doc
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (toText)
import Unison.Term qualified as Term
import Unison.Term qualified as V1
import Unison.Term qualified as V1Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Type qualified as V1
import Unison.Typechecker qualified as Typechecker
import Unison.Util.AnnotatedText (AnnotatedText)
import Unison.Util.Pretty (Width)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.SyntaxText qualified as UST
import Unison.Var (Var)

mkTypeDefinitionsOf ::
  (QueryM m) =>
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Traversal
    s
    t
    ( Maybe Name.Name,
      Reference,
      [(HashQualifiedName, UnisonHash, Doc.Doc)],
      DisplayObject
        (AnnotatedText (UST.Element Reference))
        (AnnotatedText (UST.Element Reference))
    )
    TypeDefinition ->
  s ->
  m t
mkTypeDefinitionsOf pped width trav s =
  s
    & asListOf trav %%~ \inputs -> do
      let (_, refs, _, _) = unzip4 inputs
      tags <- getTypeTagsOf traversed refs
      let biasedPPEDs = inputs <&> \(mayName, _, _, _) -> maybe pped (\name -> PPED.biasTo [name] pped) mayName
      let getBestName ppe r = Backend.bestNameForType @Symbol (PPED.suffixifiedPPE ppe) width r
      let bestNames = zipWith getBestName biasedPPEDs refs
      pure $ zipWith4 buildTypeDefinition tags bestNames biasedPPEDs inputs
  where
    buildTypeDefinition tag bn biasedPPE (_mayName, ref, docs, tp) =
      let fqnPPE = PPED.unsuffixifiedPPE biasedPPE
       in TypeDefinition
            (HQ'.toText <$> PPE.allTypeNames fqnPPE ref)
            bn
            tag
            (bimap Backend.mungeSyntaxText Backend.mungeSyntaxText tp)
            docs

mkTermDefinitionsOf ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Traversal
    s
    t
    ( Maybe Name.Name,
      Reference,
      [(HashQualifiedName, UnisonHash, Doc.Doc)],
      DisplayObject
        (AnnotatedText (UST.Element Reference))
        (AnnotatedText (UST.Element Reference))
    )
    TermDefinition ->
  s ->
  m t
mkTermDefinitionsOf codebase pped width trav s = do
  s
    & asListOf trav %%~ \inputs -> do
      let (_names, refs, _docs, _dispObs) = unzip4 inputs
      termTypes <- Codebase.expectTypesOfTermsOf codebase traversed refs
      let biasedPPEDs = inputs <&> \(mayName, _, _, _) -> maybe pped (\name -> PPED.biasTo [name] pped) mayName
      let getBestName ppe r = Backend.bestNameForTerm @Symbol (PPED.suffixifiedPPE ppe) width (Referent.Ref r)
      let bestNames = zipWith getBestName biasedPPEDs refs
      tags <- getTermTagsOf traversed (zipWith (\ref typ -> (V2Referent.Ref ref, typ)) refs termTypes)
      pure $ zipWith5 mk inputs termTypes bestNames tags biasedPPEDs
  where
    mk (_, ref, docs, tmDispObj) termType bn tag termPPED =
      let fqnTermPPE = PPED.unsuffixifiedPPE termPPED
          -- We don't ever display individual constructors (they're shown as part of their
          -- type), so term references are never constructors.
          referent = Referent.Ref ref
       in TermDefinition
            (HQ'.toText <$> PPE.allTermNames fqnTermPPE referent)
            bn
            tag
            (bimap Backend.mungeSyntaxText Backend.mungeSyntaxText tmDispObj)
            (Backend.formatSuffixedType termPPED width termType)
            docs

termListEntry ::
  (PG.QueryM m) =>
  Type Symbol Ann ->
  ExactName NameSegment V2Referent.Referent ->
  m (Backend.TermEntry Symbol Ann)
termListEntry typ (ExactName nameSegment ref) = do
  tag <- getTermTagsOf id (ref, typ)
  pure $
    Backend.TermEntry
      { termEntryReferent = ref,
        termEntryName = Name.fromSegment nameSegment,
        termEntryType = Just typ,
        termEntryTag = tag,
        -- We no longer track conflicts on branches
        termEntryConflicted = False,
        termEntryHash = Cv.referent2toshorthash1 Nothing ref
      }

typeListEntry ::
  (PG.QueryM m) =>
  ExactName NameSegment Reference ->
  m Backend.TypeEntry
typeListEntry (ExactName nameSegment ref) = do
  tag <- getTypeTagsOf id ref
  pure $
    Backend.TypeEntry
      { typeEntryReference = ref,
        typeEntryName = Name.fromSegment nameSegment,
        -- We no longer track conflicts on branches
        typeEntryConflicted = False,
        typeEntryTag = tag,
        typeEntryHash = SH.shortenTo Codebase.shorthashLength $ Reference.toShortHash ref
      }

getTermTagsOf ::
  (PG.QueryM m, Var v) =>
  Traversal s t (V2Referent.Referent, Type v Ann) TermTag ->
  s ->
  m t
getTermTagsOf trav s = do
  s
    & asListOfDeduped trav %%~ \inputs -> do
      let withConstructorRefs =
            inputs <&> \(ref, termType) -> case ref of
              (V2Referent.Ref _ref) -> (termType, Nothing)
              (V2Referent.Con ref _) -> (termType, Just ref)
      Codebase.expectDeclKindsOf (traversed . _2 . _Just) withConstructorRefs
        <&> fmap computeTag
  where
    -- A term is a doc if its type conforms to the `Doc` type.
    isDoc termType =
      Typechecker.isEqual termType (Type.ref mempty Decls.docRef)
        || Typechecker.isEqual termType (Type.ref mempty DD.doc2Ref)
    -- A term is a test if it has the type [test.Result]
    isTest termType = Typechecker.isEqual termType (Decls.testResultListType mempty)
    computeTag (termType, mayConstructorType) =
      if
        | isDoc termType -> Doc
        | isTest termType -> Test
        | Just CT.Effect <- mayConstructorType -> Constructor Ability
        | Just CT.Data <- mayConstructorType -> Constructor Data
        | otherwise -> Plain

getTypeTagsOf ::
  (PG.QueryM m) =>
  Traversal s t Reference.TypeReference TypeTag ->
  s ->
  m t
getTypeTagsOf trav s = do
  s
    & asListOf trav %%~ \refs -> do
      Codebase.loadDeclKindsOf traversed refs <&> fmap \case
        Nothing -> Data
        Just CT.Data -> Data
        Just CT.Effect -> Ability

displayTermsOf :: (QueryM m) => Codebase.CodebaseEnv -> Traversal s t Reference (DisplayObject (Type Symbol Ann) (V1.Term Symbol Ann)) -> s -> m t
displayTermsOf codebase trav s =
  s
    & asListOfDeduped trav %%~ \refs -> do
      let partitionedRefs =
            refs <&> \case
              ref@(Reference.Builtin _) -> do
                case Map.lookup ref B.termRefTypes of
                  -- This would be better as a `MissingBuiltin` constructor; `MissingObject` is kind of being
                  -- misused here. Is `MissingObject` even possible anymore?
                  Nothing -> Left $ MissingObject $ Reference.toShortHash ref
                  Just typ -> Left $ BuiltinObject (mempty <$ typ)
              Reference.DerivedId rid -> Right rid
      r <- Codebase.expectTermsByRefIdsOf codebase (traversed . _Right) partitionedRefs
      r
        & traversed
          %~ \case
            Right (term, ty) -> case term of
              V1Term.Ann' _ _ -> UserObject term
              -- manually annotate if necessary
              _ -> UserObject (V1Term.ann (ABT.annotation term) term ty)
            Left obj -> obj
        & pure

displayTypesOf :: (QueryM m) => Codebase.CodebaseEnv -> Traversal s t Reference (DisplayObject () (DD.Decl Symbol Ann)) -> s -> m t
displayTypesOf codebase trav s =
  s
    & asListOf trav %%~ \refs -> do
      let partitionedRefs =
            refs <&> \case
              Reference.Builtin _ -> Left (BuiltinObject ())
              Reference.DerivedId rid -> Right rid
      Codebase.expectTypeDeclarationsByRefIdsOf codebase (traversed . _Right) partitionedRefs
        <&> fmap \case
          Left obj -> obj
          Right decl -> (UserObject decl)

evalDocRef ::
  forall m s.
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime s IO ->
  V2.TermReference ->
  m (Doc.EvaluatedDoc Symbol)
evalDocRef codebase (CodebaseRuntime {codeLookup, codeCache, cachedEvalResult, unisonRuntime}) termRef = PG.transactionSpan "evalDocRef" mempty do
  let tm = Term.ref () termRef
  case termRef of
    Reference.Builtin _ -> Doc.evalDoc terms typeOf eval decls tm
    Reference.DerivedId refId -> do
      termId <- DefnQ.expectTermIdsByRefIdsOf id refId
      (termDeps, typeDeps) <- DefnQ.termTransitiveDependencyRefs (Set.singleton termId)
      -- Prime the cache with all the terms and types we know we'll need.
      -- No need to store them manually, they'll be persistently cached automatically just by
      -- fetching
      _ <- CC.getTermsAndTypesByRefIdsOf codeCache traversed (Set.toList termDeps)
      _ <- CC.getTypeDeclsByRefIdsOf codeCache traversed (Set.toList typeDeps)
      -- TODO: batchify evalDoc within unison
      Doc.evalDoc terms typeOf eval decls tm
  where
    -- Loading one at a time is inefficient, so we prime the cache above.
    terms :: Reference -> m (Maybe (V1.Term Symbol ()))
    terms = CC.termsForRefsOf codeCache id

    -- Loading one at a time is inefficient, so we prime the cache above.
    typeOf :: Referent.Referent -> m (Maybe (V1.Type Symbol ()))
    typeOf = CC.typesOfReferentsOf codeCache id

    -- Loading one at a time is inefficient, so we prime the cache above.
    decls :: Reference -> m (Maybe (DD.Decl Symbol ()))
    decls ref = fmap (DD.amap (const ())) <$> CC.getTypeDeclsByRefsOf codeCache id ref

    eval :: V1.Term Symbol a -> m (Maybe (V1.Term Symbol ()))
    eval (Term.amap (const mempty) -> tm) = PG.transactionSpan "eval" mempty do
      -- We use an empty ppe for evalutation, it's only used for adding additional context to errors.
      let evalPPE = PPE.empty
      termRef <- fmap eitherToMaybe . PG.transactionUnsafeIO . liftIO $ Rt.evaluateTerm' codeLookup cachedEvalResult evalPPE unisonRuntime tm
      case termRef of
        -- don't cache when there were decompile errors
        Just (errs, tmr) | null errs -> do
          Codebase.saveCachedEvalResult
            codebase
            (Hashing.hashClosedTerm tm)
            (Term.amap (const mempty) tmr)
        _ -> pure ()
      pure $ termRef <&> Term.amap (const mempty) . snd

-- | Find all definitions and children reachable from the given 'V2Branch.Branch',
lsBranch ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  V2Branch.Branch n ->
  m [Backend.ShallowListEntry Symbol Ann]
lsBranch codebase b0 = do
  let flattenTypeRefs :: Map NameSegment (Map ref v) -> [(ref, NameSegment)]
      flattenTypeRefs m =
        m
          & ifoldMap \ns refs -> (,ns) <$> Map.keys refs

  termsWithTypes :: [(V2Referent.Referent, V1.Type Symbol Ann, NameSegment)] <-
    V2Branch.terms b0
      & itoListOf (itraversed <. folding Map.keys)
      & fmap (\(ns, r) -> (r, r, ns))
      & Codebase.expectTypesOfReferentsOf codebase (traversed . _2)

  termEntries <- for termsWithTypes $ \(r, typ, ns) -> do
    Backend.ShallowTermEntry <$> termListEntry typ (ExactName ns r)
  typeEntries <-
    for (flattenTypeRefs $ V2Branch.types b0) \(r, ns) -> do
      let v1Ref = Cv.reference2to1 r
      Backend.ShallowTypeEntry <$> typeListEntry (ExactName ns v1Ref)
  childrenWithStats <-
    V2Branch.children b0
      & traversed %~ (id &&& Causal.valueHash)
      & CausalQ.expectNamespaceStatsOf (traversed . _2)
  let branchEntries :: [Backend.ShallowListEntry Symbol Ann] = do
        (ns, (h, stats)) <- Map.toList $ childrenWithStats
        let v2Stats = namespaceStatsPgToV2 stats
        guard $ V2Branch.hasDefinitions v2Stats
        pure $ Backend.ShallowBranchEntry ns (V2Causal.causalHash h) v2Stats
      patchEntries :: [Backend.ShallowListEntry Symbol Ann] = do
        (ns, _h) <- Map.toList $ V2Branch.patches b0
        pure $ Backend.ShallowPatchEntry ns
  pure . List.sortOn Backend.listEntryName $
    termEntries
      ++ typeEntries
      ++ branchEntries
      ++ patchEntries

typeDeclHeader ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  PPE.PrettyPrintEnv ->
  Reference ->
  m (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
typeDeclHeader codebase ppe r = case Reference.toId r of
  Just rid ->
    Codebase.loadV1TypeDeclarationsByRefIdsOf codebase id rid <&> \case
      Nothing -> DisplayObject.MissingObject (Reference.toShortHash r)
      Just decl ->
        DisplayObject.UserObject $
          Syntax.convertElement
            <$> Pretty.render defaultWidth (DeclPrinter.prettyDeclHeader DeclPrinter.RenderUniqueTypeGuids'No name decl)
  Nothing ->
    pure (DisplayObject.BuiltinObject (Backend.formatTypeName ppe r))
  where
    name = PPE.typeName ppe r
