{-# LANGUAGE MultiWayIf #-}

module Share.Backend
  ( mkTypeDefinition,
    mkTermDefinition,
    typeListEntry,
    termListEntry,
    displayTerm,
    displayType,
    evalDocRef,
    lsBranch,
    getTermTag,
    getTypeTag,
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
import Data.List qualified as List
import Data.Map qualified as Map
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseRuntime (CodebaseRuntime, cachedEvalResult))
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.Causal.Conversions (namespaceStatsPgToV2)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Prelude
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

mkTypeDefinition ::
  (QueryM m) =>
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Reference ->
  [(HashQualifiedName, UnisonHash, Doc.Doc)] ->
  DisplayObject
    (AnnotatedText (UST.Element Reference))
    (AnnotatedText (UST.Element Reference)) ->
  m TypeDefinition
mkTypeDefinition pped width r docs tp = do
  let bn = Backend.bestNameForType @Symbol (PPED.suffixifiedPPE pped) width r
  tag <- getTypeTag r
  pure $
    TypeDefinition
      (HQ'.toText <$> PPE.allTypeNames fqnPPE r)
      bn
      tag
      (bimap Backend.mungeSyntaxText Backend.mungeSyntaxText tp)
      docs
  where
    fqnPPE = PPED.unsuffixifiedPPE pped

mkTermDefinition ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  PPED.PrettyPrintEnvDecl ->
  Width ->
  Reference ->
  [(HashQualifiedName, UnisonHash, Doc.Doc)] ->
  DisplayObject
    (AnnotatedText (UST.Element Reference))
    (AnnotatedText (UST.Element Reference)) ->
  m TermDefinition
mkTermDefinition codebase termPPED width r docs tm = do
  let referent = V2Referent.Ref r
  -- TODO: batchify this properly
  termType <- Codebase.expectTypesOfTermsOf codebase id r
  let bn = Backend.bestNameForTerm @Symbol (PPED.suffixifiedPPE termPPED) width (Referent.Ref r)
  tag <- getTermTag referent termType
  mk termType bn tag
  where
    fqnTermPPE = PPED.unsuffixifiedPPE termPPED
    mk termType bn tag = do
      -- We don't ever display individual constructors (they're shown as part of their
      -- type), so term references are never constructors.
      let referent = Referent.Ref r
      pure $
        TermDefinition
          (HQ'.toText <$> PPE.allTermNames fqnTermPPE referent)
          bn
          tag
          (bimap Backend.mungeSyntaxText Backend.mungeSyntaxText tm)
          (Backend.formatSuffixedType termPPED width termType)
          docs

termListEntry ::
  (PG.QueryM m) =>
  Type Symbol Ann ->
  ExactName NameSegment V2Referent.Referent ->
  m (Backend.TermEntry Symbol Ann)
termListEntry typ (ExactName nameSegment ref) = do
  -- TODO: batchify this properly
  tag <- getTermTag ref typ
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
  -- TODO: batchify this properly
  tag <- getTypeTag ref
  pure $
    Backend.TypeEntry
      { typeEntryReference = ref,
        typeEntryName = Name.fromSegment nameSegment,
        -- We no longer track conflicts on branches
        typeEntryConflicted = False,
        typeEntryTag = tag,
        typeEntryHash = SH.shortenTo Codebase.shorthashLength $ Reference.toShortHash ref
      }

getTermTag ::
  (PG.QueryM m, Var v) =>
  V2Referent.Referent ->
  Type v Ann ->
  m TermTag
getTermTag r termType = do
  -- A term is a doc if its type conforms to the `Doc` type.
  let isDoc =
        Typechecker.isEqual termType (Type.ref mempty Decls.docRef)
          || Typechecker.isEqual termType (Type.ref mempty DD.doc2Ref)
  -- A term is a test if it has the type [test.Result]
  let isTest = Typechecker.isEqual termType (Decls.testResultListType mempty)
  constructorType <- case r of
    V2Referent.Ref {} -> pure Nothing
    V2Referent.Con ref _ ->
      -- TODO: batchify this properly
      Just <$> Codebase.expectDeclKindsOf id ref
  pure $
    if
      | isDoc -> Doc
      | isTest -> Test
      | Just CT.Effect <- constructorType -> Constructor Ability
      | Just CT.Data <- constructorType -> Constructor Data
      | otherwise -> Plain

getTypeTag ::
  (PG.QueryM m) =>
  Reference.TypeReference ->
  m TypeTag
getTypeTag r = do
  -- TODO: batchify this properly
  Codebase.loadDeclKindsOf id r <&> \case
    Nothing -> Data
    Just CT.Data -> Data
    Just CT.Effect -> Ability

displayTerm :: (QueryM m) => Codebase.CodebaseEnv -> Reference -> m (DisplayObject (Type Symbol Ann) (V1.Term Symbol Ann))
displayTerm codebase = \case
  ref@(Reference.Builtin _) -> do
    pure case Map.lookup ref B.termRefTypes of
      -- This would be better as a `MissingBuiltin` constructor; `MissingObject` is kind of being
      -- misused here. Is `MissingObject` even possible anymore?
      Nothing -> MissingObject $ Reference.toShortHash ref
      Just typ -> BuiltinObject (mempty <$ typ)
  Reference.DerivedId rid -> do
    -- TODO: batchify this properly
    (term, ty) <- Codebase.expectTermsByRefIdsOf codebase id rid
    pure case term of
      V1Term.Ann' _ _ -> UserObject term
      -- manually annotate if necessary
      _ -> UserObject (V1Term.ann (ABT.annotation term) term ty)

displayType :: (QueryM m) => Codebase.CodebaseEnv -> Reference -> m (DisplayObject () (DD.Decl Symbol Ann))
displayType codebase = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId rid -> do
    -- TODO: batchify this properly
    decl <- Codebase.expectTypeDeclarationsByRefIdsOf codebase id rid
    pure (UserObject decl)

evalDocRef ::
  forall m s.
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  Codebase.CodebaseRuntime s IO ->
  V2.TermReference ->
  m (Doc.EvaluatedDoc Symbol)
evalDocRef codebase (CodebaseRuntime {codeLookup, cachedEvalResult, unisonRuntime}) termRef = do
  let tm = Term.ref () termRef
  -- TODO: batchify evalDoc.
  Doc.evalDoc terms typeOf eval decls tm
  where
    terms :: Reference -> m (Maybe (V1.Term Symbol ()))
    terms termRef@(Reference.Builtin _) = pure (Just (Term.ref () termRef))
    terms (Reference.DerivedId termRef) =
      -- TODO: batchify properly
      fmap (Term.unannotate . fst) <$> (Codebase.loadTermAndTypeByRefIdsOf codebase id termRef)

    typeOf :: Referent.Referent -> m (Maybe (V1.Type Symbol ()))
    typeOf termRef =
      -- TODO: batchify properly
      fmap void <$> Codebase.loadTypesOfReferentsOf codebase id (Cv.referent1to2 termRef)

    eval :: V1.Term Symbol a -> m (Maybe (V1.Term Symbol ()))
    eval (Term.amap (const mempty) -> tm) = do
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

    decls :: Reference -> m (Maybe (DD.Decl Symbol ()))
    decls (Reference.DerivedId typeRef) =
      -- TODO: batchify properly
      fmap (DD.amap (const ())) <$> (Codebase.loadTypeDeclarationsByRefIdsOf codebase id typeRef)
    decls _ = pure Nothing

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

-- TODO: batchify this
typeDeclHeader ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  PPE.PrettyPrintEnv ->
  Reference ->
  m (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
typeDeclHeader codebase ppe r = case Reference.toId r of
  Just rid ->
    Codebase.loadTypeDeclarationsByRefIdsOf codebase id rid <&> \case
      Nothing -> DisplayObject.MissingObject (Reference.toShortHash r)
      Just decl ->
        DisplayObject.UserObject $
          Syntax.convertElement
            <$> Pretty.render defaultWidth (DeclPrinter.prettyDeclHeader DeclPrinter.RenderUniqueTypeGuids'No name decl)
  Nothing ->
    pure (DisplayObject.BuiltinObject (Backend.formatTypeName ppe r))
  where
    name = PPE.typeName ppe r
