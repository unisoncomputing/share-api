{-# LANGUAGE DataKinds #-}

-- | This module provides the background worker which syncs definitions
module Share.BackgroundJobs.Search.DefinitionSync (worker) where

import Control.Lens
import Control.Monad.Except
import Data.Either (isRight)
import Data.Generics.Product (HasField (..))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonMap
import Data.Monoid (Any (..), Sum (..))
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Errors (reportError)
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync.Types (Arity (..), DefinitionDocument (..), DefnSearchToken (..), Occurrence, OccurrenceKind (..), TermOrTypeSummary (..), TermOrTypeTag (..), VarId (..))
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase qualified as Codebase
import Share.IDs (ReleaseId, UserId)
import Share.Metrics qualified as Metrics
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NamesPerspective.Ops qualified as NPOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective (..))
import Share.Postgres.Notifications qualified as Notif
import Share.Postgres.Releases.Queries qualified as RQ
import Share.Postgres.Releases.Queries qualified as ReleasesQ
import Share.Postgres.Search.DefinitionSearch.Queries qualified as DDQ
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Share.Release (Release (..))
import Share.Telemetry (withSpan)
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import Share.Web.Errors (SomeServerError)
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.ABT qualified as ABT
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (libSegment)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TypeReference)
import Unison.Reference qualified as Reference
import Unison.Server.Share.DefinitionSummary qualified as Summary
import Unison.Server.Share.DefinitionSummary.Types qualified as Summary
import Unison.Server.Types qualified as Server.Types
import Unison.ShortHash (ShortHash)
import Unison.Type qualified as Type
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Recursion qualified as Rec
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var

data DefnIndexingFailure
  = NoTypeSigForTerm Name Referent
  | NoDeclForType Name TypeReference
  | CatastrophicError SomeServerError
  deriving stock (Show)
  deriving (Logging.Loggable) via (Logging.ShowLoggable Logging.Error DefnIndexingFailure)

-- | Check every 10 minutes if we haven't heard on the notifications channel.
-- Just in case we missed a notification.
maxPollingIntervalSeconds :: Int
maxPollingIntervalSeconds = 10 * 60 -- 10 minutes

-- | How many definitions to hold in memory at a time while syncing
defnBatchSize :: Int32
defnBatchSize = 1000

worker :: Ki.Scope -> Background ()
worker scope = do
  authZReceipt <- AuthZ.backgroundJobAuthZ
  newWorker scope "search:defn-sync" $ forever do
    Notif.waitOnChannel Notif.DefinitionSyncChannel (maxPollingIntervalSeconds * 1000000)
    processReleases authZReceipt

processReleases :: AuthZ.AuthZReceipt -> Background ()
processReleases authZReceipt = do
  processRelease authZReceipt >>= \case
    -- If we processed a release, continue processing releases.
    True -> processReleases authZReceipt
    -- If we didn't process a release, just wait for the next notification.
    False -> pure ()

processRelease :: AuthZ.AuthZReceipt -> Background Bool
processRelease authZReceipt = withSpan "background:definition-sync:process-release" mempty do
  (errs, badNames, mayProcessedRelease) <- Metrics.recordDefinitionSearchIndexDuration $ PG.runTransactionMode PG.RepeatableRead PG.ReadWrite $ do
    mayUnsynced <- DDQ.claimUnsynced
    case mayUnsynced of
      Nothing -> pure (mempty, mempty, Nothing)
      Just (mayReleaseId, branchHashId, userId) -> do
        result <-
          PG.catchAllTransaction (syncRoot authZReceipt (mayReleaseId, branchHashId, userId)) >>= \case
            Right (syncErrors, badNames)
              | null syncErrors -> do
                  DDQ.deleteClaimed branchHashId
                  pure (syncErrors, badNames, Just (mayReleaseId, branchHashId, userId))
              | otherwise -> do
                  DDQ.markAsFailed branchHashId (Text.intercalate "," (tShow <$> syncErrors))
                  pure (syncErrors, badNames, Just (mayReleaseId, branchHashId, userId))
            Left (PG.Unrecoverable catastrophicError) -> do
              DDQ.markAsFailed branchHashId (tShow catastrophicError)
              pure ([CatastrophicError catastrophicError], mempty, Nothing)
        pure result
  case (errs, mayProcessedRelease) of
    -- No errors and no release processed, just return and we'll wait for the next thing
    -- to hit the queue.
    (_, Nothing) -> pure False
    -- Errors occurred, log them then continue processing.
    (errs@(_ : _), Just (mayReleaseId, rootBranchHashId, codebaseOwner)) -> do
      let msg =
            Logging.textLog ("Definition sync errors: " <> Text.intercalate "," (tShow <$> errs))
              & Logging.withSeverity Logging.Error
              & Logging.withTag ("release-id", tShow mayReleaseId)
              & Logging.withTag ("root-branch-hash-id", tShow rootBranchHashId)
              & Logging.withTag ("codebase-owner", tShow codebaseOwner)
      Logging.logMsg msg
      for_ errs reportError
      pure True
    -- No errors, but we processed a release, log that.
    ([], Just (mayReleaseId, rootBranchHashId, codebaseOwner)) -> do
      for_ badNames \badName -> do
        let msg =
              Logging.textLog ("Definition sync found bad name: " <> badName)
                & Logging.withSeverity Logging.Error
                & Logging.withTag ("release-id", tShow mayReleaseId)
                & Logging.withTag ("root-branch-hash-id", tShow rootBranchHashId)
                & Logging.withTag ("codebase-owner", tShow codebaseOwner)
        Logging.logMsg msg

      Logging.textLog ("Built definition search index for rootBranchHashId: " <> tShow rootBranchHashId <> " for codebaseOwner: " <> tShow codebaseOwner <> " and releaseId: " <> tShow mayReleaseId)
        & Logging.withTag ("release-id", tShow mayReleaseId)
        & Logging.withTag ("root-branch-hash-id", tShow rootBranchHashId)
        & Logging.withTag ("codebase-owner", tShow codebaseOwner)
        & Logging.withSeverity Logging.Info
        & Logging.logMsg
      pure True

syncRoot ::
  AuthZ.AuthZReceipt ->
  (Maybe ReleaseId, BranchHashId, UserId) ->
  PG.Transaction e ([DefnIndexingFailure], [Text])
syncRoot authZReceipt (mayReleaseId, rootBranchHashId, codebaseOwner) = do
  -- Only index it if it's not already indexed.
  (errs, badNames) <-
    (DDQ.isRootIndexed rootBranchHashId) >>= \case
      False -> do
        DDQ.markRootAsIndexed rootBranchHashId
        namesPerspective <- NPOps.namesPerspectiveForRoot rootBranchHashId
        let nlReceipt = nameLookupReceipt namesPerspective
        let codebaseLoc = Codebase.codebaseLocationForProjectRelease codebaseOwner
        let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
        termsCursor <- NLOps.projectTermsWithinRoot nlReceipt rootBranchHashId

        termErrs <- syncTerms codebase namesPerspective rootBranchHashId termsCursor
        typesCursor <- NLOps.projectTypesWithinRoot nlReceipt rootBranchHashId
        typeErrs <- syncTypes codebase namesPerspective rootBranchHashId typesCursor
        pure (termErrs <> typeErrs)
      True -> pure mempty
  -- Copy relevant index rows into the global search index as well
  for mayReleaseId (syncRelease rootBranchHashId)
  pure (errs, badNames)

syncRelease :: BranchHashId -> ReleaseId -> PG.Transaction e ()
syncRelease rootBranchHashId releaseId = fmap (fromMaybe ()) . runMaybeT $ do
  Release {projectId, releaseId, version} <- lift $ ReleasesQ.releaseById releaseId
  -- Wipe out any existing rows for this release. Normally there should be none, but this
  -- makes it easy to re-index later if we change how we tokenize things.
  latestRelease <- MaybeT $ RQ.latestReleaseByProjectId projectId
  -- Only index the latest version of a release.
  guard $ version == latestRelease.version
  lift $ DDQ.cleanIndexForProject projectId
  -- Copy the indexed documents from the scoped index into the global index.
  lift $ DDQ.copySearchDocumentsForRelease rootBranchHashId projectId releaseId

syncTerms ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  NamesPerspective m ->
  BranchHashId ->
  Cursors.PGCursor (Name, Referent) ->
  m ([DefnIndexingFailure], [Text])
syncTerms codebase namesPerspective rootBranchHashId termsCursor = do
  Cursors.foldBatched termsCursor defnBatchSize \terms -> do
    (errs, refDocs) <-
      PG.timeTransaction "Building terms" $ do
        let nonLibTerms =
              terms
                -- Most lib names are already filtered out by using the name lookup; but sometimes
                -- when libs aren't at the project root some can slip through, so we remove them.
                & V.filter
                  ( \(fqn, _) -> not (libSegment `elem` (NEL.toList $ Name.reverseSegments fqn))
                  )

        let (fqns, refs) = V.unzip nonLibTerms
        mayTypes <- Codebase.loadTypesOfReferentsOf codebase traversed refs
        let (errs, termsWithTypes) =
              V.zip3 refs fqns mayTypes & foldMap \case
                (ref, fqn, Nothing) -> ([NoTypeSigForTerm fqn ref], [])
                (ref, fqn, Just typ) -> ([], [(ref, fqn, typ)])
        termDefinitions <- for termsWithTypes \(ref, fqn, typ) -> do
          let displayName =
                fqn
                  & Name.reverseSegments
                  -- For now we treat the display name for search as just the last 2 segments of the name.
                  & \case
                    (ns :| rest) -> ns :| take 1 rest
                  & Name.fromReverseSegments
          -- TODO: batchify this
          termSummary <- Summary.termSummariesForReferentsOf namesPerspective Nothing id (ref, typ, Just displayName)
          let sh = Referent.toShortHash ref
          let (refTokens, arity) = tokensForTerm fqn ref typ termSummary
          let dd =
                DefinitionDocument
                  { rootBranchHashId,
                    fqn,
                    hash = sh,
                    tokens = refTokens,
                    arity = arity,
                    tag = ToTTermTag (termSummary.tag),
                    metadata = ToTTermSummary termSummary
                  }
          pure dd
        pure (errs, termDefinitions)

    -- It's much more efficient to build only one PPE per batch.
    let allDeps = setOf (folded . folding tokens . folded . to LD.TypeReference) refDocs
    pped <- PG.timeTransaction "Build PPED" $ PPEPostgres.ppedForReferences namesPerspective allDeps
    let ppe = PPED.unsuffixifiedPPE pped
    let namedDocs :: [DefinitionDocument Name (Name, ShortHash)]
        namedDocs =
          refDocs
            & traversed . field @"tokens" %~ Set.mapMaybe \token -> do
              for token \ref -> do
                name <- PPE.types ppe ref
                pure $ (HQ'.toName $ name, Reference.toShortHash ref)
    badNames <- PG.timeTransaction "Inserting Docs" $ DDQ.insertDefinitionDocuments namedDocs
    pure (errs, badNames)

-- | Compute the search tokens for a term given its name, hash, and type signature
tokensForTerm :: (Var.Var v) => Name -> Referent -> Type.Type v a -> Summary.TermSummary -> (Set (DefnSearchToken TypeReference), Arity)
tokensForTerm name ref typ (Summary.TermSummary {tag}) = do
  let sh = Referent.toShortHash ref
      baseTokens = Set.fromList [NameToken name, HashToken sh]
      tagTokens = Set.singleton $ TermTagToken tag
      (tsTokens, arity) = typeSigTokens typ
   in (baseTokens <> tsTokens <> tagTokens, arity)

data TokenGenEnv v = TokenGenEnv
  { -- Track which var names we've assigned to which Ids.
    varIds :: Map v VarId
  }
  deriving stock (Show, Generic)

data TokenGenState v = TokenGenState
  { -- The next VarId to assign.
    nextVarId :: VarId
  }
  deriving stock (Show, Generic)

type TokenGenM v = ReaderT (TokenGenEnv v) (State (TokenGenState v))

-- | Compute var occurrence and type ref occurrence search tokens from a type signature.
typeSigTokens :: forall v ann. (Var.Var v) => Type.Type v ann -> (Set (DefnSearchToken TypeReference), Arity)
typeSigTokens typ =
  let occMap :: MonoidalMap (Either VarId TypeReference) (Occurrence, Any)
      arity :: Arity
      (occMap, Sum arity) = flip evalState initState . flip runReaderT (TokenGenEnv mempty) $ Rec.cata alg typ
      (varIds, typeRefs) =
        MonMap.toList occMap & foldMap \case
          (Left vId, (occ, ret)) -> ([(vId, (occ, ret))], [])
          (Right typeRef, (occ, ret)) -> ([], [(typeRef, (occ, ret))])
      -- Normalize and expand var tokens.
      -- Tokens are normalized such that the varIds are assigned in increasing order to vars
      -- by the number of occurrences they have in the type signature.
      --
      -- Then we expand them into one token for [1..numOccurrences] so they can be found by a
      -- portion of the type in a search.
      normalizedVarTokens :: Set (DefnSearchToken TypeReference)
      normalizedVarTokens =
        varIds
          -- Rewrite varIds normalized by number of occurrences,
          -- this is necessary to ensure that order of type variables
          -- in a type signature don't actually matter.
          --
          -- E.g. BOTH of (a -> b -> a) and (b -> a -> a) turn into [(VarId 1, 1), (VarId 2, 2)]
          & List.sortOn snd
          -- Expand a token for each occurrence of a variable, this way
          -- a -> a -> a still matches the type a -> a, since the user may not have typed the
          -- whole thing.
          & imap (\i (_vId, occ) -> (VarId i, occ))
          -- Expand a token for each occurrence of a variable, this way
          -- 'Text' still matches the type 'Text -> Text'
          & foldMap
            ( \(vId, (occ, Any isReturn)) ->
                Monoid.whenM isReturn [TypeVarToken vId ReturnPosition] <> ((TypeVarToken vId . Count) <$> [1 .. occ])
            )
          & Set.fromList
      expandedTypeRefTokens =
        typeRefs
          & foldMap \(typeRef, (occ, Any isReturn)) ->
            Monoid.whenM isReturn [TypeMentionToken typeRef ReturnPosition] <> (TypeMentionToken typeRef . Count <$> [1 .. occ])
              & Set.fromList
   in (normalizedVarTokens <> expandedTypeRefTokens, arity)
  where
    initState = TokenGenState 0
    -- Algebra for collecting type reference tokens and arity from a type signature.
    alg ::
      ABT.Term' Type.F v ann (TokenGenM v (MonoidalMap (Either VarId TypeReference) (Occurrence, Any {- Is return type -}), Sum Arity)) ->
      TokenGenM v (MonoidalMap (Either VarId TypeReference) (Occurrence, Any {- Is return type -}), Sum Arity)
    alg =
      ABT.out' >>> \case
        -- Type Var usage
        ABT.Var v -> do
          vId <- varIdFor v
          pure $ (MonMap.singleton (Left vId) (1, Any True), mempty)
        ABT.Cycle a -> a
        -- Type Var scoping. Assign this var to a var id within this scope.
        ABT.Abs v r -> do
          vId <- nextVarId
          local (field @"varIds" . at v ?~ vId) r
        ABT.Tm tf -> case tf of
          -- Type reference mention
          Type.Ref typeRef -> do
            pure $ (MonMap.singleton (Right typeRef) (1, Any True), mempty)
          -- Arrow increases the arity of the type signature, we only use the arity of the RHS
          -- due to how arrows are associated, this sorts out types like `(a -> b) -> c` where
          -- the arity needs to be 1, not 2.
          Type.Arrow a b -> do
            -- Anything on the left of an arrow is not a return type.
            aTokens <- a <&> \(toks, _arity) -> MonMap.map (\(occ, _) -> (occ, Any False)) toks
            (bTokens, arity) <- b
            pure $ (aTokens <> bTokens, arity + 1)
          Type.Ann a _kind -> a
          -- At the moment we don't handle higher kinded type applications differently than regular
          -- type mentions.
          Type.App a b -> do
            aTokens <- a
            bTokens <- b
            pure $ aTokens <> bTokens
          Type.Effect a b -> do
            -- Don't include vars from effects, people often leave off the `{g}` in types, so
            -- including effect vars would throw off type-directed search a lot.
            aTokens <- removeVars . fst <$> a
            (bTokens, bArity) <- b
            pure $ (aTokens <> bTokens, bArity)
          Type.Effects as -> first removeVars <$> Monoid.foldMapM id as
          Type.Forall a -> a
          Type.IntroOuter a -> a
    removeVars :: MonoidalMap (Either VarId TypeReference) (Occurrence, Any) -> MonoidalMap (Either VarId TypeReference) (Occurrence, Any)
    removeVars = MonMap.filterWithKey (\k _ -> isRight k)
    nextVarId :: TokenGenM v VarId
    nextVarId = field @"nextVarId" <<%= succ
    varIdFor :: v -> TokenGenM v VarId
    varIdFor v = do
      asks (Map.lookup v . varIds) >>= \case
        Just vid -> pure vid
        Nothing -> do
          -- Should be impossible
          error "typeRefTokens: Found variable without corresponding Abs in type signature"

syncTypes ::
  (PG.QueryM m) =>
  Codebase.CodebaseEnv ->
  NamesPerspective m ->
  BranchHashId ->
  Cursors.PGCursor (Name, TypeReference) ->
  m ([DefnIndexingFailure], [Text])
syncTypes codebase namesPerspective rootBranchHashId typesCursor = do
  Cursors.foldBatched typesCursor defnBatchSize \types -> do
    (errs, refDocs) <-
      types
        -- Most lib names are already filtered out by using the name lookup; but sometimes
        -- when libs aren't at the project root some can slip through, so we remove them.
        & V.filter
          ( \(fqn, _) -> not (libSegment `elem` (NEL.toList $ Name.reverseSegments fqn))
          )
        & foldMapM \(fqn, ref) -> fmap (either (\err -> ([err], [])) (\doc -> ([], [doc]))) . runExceptT $ do
          (declTokens, declArity) <- case ref of
            Reference.Builtin _ -> pure (mempty, Arity 0)
            Reference.DerivedId refId -> do
              -- TODO: batchify this
              decl <- lift (Codebase.loadV1TypeDeclarationsByRefIdsOf codebase id refId) `whenNothingM` throwError (NoDeclForType fqn ref)
              pure $ (tokensForDecl refId decl, Arity . fromIntegral . length . DD.bound $ DD.asDataDecl decl)
          let basicTokens = Set.fromList [NameToken fqn, HashToken $ Reference.toShortHash ref]
          typeSummary <- lift $ Summary.typeSummariesForReferencesOf codebase Nothing id (ref, Just fqn)
          let sh = Reference.toShortHash ref
          let dd =
                DefinitionDocument
                  { rootBranchHashId,
                    fqn,
                    hash = sh,
                    tokens = declTokens <> basicTokens,
                    arity = declArity,
                    tag = ToTTypeTag (typeSummary.tag),
                    metadata = ToTTypeSummary typeSummary
                  }
          pure dd
    -- It's much more efficient to build only one PPE per batch.
    let allDeps = setOf (folded . folding tokens . folded . to LD.TypeReference) refDocs
    pped <- PPEPostgres.ppedForReferences namesPerspective allDeps
    let ppe = PPED.unsuffixifiedPPE pped
    let namedDocs :: [DefinitionDocument Name (Name, ShortHash)]
        namedDocs =
          refDocs
            & traversed . field @"tokens" %~ Set.mapMaybe \token -> do
              for token \ref -> do
                name <- PPE.types ppe ref
                pure $ (HQ'.toName name, Reference.toShortHash ref)
    badNames <- DDQ.insertDefinitionDocuments namedDocs
    pure (errs, badNames)

-- | Compute the search tokens for a type declaration.
-- Note that constructors are handled separately when syncing terms.
tokensForDecl :: Reference.TypeReferenceId -> DD.Decl v a -> Set (DefnSearchToken TypeReference)
tokensForDecl _typeRefId decl = do
  let ddecl = DD.asDataDecl decl
      tagToken = case DD.constructorType decl of
        CT.Data -> TypeTagToken Server.Types.Data
        CT.Effect -> TypeTagToken Server.Types.Ability
      modToken = TypeModToken $ DD.modifier ddecl
   in Set.fromList $ [tagToken, modToken]
