{-# LANGUAGE DataKinds #-}

module Share.BackgroundJobs.Search.DefinitionSync (worker) where

import Control.Lens
import Control.Monad.Except
import Data.Generics.Product (HasField (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonMap
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync.Types (DefinitionDocument (..), DefnSearchToken (..), Occurrence, TermOrTypeSummary (..), VarId (VarId))
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase (CodebaseM)
import Share.Codebase qualified as Codebase
import Share.IDs (ProjectId, ReleaseId)
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Postgres.Queries qualified as PG
import Share.Postgres.Search.DefinitionSync qualified as DDQ
import Share.Postgres.Search.DefinitionSync qualified as DefnSyncQ
import Share.Prelude
import Share.Project (Project (..), ProjectVisibility (..))
import Share.Release (Release (..))
import Share.User (User (..), UserVisibility (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.Authorization qualified as AuthZ
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.ABT qualified as ABT
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Reference (TypeReference)
import Unison.Reference qualified as Reference
import Unison.Server.Share.DefinitionSummary qualified as Summary
import Unison.Server.Types qualified as Server.Types
import Unison.ShortHash (ShortHash)
import Unison.Type qualified as Type
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var
import UnliftIO.Concurrent qualified as UnliftIO

data DefnIndexingFailure
  = NoTypeSigForTerm Name Referent
  | NoDeclForType Name TypeReference

-- | How often to poll for new releases to sync in seconds.
pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

-- | How many definitions to hold in memory at a time while syncing
defnBatchSize :: Int32
defnBatchSize = 1000

worker :: Ki.Scope -> Background ()
worker scope = newWorker scope "search:defn-sync" $ forever do
  Logging.logInfoText "Syncing definitions..."
  authZReceipt <- AuthZ.backgroundJobAuthZ
  PG.runTransaction $ do
    mayReleaseId <- DefnSyncQ.claimUnsyncedRelease
    Debug.debugM Debug.Temp "Syncing release" mayReleaseId
    for_ mayReleaseId (syncRelease authZReceipt)
  liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000

syncRelease ::
  AuthZ.AuthZReceipt ->
  ReleaseId ->
  PG.Transaction e [DefnIndexingFailure]
syncRelease authZReceipt releaseId = fmap (fromMaybe []) . runMaybeT $ do
  Release {projectId, releaseId, squashedCausal} <- lift $ PG.expectReleaseById releaseId
  Project {ownerUserId, visibility = projectVis} <- lift $ PG.expectProjectById projectId
  User {visibility = userVis} <- PG.expectUserByUserId ownerUserId
  -- Don't sync private projects
  guard $ projectVis == ProjectPublic
  -- Don't sync private users
  guard $ userVis == UserPublic
  Debug.debugM Debug.Temp "Syncing release" releaseId
  lift $ do
    bhId <- HashQ.expectNamespaceIdsByCausalIdsOf id squashedCausal
    Debug.debugM Debug.Temp "bhId" bhId
    namesPerspective <- NLOps.namesPerspectiveForRootAndPath bhId (NL.PathSegments [])
    let nlReceipt = NL.nameLookupReceipt namesPerspective
    let codebaseLoc = Codebase.codebaseLocationForProjectRelease ownerUserId
    let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
    Codebase.codebaseMToTransaction codebase $ do
      Debug.debugM Debug.Temp "Building cursor" releaseId
      termsCursor <- lift $ NLOps.termsWithinNamespace nlReceipt bhId
      Debug.debugM Debug.Temp "Syncing terms" releaseId
      termErrs <- syncTerms namesPerspective bhId projectId releaseId termsCursor
      typesCursor <- lift $ NLOps.typesWithinNamespace nlReceipt bhId
      Debug.debugM Debug.Temp "Syncing types" releaseId
      typeErrs <- syncTypes namesPerspective projectId releaseId typesCursor
      pure (termErrs <> typeErrs)

syncTerms ::
  NL.NamesPerspective ->
  BranchHashId ->
  ProjectId ->
  ReleaseId ->
  Cursors.PGCursor (Name, Referent) ->
  CodebaseM e [DefnIndexingFailure]
syncTerms namesPerspective bhId projectId releaseId termsCursor = do
  Cursors.foldBatched termsCursor defnBatchSize \terms -> do
    Debug.debugM Debug.Temp "Fetched N more terms" (length terms)
    (errs, refDocs) <-
      terms & foldMapM \(fqn, ref) -> fmap (either (\err -> ([err], [])) (\doc -> ([], [doc]))) . runExceptT $ do
        typ <- lift (Codebase.loadTypeOfReferent ref) `whenNothingM` throwError (NoTypeSigForTerm fqn ref)
        termSummary <- lift $ Summary.termSummaryForReferent ref typ (Just fqn) bhId Nothing Nothing
        let sh = Referent.toShortHash ref
        let refTokens = tokensForTerm fqn ref typ termSummary
        let dd =
              DefinitionDocument
                { project = projectId,
                  release = releaseId,
                  fqn,
                  hash = sh,
                  tokens = refTokens,
                  metadata = ToTTermSummary termSummary
                }
        pure dd

    -- It's much more efficient to build only one PPE per batch.
    let allDeps = setOf (folded . folding tokens . folded . to LD.TypeReference) refDocs
    pped <- PPEPostgres.ppedForReferences namesPerspective allDeps
    let ppe = PPED.unsuffixifiedPPE pped
    let namedDocs :: [DefinitionDocument ProjectId ReleaseId Name (NameSegment, ShortHash)]
        namedDocs =
          refDocs
            & traversed . field @"tokens" %~ Set.mapMaybe \token -> do
              for token \ref -> do
                name <- PPE.types ppe ref
                pure $ (Name.lastSegment . HQ'.toName $ name, Reference.toShortHash ref)
    lift $ DDQ.insertDefinitionDocuments namedDocs
    pure errs

-- | Compute the search tokens for a term given its name, hash, and type signature
tokensForTerm :: (Var.Var v) => Name -> Referent -> Type.Type v a -> Summary.TermSummary -> Set (DefnSearchToken TypeReference)
tokensForTerm name ref typ (Summary.TermSummary {tag}) = do
  let sh = Referent.toShortHash ref
      baseTokens = Set.fromList [NameToken name, HashToken sh]
      tagTokens = Set.singleton $ TermTagToken tag
   in baseTokens <> typeSigTokens typ <> tagTokens

data TokenGenEnv v = TokenGenEnv
  { varIds :: Map v VarId
  }
  deriving stock (Show, Generic)

data TokenGenState v = TokenGenState
  { nextVarId :: VarId
  }
  deriving stock (Show, Generic)

type TokenGenM v = ReaderT (TokenGenEnv v) (State (TokenGenState v))

-- | Compute var occurrence and type ref occurrence search tokens from a type signature.
typeSigTokens :: forall v ann. (Var.Var v) => Type.Type v ann -> Set (DefnSearchToken TypeReference)
typeSigTokens typ =
  let occMap :: MonoidalMap (Either VarId TypeReference) Occurrence
      occMap = flip evalState initState . flip runReaderT (TokenGenEnv mempty) $ ABT.cata alg typ
      (varIds, typeRefs) =
        MonMap.toList occMap & foldMap \case
          (Left vId, occ) -> ([(vId, occ)], [])
          (Right typeRef, occ) -> ([], [(typeRef, occ)])
      expandedVarTokens =
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
          & foldMap (\(vId, occ) -> (TypeVarToken vId) <$> [1 .. occ])
          & Set.fromList
      expandedTypeRefTokens =
        typeRefs
          & foldMap \(typeRef, occ) ->
            TypeMentionToken typeRef <$> [1 .. occ]
              & Set.fromList
   in expandedVarTokens <> expandedTypeRefTokens
  where
    initState = TokenGenState 0
    -- Cata algebra for collecting type reference tokens from a type signature.
    alg ::
      ann ->
      ABT.ABT Type.F v (TokenGenM v (MonoidalMap (Either VarId TypeReference) Occurrence)) ->
      TokenGenM v (MonoidalMap (Either VarId TypeReference) Occurrence)
    alg _ann = \case
      ABT.Var v -> do
        vId <- varIdFor v
        pure $ MonMap.singleton (Left vId) 1
      ABT.Cycle a -> a
      ABT.Abs v r -> do
        vId <- nextVarId
        local (field @"varIds" . at v ?~ vId) r
      ABT.Tm tf -> case tf of
        Type.Ref typeRef -> do
          pure $ MonMap.singleton (Right typeRef) 1
        Type.Arrow a b -> do
          aTokens <- a
          bTokens <- b
          pure $ aTokens <> bTokens
        Type.Ann a _kind -> a
        -- At the moment we don't handle higher kinded type applications differently than regular
        -- type mentions.
        Type.App a b -> do
          aTokens <- a
          bTokens <- b
          pure $ aTokens <> bTokens
        Type.Effect a b -> do
          aTokens <- a
          bTokens <- b
          pure $ aTokens <> bTokens
        Type.Effects as -> Monoid.foldMapM id as
        Type.Forall a -> a
        Type.IntroOuter a -> a
    nextVarId :: TokenGenM v VarId
    nextVarId = field @"nextVarId" <<%= succ
    varIdFor :: v -> TokenGenM v VarId
    varIdFor v = do
      asks (Map.lookup v . varIds) >>= \case
        Just vid -> pure vid
        Nothing -> do
          error "typeRefTokens: Found variable without corresponding Abs in type signature"

syncTypes ::
  NL.NamesPerspective ->
  ProjectId ->
  ReleaseId ->
  Cursors.PGCursor (Name, TypeReference) ->
  CodebaseM e [DefnIndexingFailure]
syncTypes namesPerspective projectId releaseId typesCursor = do
  Cursors.foldBatched typesCursor defnBatchSize \types -> do
    (errs, refDocs) <-
      types & foldMapM \(fqn, ref) -> fmap (either (\err -> ([err], [])) (\doc -> ([], [doc]))) . runExceptT $ do
        declTokens <- case ref of
          Reference.Builtin _ -> pure mempty
          Reference.DerivedId refId -> do
            decl <- lift (Codebase.loadTypeDeclaration refId) `whenNothingM` throwError (NoDeclForType fqn ref)
            pure $ tokensForDecl refId decl
        let basicTokens = Set.fromList [NameToken fqn, HashToken $ Reference.toShortHash ref]

        typeSummary <- lift $ Summary.typeSummaryForReference ref (Just fqn) Nothing
        let sh = Reference.toShortHash ref
        let dd =
              DefinitionDocument
                { project = projectId,
                  release = releaseId,
                  fqn,
                  hash = sh,
                  tokens = declTokens <> basicTokens,
                  metadata = ToTTypeSummary typeSummary
                }
        pure dd
    -- It's much more efficient to build only one PPE per batch.
    let allDeps = setOf (folded . folding tokens . folded . to LD.TypeReference) refDocs
    pped <- PPEPostgres.ppedForReferences namesPerspective allDeps
    let ppe = PPED.unsuffixifiedPPE pped
    let namedDocs :: [DefinitionDocument ProjectId ReleaseId Name (NameSegment, ShortHash)]
        namedDocs =
          refDocs
            & traversed . field @"tokens" %~ Set.mapMaybe \token -> do
              for token \ref -> do
                name <- PPE.types ppe ref
                pure $ (Name.lastSegment . HQ'.toName $ name, Reference.toShortHash ref)
    lift $ DDQ.insertDefinitionDocuments namedDocs
    pure errs

tokensForDecl :: Reference.TypeReferenceId -> DD.Decl v a -> Set (DefnSearchToken TypeReference)
tokensForDecl _typeRefId decl = do
  let ddecl = DD.asDataDecl decl
      tagToken = case DD.constructorType decl of
        CT.Data -> TypeTagToken Server.Types.Data
        CT.Effect -> TypeTagToken Server.Types.Ability
      modToken = TypeModToken $ DD.modifier ddecl
   in -- Include the constructors as Name Tokens
      -- constructorTokens =
      --   DD.declConstructorReferents typeRefId decl
      --     & (fmap . fmap) Reference.DerivedId
      --     <&> \constructorReferent -> ConstructorToken constructorReferent
      Set.fromList $ [tagToken, modToken]
