{-# LANGUAGE DataKinds #-}

module Share.BackgroundJobs.Search.DefinitionSync (worker) where

import Control.Lens
import Control.Monad.Except
import Data.Either (fromLeft)
import Data.Generics.Product (HasField (..))
import Data.Map qualified as Map
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonMap
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Ki.Unlifted qualified as Ki
import Share.BackgroundJobs.Monad (Background)
import Share.BackgroundJobs.Search.DefinitionSync.Types (DefinitionDocument (..), DefnSearchToken (..), Occurrence (Occurrence), TermOrTypeSummary (..), VarId)
import Share.BackgroundJobs.Workers (newWorker)
import Share.Codebase (CodebaseM)
import Share.Codebase qualified as Codebase
import Share.IDs (ProjectId, ProjectShortHand (ProjectShortHand), ReleaseId, ReleaseVersion)
import Share.Postgres (QueryM (transactionUnsafeIO))
import Share.Postgres qualified as PG
import Share.Postgres.Cursors qualified as Cursors
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (BranchHashId, ComponentHash)
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
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Reference (TypeReference)
import Unison.Server.Share.DefinitionSummary qualified as Summary
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

data DefnIndexingFailure
  = NoTypeSigForTerm Name Referent

-- | How often to poll for new releases to sync in seconds.
pollingIntervalSeconds :: Int
pollingIntervalSeconds = 10

-- | How many definitions to hold in memory at a time while syncing
defnBatchSize :: Int32
defnBatchSize = 10

worker :: Ki.Scope -> Background ()
worker scope = newWorker scope "search:defn-sync" $ forever do
  liftIO $ UnliftIO.threadDelay $ pollingIntervalSeconds * 1000000
  Logging.logInfoText "Syncing definitions..."
  authZReceipt <- AuthZ.backgroundJobAuthZ
  PG.runTransaction $ do
    mayReleaseId <- DefnSyncQ.claimUnsyncedRelease
    for_ mayReleaseId (syncRelease authZReceipt syncDD)

syncRelease ::
  AuthZ.AuthZReceipt ->
  ReleaseId ->
  PG.Transaction e ()
syncRelease authZReceipt releaseId = fmap (fromMaybe ()) . runMaybeT $ do
  Release {projectId, releaseId, squashedCausal, version = releaseVersion} <- lift $ PG.expectReleaseById releaseId
  Project {slug, ownerUserId, visibility = projectVis} <- lift $ PG.expectProjectById projectId
  User {handle, visibility = userVis} <- PG.expectUserByUserId ownerUserId
  -- Don't sync private projects
  guard $ projectVis == ProjectPublic
  -- Don't sync private users
  guard $ userVis == UserPublic
  lift $ do
    bhId <- HashQ.expectNamespaceIdsByCausalIdsOf id squashedCausal
    namesPerspective <- NLOps.namesPerspectiveForRootAndPath bhId (NL.PathSegments [])
    let nlReceipt = NL.nameLookupReceipt namesPerspective
    let codebaseLoc = Codebase.codebaseLocationForProjectRelease ownerUserId
    let codebase = Codebase.codebaseEnv authZReceipt codebaseLoc
    Codebase.codebaseMToTransaction codebase $ do
      termsCursor <- lift $ NLOps.termsWithinNamespace nlReceipt bhId
      let projectShortHand = ProjectShortHand handle slug
      syncTerms namesPerspective bhId projectId releaseId termsCursor
      typesCursor <- lift $ NLOps.typesWithinNamespace nlReceipt bhId
      syncTypes projectShortHand releaseVersion typesCursor

syncTerms ::
  NL.NamesPerspective ->
  BranchHashId ->
  ProjectId ->
  ReleaseId ->
  Cursors.PGCursor (Name, Referent) ->
  CodebaseM e [DefnIndexingFailure]
syncTerms namesPerspective bhId projectId releaseId termsCursor =
  Cursors.foldBatched termsCursor defnBatchSize \terms -> do
    (errs, docs) <-
      terms & foldMapM \(fqn, ref) -> fmap (either (\err -> ([err], [])) (\doc -> ([], [doc]))) . runExceptT $ do
        typ <- lift (Codebase.loadTypeOfReferent ref) `whenNothingM` throwError (NoTypeSigForTerm fqn ref)
        termSummary <- lift $ Summary.termSummaryForReferent ref typ (Just fqn) bhId Nothing Nothing
        let sh = Referent.toShortHash ref
        let refTokens = tokensForTerm fqn ref typ termSummary
        let deps = setOf (folded . folded . to LD.TypeReference) refTokens
        pped <- PPEPostgres.ppedForReferences namesPerspective deps
        let ppe = PPED.unsuffixifiedPPE pped
        let namedTokens =
              refTokens & Set.mapMaybe \token -> do
                fqn <- traverse (PPE.types ppe) token
                pure $ Name.lastSegment . HQ'.toName <$> fqn
        let dd =
              DefinitionDocument
                { project = projectId,
                  release = releaseId,
                  fqn,
                  hash = sh,
                  tokens = namedTokens,
                  metadata = ToTTermSummary termSummary
                }
        pure dd
    lift $ DDQ.insertDefinitionDocuments docs
    pure errs

-- | Compute the search tokens for a term given its name, hash, and type signature
tokensForTerm :: Name -> Referent -> Type.Type v a -> Summary.TermSummary -> Set (DefnSearchToken TypeReference)
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
      occMap = flip evalState initState . runReaderT mempty $ ABT.cata alg typ
   in MonMap.toList occMap & foldMap \case
        (Left vId, occ) -> Set.singleton (TypeVarToken vId occ)
        (Right typeRef, occ) -> Set.singleton (TypeMentionToken typeRef occ)
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

syncTypes = undefined
