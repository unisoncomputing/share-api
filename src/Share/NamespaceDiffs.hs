-- | Logic for computing the differerences between two namespaces,
-- typically used when showing the differences caused by a contribution.
module Share.NamespaceDiffs
  ( NamespaceDiffResult (..),
    NamespaceAndLibdepsDiff,
    GNamespaceAndLibdepsDiff (..),
    NamespaceTreeDiff,
    GNamespaceTreeDiff,
    DiffAtPath (..),
    NamespaceDiffError (..),
    DefinitionDiff (..),
    DefinitionDiffKind (..),
    computeThreeWayNamespaceDiff,
    compressNameTree,
    namespaceTreeDiffReferences_,
    namespaceTreeDiffReferents_,
    namespaceTreeDiffTermDiffs_,
    witherNamespaceTreeDiffTermDiffs,
    namespaceTreeDiffTypeDiffs_,
    namespaceTreeDiffRenderedTerms_,
    namespaceTreeDiffRenderedTypes_,
    namespaceTreeTermDiffKinds_,
    witherNamespaceTreeTermDiffKinds,
    namespaceTreeTypeDiffKinds_,
    definitionDiffRendered_,
    definitionDiffRefs_,
    definitionDiffDiffs_,
    witherDiffAtPathTermDiffs,
    definitionDiffKindRefs_,
    definitionDiffKindDiffs_,
    definitionDiffKindRendered_,
    namespaceAndLibdepsDiffDefns_,
    namespaceAndLibdepsDiffLibdeps_,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((.=), (:<))
import Control.Monad.Except
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (ToJSON (..))
import Data.Align (Semialign (..))
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Functor.Compose (Compose (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Servant (err400, err404, err500)
import Share.Codebase qualified as Codebase
import Share.IDs (UserId)
import Share.Names.Postgres qualified as PGNames
import Share.Postgres qualified as PG
import Share.Postgres.Definitions.Queries qualified as DefnsQ
import Share.Postgres.IDs (BranchHash, BranchHashId)
import Share.Postgres.NameLookups.Ops qualified as NL
import Share.Postgres.NameLookups.Types (NameLookupReceipt)
import Share.Postgres.NameLookups.Types qualified as NL
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.DataDeclaration (Decl)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Merge (DiffOp, EitherWay, IncoherentDeclReason (..), Mergeblob0, Mergeblob1, ThreeWay (..), TwoOrThreeWay (..), TwoWay (..))
import Unison.Merge qualified as Merge
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Mergeblob1 qualified as Mergeblob1
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Types (DisplayObjectDiff (..), TermDefinition, TermDefinitionDiff (..), TermTag, TypeDefinition, TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF3, alignDefnsWith)
import Unison.Util.Nametree (Nametree (..))
import Unison.Util.Set qualified as Set

data NamespaceDiffError
  = ImpossibleError Text
  | IncoherentDecl (EitherWay IncoherentDeclReason)
  | LibFoundAtUnexpectedPath Path
  | MissingEntityError EntityMissing
  deriving stock (Eq, Show)

instance ToServerError NamespaceDiffError where
  toServerError = \case
    ImpossibleError {} -> (ErrorID "namespace-diff:impossible-error", err500)
    IncoherentDecl {} -> (ErrorID "namespace-diff:incoherent-decl", err400)
    LibFoundAtUnexpectedPath {} -> (ErrorID "namespace-diff:lib-at-unexpected-path", err400)
    MissingEntityError (EntityMissing eId _msg) -> (eId, err404)

instance Logging.Loggable NamespaceDiffError where
  toLog = \case
    (ImpossibleError t) ->
      Logging.textLog t
        & Logging.withSeverity Logging.Error
    (IncoherentDecl _) -> Logging.textLog "couldn't diff namespaces due to incoherent decl"
    (LibFoundAtUnexpectedPath _) -> Logging.textLog "couldn't diff namespaces due to lib found at unexpected path"
    (MissingEntityError e) -> Logging.toLog e

-- | The differences between two namespaces.
data DefinitionDiffs name r = DefinitionDiffs
  { -- Brand new added terms, neither the name nor definition exist in the old namespace.
    added :: Map name r,
    -- Removed terms. These names for these definitions were removed, and there are no newly
    -- added names for these definitions.
    removed :: Map name r,
    -- Updated terms, split into non-propagated (`updated`) and propagated (`propagated`) updates. These names exist in
    -- both the old and new namespace, but the definitions assigned to them have changed.
    updated :: Map name (r {- old -}, r {- new -}),
    propagated :: Map name (r {- old -}, r {- new -}),
    -- Renamed terms. These definitions exist in both the old and new namespace, but the names have
    -- changed.
    renamed :: Map r (NESet name {- old names for this ref -}, NESet name {- new names for this ref -}),
    -- New aliases. These definitions exist in both the old namespace, but have received new names
    -- in the new namespace without removing the old ones.
    newAliases :: Map r (NESet name {- Existing names for this ref -}, NESet name)
  }
  deriving stock (Eq, Show)

data DefinitionDiff r rendered diff = DefinitionDiff
  { kind :: DefinitionDiffKind r rendered diff,
    -- The fully qualified name of the definition we're concerned with.
    fqn :: Name
  }
  deriving stock (Eq, Show, Ord)

definitionDiffKind_ :: Lens (DefinitionDiff r rendered diff) (DefinitionDiff r' rendered' diff') (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r' rendered' diff')
definitionDiffKind_ = lens getter setter
  where
    getter (DefinitionDiff k _) = k
    setter (DefinitionDiff _ n) k = DefinitionDiff k n

definitionDiffRefs_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r' rendered diff) r r'
definitionDiffRefs_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindRefs_ f k <*> pure n

definitionDiffDiffs_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r rendered diff') diff diff'
definitionDiffDiffs_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindDiffs_ f k <*> pure n

definitionDiffRendered_ :: Traversal (DefinitionDiff r rendered diff) (DefinitionDiff r rendered' diff) rendered rendered'
definitionDiffRendered_ f (DefinitionDiff k n) = DefinitionDiff <$> definitionDiffKindRendered_ f k <*> pure n

-- | Information about a single definition which is different.
data DefinitionDiffKind r rendered diff
  = Added r rendered
  | NewAlias r (NESet Name {- existing names -}) rendered
  | Removed r rendered
  | -- | A non-propagated update, where old and new have different syntactic hashes.
    Updated r {- old -} r {- new -} diff
  | -- | A propagated update (old and new are different but have the same syntactic hash)
    Propagated r {- old -} r {- new -} diff
  | -- This definition was removed away from this location and added at the provided names.
    RenamedTo r (NESet Name) rendered
  | -- This definition was added at this location and removed from the provided names.
    RenamedFrom r (NESet Name) rendered
  deriving stock (Eq, Show, Ord)

instance (Ord r) => Semigroup (DefinitionDiffs Name r) where
  d1 <> d2 =
    DefinitionDiffs
      { added = added d1 <> added d2,
        removed = removed d1 <> removed d2,
        updated = updated d1 <> updated d2,
        propagated = propagated d1 <> propagated d2,
        renamed = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (renamed d1) (renamed d2),
        newAliases = Map.unionWith (\(a1, b1) (a2, b2) -> (a1 <> a2, b1 <> b2)) (newAliases d1) (newAliases d2)
      }

instance (Ord r) => Monoid (DefinitionDiffs Name r) where
  mempty =
    DefinitionDiffs
      { added = mempty,
        removed = mempty,
        updated = mempty,
        propagated = mempty,
        renamed = mempty,
        newAliases = mempty
      }

definitionDiffKindRefs_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r' rendered diff) r r'
definitionDiffKindRefs_ f = \case
  Added r rendered -> Added <$> f r <*> pure rendered
  NewAlias r ns rendered -> NewAlias <$> f r <*> pure ns <*> pure rendered
  Removed r rendered -> Removed <$> f r <*> pure rendered
  Propagated old new diff -> Propagated <$> f old <*> f new <*> pure diff
  Updated old new diff -> Updated <$> f old <*> f new <*> pure diff
  RenamedTo r old rendered -> RenamedTo <$> f r <*> pure old <*> pure rendered
  RenamedFrom r old rendered -> RenamedFrom <$> f r <*> pure old <*> pure rendered

definitionDiffKindDiffs_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r rendered diff') diff diff'
definitionDiffKindDiffs_ f = \case
  Added r rendered -> Added r <$> pure rendered
  NewAlias r ns rendered -> NewAlias r ns <$> pure rendered
  Removed r rendered -> Removed r <$> pure rendered
  Propagated old new diff -> Propagated old new <$> f diff
  Updated old new diff -> Updated old new <$> f diff
  RenamedTo r old rendered -> RenamedTo r old <$> pure rendered
  RenamedFrom r old rendered -> RenamedFrom r old <$> pure rendered

definitionDiffKindRendered_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r rendered' diff) rendered rendered'
definitionDiffKindRendered_ f = \case
  Added r rendered -> Added r <$> f rendered
  NewAlias r ns rendered -> NewAlias r ns <$> f rendered
  Removed r rendered -> Removed r <$> f rendered
  Propagated old new diff -> Propagated old new <$> pure diff
  Updated old new diff -> Updated old new <$> pure diff
  RenamedTo r old rendered -> RenamedTo r old <$> f rendered
  RenamedFrom r old rendered -> RenamedFrom r old <$> f rendered

data NamespaceDiffResult
  = NamespaceDiffResult'Ok
      ( NamespaceAndLibdepsDiff
          (TermTag, ShortHash)
          (TypeTag, ShortHash)
          TermDefinition
          TypeDefinition
          TermDefinitionDiff
          TypeDefinitionDiff
          BranchHash
      )
  | NamespaceDiffResult'Err NamespaceDiffError

instance ToJSON NamespaceDiffResult where
  toJSON = \case
    NamespaceDiffResult'Ok diff ->
      Aeson.object
        [ "defns" .= namespaceTreeDiffJSON diff.defns,
          "libdeps" .= libdepsDiffJSON diff.libdeps,
          "tag" .= ("ok" :: Text)
        ]
    NamespaceDiffResult'Err err ->
      Aeson.object
        [ "error" .= errValue,
          "tag" .= ("error" :: Text)
        ]
      where
        errValue =
          case err of
            ImpossibleError _ ->
              Aeson.object
                [ "errorKind" .= ("impossibleError" :: Text)
                ]
            IncoherentDecl reason ->
              let f :: Text -> IncoherentDeclReason -> Aeson.Value
                  f which reason =
                    Aeson.object
                      ( "oldOrNewBranch" .= which
                          : case reason of
                            IncoherentDeclReason'ConstructorAlias typeName constructorName1 constructorName2 ->
                              [ "errorKind" .= ("constructorAlias" :: Text),
                                "typeName" .= typeName,
                                "constructorName1" .= constructorName1,
                                "constructorName2" .= constructorName2
                              ]
                            IncoherentDeclReason'MissingConstructorName typeName ->
                              [ "errorKind" .= ("missingConstructorName" :: Text),
                                "typeName" .= typeName
                              ]
                            IncoherentDeclReason'NestedDeclAlias constructorName1 constructorName2 ->
                              [ "errorKind" .= ("constructorAlias" :: Text),
                                "constructorName1" .= constructorName1,
                                "constructorName2" .= constructorName2
                              ]
                            IncoherentDeclReason'StrayConstructor _ constructorName ->
                              [ "errorKind" .= ("strayConstructor" :: Text),
                                "constructorName" .= constructorName
                              ]
                      )
               in case reason of
                    EitherWay.Alice reason -> f "old" reason
                    EitherWay.Bob reason -> f "new" reason
            LibFoundAtUnexpectedPath _ ->
              Aeson.object
                [ "errorKind" .= ("libFoundAtUnexpectedPath" :: Text)
                ]
            MissingEntityError _ ->
              Aeson.object
                [ "errorKind" .= ("missingEntityError" :: Text)
                ]
    where
      text :: Text -> Text
      text t = t
      hqNameJSON :: Name -> NameSegment -> ShortHash -> Aeson.Value -> Aeson.Value
      hqNameJSON fqn name sh rendered = Aeson.object ["hash" .= sh, "shortName" .= name, "fullName" .= fqn, "rendered" .= rendered]
      -- The preferred frontend format is a bit clunky to calculate here:
      diffDataJSON :: (ToJSON tag) => NameSegment -> DefinitionDiff (tag, ShortHash) Aeson.Value Aeson.Value -> (tag, Aeson.Value)
      diffDataJSON shortName (DefinitionDiff {fqn, kind}) = case kind of
        Added (defnTag, r) rendered -> (defnTag, Aeson.object ["tag" .= text "Added", "contents" .= hqNameJSON fqn shortName r rendered])
        NewAlias (defnTag, r) existingNames rendered ->
          let contents = Aeson.object ["hash" .= r, "aliasShortName" .= shortName, "aliasFullName" .= fqn, "otherNames" .= toList existingNames, "rendered" .= rendered]
           in (defnTag, Aeson.object ["tag" .= text "Aliased", "contents" .= contents])
        Removed (defnTag, r) rendered -> (defnTag, Aeson.object ["tag" .= text "Removed", "contents" .= hqNameJSON fqn shortName r rendered])
        Updated (oldTag, oldRef) (newTag, newRef) diffVal ->
          let contents = Aeson.object ["oldHash" .= oldRef, "newHash" .= newRef, "shortName" .= shortName, "fullName" .= fqn, "oldTag" .= oldTag, "newTag" .= newTag, "diff" .= diffVal]
           in (newTag, Aeson.object ["tag" .= text "Updated", "contents" .= contents])
        Propagated (oldTag, oldRef) (newTag, newRef) diffVal ->
          let contents = Aeson.object ["oldHash" .= oldRef, "newHash" .= newRef, "shortName" .= shortName, "fullName" .= fqn, "oldTag" .= oldTag, "newTag" .= newTag, "diff" .= diffVal]
           in (newTag, Aeson.object ["tag" .= text "Propagated", "contents" .= contents])
        RenamedTo (defnTag, r) newNames rendered ->
          let contents = Aeson.object ["oldShortName" .= shortName, "oldFullName" .= fqn, "newNames" .= newNames, "hash" .= r, "rendered" .= rendered]
           in (defnTag, Aeson.object ["tag" .= text "RenamedTo", "contents" .= contents])
        RenamedFrom (defnTag, r) oldNames rendered ->
          let contents = Aeson.object ["oldNames" .= oldNames, "newShortName" .= shortName, "newFullName" .= fqn, "hash" .= r, "rendered" .= rendered]
           in (defnTag, Aeson.object ["tag" .= text "RenamedFrom", "contents" .= contents])
      displayObjectDiffToJSON :: DisplayObjectDiff -> Aeson.Value
      displayObjectDiffToJSON = \case
        DisplayObjectDiff dispDiff ->
          Aeson.object ["diff" .= dispDiff, "diffKind" .= ("diff" :: Text)]
        MismatchedDisplayObjects {} ->
          Aeson.object ["diffKind" .= ("mismatched" :: Text)]

      termDefinitionDiffToJSON :: TermDefinitionDiff -> Aeson.Value
      termDefinitionDiffToJSON (TermDefinitionDiff {left, right, diff}) = Aeson.object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]

      typeDefinitionDiffToJSON :: TypeDefinitionDiff -> Aeson.Value
      typeDefinitionDiffToJSON (TypeDefinitionDiff {left, right, diff}) = Aeson.object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]

      namespaceTreeDiffJSON ::
        NamespaceTreeDiff
          (TermTag, ShortHash)
          (TypeTag, ShortHash)
          TermDefinition
          TypeDefinition
          TermDefinitionDiff
          TypeDefinitionDiff ->
        Aeson.Value
      namespaceTreeDiffJSON (diffs Cofree.:< children) =
        let changesJSON =
              diffs
                & Map.toList
                & foldMap
                  ( \(name, DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) ->
                      ( Foldable.toList termDiffsAtPath
                          <&> over definitionDiffDiffs_ termDefinitionDiffToJSON
                          <&> over definitionDiffRendered_ toJSON
                          & fmap (diffDataJSON name)
                          & fmap (\(tag, dJSON) -> Aeson.object ["tag" .= tag, "contents" .= dJSON])
                      )
                        <> ( Foldable.toList typeDiffsAtPath
                               <&> over definitionDiffDiffs_ typeDefinitionDiffToJSON
                               <&> over definitionDiffRendered_ toJSON
                               & fmap (diffDataJSON name)
                               & fmap (\(tag, dJSON) -> Aeson.object ["tag" .= tag, "contents" .= dJSON])
                           )
                  )
                & toJSON @[Aeson.Value]
            childrenJSON =
              children
                & Map.toList
                & fmap
                  ( \(path, childNode) ->
                      Aeson.object ["path" .= path, "contents" .= namespaceTreeDiffJSON childNode]
                  )
         in Aeson.object
              [ "changes" .= changesJSON,
                "children" .= childrenJSON
              ]

      libdepsDiffJSON :: Map NameSegment (DiffOp BranchHash) -> Aeson.Value
      libdepsDiffJSON =
        Map.toList
          >>> map
            ( \(name, op) ->
                case op of
                  Merge.DiffOp'Add hash ->
                    Aeson.object
                      [ "hash" .= hash,
                        "name" .= name,
                        "tag" .= ("Added" :: Text)
                      ]
                  Merge.DiffOp'Delete hash ->
                    Aeson.object
                      [ "hash" .= hash,
                        "name" .= name,
                        "tag" .= ("Removed" :: Text)
                      ]
                  Merge.DiffOp'Update Merge.Updated {old, new} ->
                    Aeson.object
                      [ "name" .= name,
                        "newHash" .= new,
                        "oldHash" .= old,
                        "tag" .= ("Updated" :: Text)
                      ]
            )
          >>> toJSON @[Aeson.Value]

type NamespaceAndLibdepsDiff referent reference renderedTerm renderedType termDiff typeDiff libdep =
  GNamespaceAndLibdepsDiff Path referent reference renderedTerm renderedType termDiff typeDiff libdep

data GNamespaceAndLibdepsDiff k referent reference renderedTerm renderedType termDiff typeDiff libdep
  = NamespaceAndLibdepsDiff
  { defns :: GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff,
    libdeps :: Map NameSegment (DiffOp libdep)
  }
  deriving stock (Show)

namespaceAndLibdepsDiffDefns_ ::
  Traversal
    (GNamespaceAndLibdepsDiff k referent reference renderedTerm renderedType termDiff typeDiff libdep)
    (GNamespaceAndLibdepsDiff k' referent' reference' renderedTerm' renderedType' termDiff' typeDiff' libdep)
    (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff)
    (GNamespaceTreeDiff k' referent' reference' renderedTerm' renderedType' termDiff' typeDiff')
namespaceAndLibdepsDiffDefns_ f (NamespaceAndLibdepsDiff defns libdeps) =
  NamespaceAndLibdepsDiff <$> f defns <*> pure libdeps

namespaceAndLibdepsDiffLibdeps_ ::
  Traversal
    (GNamespaceAndLibdepsDiff k referent reference renderedTerm renderedType termDiff typeDiff libdep)
    (GNamespaceAndLibdepsDiff k referent reference renderedTerm renderedType termDiff typeDiff libdep')
    (Map NameSegment (DiffOp libdep))
    (Map NameSegment (DiffOp libdep'))
namespaceAndLibdepsDiffLibdeps_ f (NamespaceAndLibdepsDiff defns libdeps) =
  NamespaceAndLibdepsDiff defns <$> f libdeps

-- | A compressed tree of differences between two namespaces.
-- All intermediate namespaces with no differences are compressed into the keys of the
-- first child that has differences.
--
-- E.g.
--
-- If there's a change at `a.b.c` and `a.x.y`, the tree will look like:
--
-- @@
-- a
-- ├── b.c = DiffAtPath
-- └── x.y = DiffAtPath
-- @@
--
-- If there's a change at a.b.c and a.b.x, the tree will look like:
-- @@
-- a
-- └── b
--    ├── c = DiffAtPath
--    └── x = DiffAtPath
-- @@
type NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff =
  GNamespaceTreeDiff Path referent reference renderedTerm renderedType termDiff typeDiff

type GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff =
  GNamespaceTreeOf k (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff)

type GNamespaceTreeOf k a =
  Cofree (Map k) (Map NameSegment a)

-- | The differences at a specific path in the namespace tree.
data DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff = DiffAtPath
  { termDiffsAtPath :: Set (DefinitionDiff referent renderedTerm termDiff),
    typeDiffsAtPath :: Set (DefinitionDiff reference renderedType typeDiff)
  }
  deriving stock (Eq, Show)

-- | A traversal over all the referents in a `DiffAtPath`.
diffAtPathReferents_ :: (Ord referent', Ord termDiff, Ord renderedTerm) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent' reference renderedTerm renderedType termDiff typeDiff) referent referent'
diffAtPathReferents_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffRefs_) %%~ f
    & fmap \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the references in a `DiffAtPath`.
diffAtPathReferences_ :: (Ord reference', Ord typeDiff, Ord renderedType) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference' renderedTerm renderedType termDiff typeDiff) reference reference'
diffAtPathReferences_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffRefs_) %%~ f
    & fmap \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the term diffs in a `DiffAtPath`.
diffAtPathTermDiffs_ :: (Ord termDiff', Ord referent, Ord renderedTerm) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType termDiff' typeDiff) termDiff termDiff'
diffAtPathTermDiffs_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffDiffs_) %%~ f
    <&> \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

witherDiffAtPathTermDiffs ::
  forall f reference referent renderedTerm renderedType termDiff termDiff' typeDiff.
  (Applicative f, Ord termDiff', Ord referent, Ord renderedTerm) =>
  (termDiff -> f (Maybe termDiff')) ->
  DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff ->
  f (Maybe (DiffAtPath referent reference renderedTerm renderedType termDiff' typeDiff))
witherDiffAtPathTermDiffs f DiffAtPath {termDiffsAtPath, typeDiffsAtPath} =
  g <$> Set.forMaybe termDiffsAtPath (getCompose . (definitionDiffDiffs_ (Compose . f)))
  where
    g ::
      Set (DefinitionDiff referent renderedTerm termDiff') ->
      Maybe (DiffAtPath referent reference renderedTerm renderedType termDiff' typeDiff)
    g termDiffsAtPath =
      if Set.null termDiffsAtPath && Set.null typeDiffsAtPath
        then Nothing
        else Just DiffAtPath {termDiffsAtPath, typeDiffsAtPath}

-- | A traversal over all the type diffs in a `DiffAtPath`.
diffAtPathTypeDiffs_ :: (Ord typeDiff', Ord reference, Ord renderedType) => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff') typeDiff typeDiff'
diffAtPathTypeDiffs_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffDiffs_) %%~ f
    <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

diffAtPathTermDiffKinds_ :: (Ord renderedTerm', Ord termDiff', Ord referent') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent' reference renderedTerm' renderedType termDiff' typeDiff) (DefinitionDiffKind referent renderedTerm termDiff) (DefinitionDiffKind referent' renderedTerm' termDiff')
diffAtPathTermDiffKinds_ f (DiffAtPath terms types) = do
  newTerms <- terms & Set.traverse . definitionDiffKind_ %%~ f
  pure $ DiffAtPath newTerms types

witherDiffAtPathTermDiffKinds ::
  forall f reference referent referent' renderedTerm renderedTerm' renderedType termDiff termDiff' typeDiff.
  (Applicative f, Ord renderedTerm', Ord termDiff', Ord referent') =>
  (DefinitionDiffKind referent renderedTerm termDiff -> f (Maybe (DefinitionDiffKind referent' renderedTerm' termDiff'))) ->
  DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff ->
  f (Maybe (DiffAtPath referent' reference renderedTerm' renderedType termDiff' typeDiff))
witherDiffAtPathTermDiffKinds f DiffAtPath {termDiffsAtPath, typeDiffsAtPath} =
  g <$> Set.forMaybe termDiffsAtPath (getCompose . definitionDiffKind_ (Compose . f))
  where
    g ::
      Set (DefinitionDiff referent' renderedTerm' termDiff') ->
      Maybe (DiffAtPath referent' reference renderedTerm' renderedType termDiff' typeDiff)
    g termDiffsAtPath =
      if Set.null termDiffsAtPath && Set.null typeDiffsAtPath
        then Nothing
        else Just DiffAtPath {termDiffsAtPath, typeDiffsAtPath}

diffAtPathTypeDiffKinds_ :: (Ord renderedType', Ord typeDiff', Ord reference') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference' renderedTerm renderedType' termDiff typeDiff') (DefinitionDiffKind reference renderedType typeDiff) (DefinitionDiffKind reference' renderedType' typeDiff')
diffAtPathTypeDiffKinds_ f (DiffAtPath terms types) = do
  newTypes <- types & Set.traverse . definitionDiffKind_ %%~ f
  pure $ DiffAtPath terms newTypes

-- | A traversal over all the rendered terms in a `DiffAtPath`.
diffAtPathRenderedTerms_ :: (Ord termDiff, Ord referent, Ord renderedTerm') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm' renderedType termDiff typeDiff) renderedTerm renderedTerm'
diffAtPathRenderedTerms_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  termDiffsAtPath
    & (Set.traverse . definitionDiffRendered_) %%~ f
    <&> \termDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | A traversal over all the rendered types in a `DiffAtPath`.
diffAtPathRenderedTypes_ :: (Ord typeDiff, Ord reference, Ord renderedType') => Traversal (DiffAtPath referent reference renderedTerm renderedType termDiff typeDiff) (DiffAtPath referent reference renderedTerm renderedType' termDiff typeDiff) renderedType renderedType'
diffAtPathRenderedTypes_ f (DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) =
  typeDiffsAtPath
    & (Set.traverse . definitionDiffRendered_) %%~ f
    <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | Traversal over all the referents in a `NamespaceTreeDiff`.
namespaceTreeDiffReferents_ :: (Ord referent', Ord termDiff, Ord renderedTerm) => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent' reference renderedTerm renderedType termDiff typeDiff) referent referent'
namespaceTreeDiffReferents_ =
  traversed . traversed . diffAtPathReferents_

-- | Traversal over all the references in a `NamespaceTreeDiff`.
namespaceTreeDiffReferences_ :: (Ord reference', Ord typeDiff, Ord renderedType) => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference' renderedTerm renderedType termDiff typeDiff) reference reference'
namespaceTreeDiffReferences_ = traversed . traversed . diffAtPathReferences_

namespaceTreeDiffTermDiffs_ :: (Ord termDiff', Ord referent, Ord renderedTerm) => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff' typeDiff) termDiff termDiff'
namespaceTreeDiffTermDiffs_ = traversed . traversed . diffAtPathTermDiffs_

witherNamespaceTreeDiffTermDiffs ::
  forall f k reference referent renderedTerm renderedType termDiff termDiff' typeDiff.
  (Monad f, Ord termDiff', Ord referent, Ord renderedTerm) =>
  (termDiff -> f (Maybe termDiff')) ->
  GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
  f (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff)
witherNamespaceTreeDiffTermDiffs f =
  fmap (fromMaybe (Map.empty Cofree.:< Map.empty)) . go
  where
    go ::
      GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
      f (Maybe (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff))
    go (x Cofree.:< xs) =
      g <$> wither (witherDiffAtPathTermDiffs f) x <*> wither go xs

    g ::
      Map NameSegment (DiffAtPath referent reference renderedTerm renderedType termDiff' typeDiff) ->
      Map k (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff) ->
      Maybe (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff)
    g x xs
      | Map.null x && Map.null xs = Nothing
      | otherwise = Just (x Cofree.:< xs)

namespaceTreeDiffTypeDiffs_ :: (Ord typeDiff', Ord reference, Ord renderedType) => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff') typeDiff typeDiff'
namespaceTreeDiffTypeDiffs_ = traversed . traversed . diffAtPathTypeDiffs_

namespaceTreeTermDiffKinds_ :: (Ord renderedTerm', Ord termDiff', Ord referent') => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff) (DefinitionDiffKind referent renderedTerm termDiff) (DefinitionDiffKind referent' renderedTerm' termDiff')
namespaceTreeTermDiffKinds_ = traversed . traversed . diffAtPathTermDiffKinds_

witherNamespaceTreeTermDiffKinds ::
  forall f k reference referent referent' renderedTerm renderedTerm' renderedType termDiff termDiff' typeDiff.
  (Monad f, Ord renderedTerm', Ord termDiff', Ord referent') =>
  (DefinitionDiffKind referent renderedTerm termDiff -> f (Maybe (DefinitionDiffKind referent' renderedTerm' termDiff'))) ->
  GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
  f (GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff)
witherNamespaceTreeTermDiffKinds f =
  fmap (fromMaybe (Map.empty Cofree.:< Map.empty)) . go
  where
    go ::
      GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
      f (Maybe (GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff))
    go (x Cofree.:< xs) =
      g <$> wither (witherDiffAtPathTermDiffKinds f) x <*> wither go xs

    g ::
      Map NameSegment (DiffAtPath referent' reference renderedTerm' renderedType termDiff' typeDiff) ->
      Map k (GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff) ->
      Maybe (GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff)
    g x xs
      | Map.null x && Map.null xs = Nothing
      | otherwise = Just (x Cofree.:< xs)

namespaceTreeTypeDiffKinds_ :: (Ord renderedType', Ord typeDiff', Ord reference') => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference' renderedTerm renderedType' termDiff typeDiff') (DefinitionDiffKind reference renderedType typeDiff) (DefinitionDiffKind reference' renderedType' typeDiff')
namespaceTreeTypeDiffKinds_ = traversed . traversed . diffAtPathTypeDiffKinds_

namespaceTreeDiffRenderedTerms_ :: (Ord termDiff, Ord referent, Ord renderedTerm') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm' renderedType termDiff typeDiff) renderedTerm renderedTerm'
namespaceTreeDiffRenderedTerms_ = traversed . traversed . diffAtPathRenderedTerms_

namespaceTreeDiffRenderedTypes_ :: (Ord typeDiff, Ord reference, Ord renderedType') => Traversal (NamespaceTreeDiff referent reference renderedTerm renderedType termDiff typeDiff) (NamespaceTreeDiff referent reference renderedTerm renderedType' termDiff typeDiff) renderedType renderedType'
namespaceTreeDiffRenderedTypes_ = traversed . traversed . diffAtPathRenderedTypes_

-- | Convert a `DefinitionDiffs` into a tree of differences.
definitionDiffsToTree ::
  forall ref.
  (Ord ref) =>
  DefinitionDiffs Name ref ->
  GNamespaceTreeOf NameSegment (Set (DefinitionDiff ref Name Name))
definitionDiffsToTree dd =
  let DefinitionDiffs {added, removed, updated, propagated, renamed, newAliases} = dd
      expandedAliases :: Map Name (Set (DefinitionDiffKind ref Name Name))
      expandedAliases =
        newAliases
          & Map.toList
          & foldMap
            ( \(r, (existingNames, newNames)) ->
                ( Foldable.toList newNames
                    <&> \newName -> Map.singleton newName (Set.singleton (NewAlias r existingNames newName))
                )
            )
          & Map.unionsWith (<>)
      expandedRenames :: Map Name (Set (DefinitionDiffKind ref Name Name))
      expandedRenames =
        renamed
          & Map.toList
          & foldMap \(r, (oldNames, newNames)) ->
            ( -- We don't currently want to track the old names in a rename, and including them messes up
              -- the path-compression for the diff tree, so we just omit them.
              -- ( Foldable.toList oldNames
              --       <&> \oldName -> Map.singleton oldName (Set.singleton (RenamedTo r newNames))
              --   )
              -- <>
              ( Foldable.toList newNames
                  <&> \newName -> Map.singleton newName (Set.singleton (RenamedFrom r oldNames newName))
              )
            )
              & Map.unionsWith (<>)
      diffTree :: Map Name (Set (DefinitionDiffKind ref Name Name))
      diffTree =
        Map.unionsWith
          (<>)
          [ (added & Map.mapWithKey \n r -> Set.singleton $ Added r n),
            expandedAliases,
            (removed & Map.mapWithKey \n r -> Set.singleton $ Removed r n),
            (updated & Map.mapWithKey \name (oldR, newR) -> Set.singleton $ Updated oldR newR name),
            (propagated & Map.mapWithKey \name (oldR, newR) -> Set.singleton $ Propagated oldR newR name),
            expandedRenames
          ]
      includeFQNs :: Map Name (Set (DefinitionDiffKind ref Name Name)) -> Map Name (Set (DefinitionDiff ref Name Name))
      includeFQNs m = m & imap \n ds -> (ds & Set.map \d -> DefinitionDiff {kind = d, fqn = n})
   in diffTree
        & includeFQNs
        & expandNameTree

-- Unfolds a Map of names into a Cofree of paths by name segemnt.
--
-- >>> import qualified Unison.Syntax.Name as NS
-- >>> expandNameTree $ Map.fromList [(NS.unsafeParseText "a.b", "a.b"), (NS.unsafeParseText "a.c", "a.c"), (NS.unsafeParseText "x.y.z", "x.y.z")]
-- fromList [] :< fromList [(NameSegment {toUnescapedText = "a"},fromList [(NameSegment {toUnescapedText = "b"},"a.b"),(NameSegment {toUnescapedText = "c"},"a.c")] :< fromList []),(NameSegment {toUnescapedText = "x"},fromList [] :< fromList [(NameSegment {toUnescapedText = "y"},fromList [(NameSegment {toUnescapedText = "z"},"x.y.z")] :< fromList [])])]
expandNameTree :: forall a. Map Name a -> Cofree (Map NameSegment) (Map NameSegment a)
expandNameTree m =
  let (here, children) =
        m
          & Map.toList
          & fmap splitNames
          & partitionEithers
      childMap =
        children
          & Map.fromListWith Map.union
          & fmap expandNameTree
   in Map.fromList here Cofree.:< childMap
  where
    splitNames :: (Name, a) -> Either (NameSegment, a) (NameSegment, Map Name a)
    splitNames (n, a) =
      case Name.segments n of
        (ns :| []) -> Left (ns, a)
        (ns :| (r : rs)) -> Right (ns, Map.singleton (Name.fromSegments (r :| rs)) a)

combineTermsAndTypes ::
  forall f reference referent.
  (Semialign f, Ord reference, Ord referent) =>
  These
    (f (Set (DefinitionDiff referent Name Name)))
    (f (Set (DefinitionDiff reference Name Name))) ->
  f (DiffAtPath referent reference Name Name Name Name)
combineTermsAndTypes = \case
  This termsMap -> termsMap <&> \termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
  That typesMap -> typesMap <&> \typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
  These trms typs -> alignWith combineNode trms typs
  where
    combineNode ::
      These
        (Set (DefinitionDiff referent Name Name))
        (Set (DefinitionDiff reference Name Name)) ->
      DiffAtPath referent reference Name Name Name Name
    combineNode = \case
      This termDiffsAtPath -> DiffAtPath {termDiffsAtPath, typeDiffsAtPath = mempty}
      That typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath = mempty}
      These termDiffsAtPath typeDiffsAtPath -> DiffAtPath {typeDiffsAtPath, termDiffsAtPath}

-- | Collapse all links which have only a single child into a path.
-- I.e. the resulting tree will not contain nodes that have only a single namespace child with no diffs.
--
-- Note that the final node will always have a map of name segments containing the final
-- segment of the name, since that's considered the name and not part of the path.
--
-- >>> import qualified Unison.Syntax.Name as NS
-- >>> let expanded = expandNameTree $ Map.fromList [(NS.unsafeParseText "a.b", "a.b"), (NS.unsafeParseText "a.c", "a.c"), (NS.unsafeParseText "x.y.z", "x.y.z")]
-- >>> compressNameTree expanded
-- fromList [] :< fromList [(a,fromList [(b,"a.b"),(c,"a.c")] :< fromList []),(x.y,fromList [(z,"x.y.z")] :< fromList [])]
compressNameTree :: Cofree (Map NameSegment) (Map NameSegment a) -> Cofree (Map Path) (Map NameSegment a)
compressNameTree (diffs Cofree.:< children) =
  let compressedChildren =
        children
          & fmap compressNameTree
          & Map.toList
          & fmap
            ( \(ns, child) ->
                case child of
                  (childDiffs Cofree.:< nestedChildren)
                    | null childDiffs,
                      [(k, v)] <- Map.toList nestedChildren ->
                        (Path.prefix (Path.singleton ns) (Path.Relative k), v)
                    | otherwise ->
                        (Path.singleton ns, child)
            )
          & Map.fromList
   in diffs Cofree.:< compressedChildren

computeThreeWayNamespaceDiff ::
  TwoWay Codebase.CodebaseEnv ->
  TwoOrThreeWay BranchHashId ->
  TwoOrThreeWay NameLookupReceipt ->
  PG.Transaction NamespaceDiffError (GNamespaceAndLibdepsDiff NameSegment Referent Reference Name Name Name Name BranchHashId)
computeThreeWayNamespaceDiff codebaseEnvs2 branchHashIds3 nameLookupReceipts3 = do
  -- Load a flat definitions names (no lib) for Alice/Bob/LCA
  defnsNames3 :: TwoOrThreeWay Names <-
    sequence (NL.projectNamesWithoutLib <$> nameLookupReceipts3 <*> branchHashIds3)

  -- Unflatten each Names to a Nametree (leniently). Really, only the LCA is "allowed" to break the diff/merge rules of
  -- no conflicted names, but we don't enforce that here. If Alice or Bob have a conflicted name for some reason, we'll
  -- just silently pick one of the refs and move on.
  let defnsNametrees3 :: TwoOrThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
      defnsNametrees3 =
        Names.lenientToNametree <$> defnsNames3

  -- Load the shallow libdeps for Alice/Bob/LCA. This can fail with "lib at unexpected path"
  libdeps3 :: TwoOrThreeWay (Map NameSegment BranchHashId) <- do
    let f :: NameLookupReceipt -> BranchHashId -> PG.Transaction NamespaceDiffError (Map NameSegment BranchHashId)
        f nameLookupReceipt branchHashId = do
          mounts <- NL.listNameLookupMounts nameLookupReceipt branchHashId
          libDepsList <-
            for mounts \(NL.PathSegments path, libBhId) -> do
              case NameSegment.unsafeParseText <$> path of
                [NameSegment.LibSegment, dep] -> pure (dep, libBhId)
                p -> throwError $ LibFoundAtUnexpectedPath (Path.fromList p)
          pure $ Map.fromList libDepsList
    sequence (f <$> nameLookupReceipts3 <*> branchHashIds3)

  -- Make that 0th mergeblob
  let blob0 :: Mergeblob0 BranchHashId
      blob0 =
        Merge.makeMergeblob0
          ThreeWay
            { alice = defnsNametrees3.alice,
              bob = defnsNametrees3.bob,
              lca = fromMaybe Nametree {value = Defns Map.empty Map.empty, children = Map.empty} defnsNametrees3.lca
            }
          ThreeWay
            { alice = libdeps3.alice,
              bob = libdeps3.bob,
              lca = fromMaybe Map.empty libdeps3.lca
            }

  -- Hydrate defns in Alice/Bob/LCA
  hydratedDefns3 ::
    ThreeWay
      ( DefnsF
          (Map Name)
          (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
          (TypeReferenceId, Decl Symbol Ann)
      ) <- do
    let hydrateTerms ::
          UserId ->
          BiMultimap Referent Name ->
          PG.Transaction e (Map Name (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)))
        hydrateTerms codebaseUser termReferents = do
          let termReferenceIds = Map.mapMaybe Referent.toTermReferenceId (BiMultimap.range termReferents)
          termIds <-
            PG.pFor termReferenceIds \refId ->
              (refId,) <$> DefnsQ.pipelinedExpectTermId refId
          v2Terms <-
            PG.pFor termIds \(refId, termId) ->
              (refId,) <$> DefnsQ.pipelinedExpectTermById codebaseUser refId termId
          v1Terms <-
            for v2Terms \(refId, (term, typ)) ->
              (refId,) <$> Codebase.convertTerm2to1 (Reference.idToHash refId) term typ
          pure v1Terms
        hydrateTypes ::
          UserId ->
          BiMultimap TypeReference Name ->
          PG.Transaction e (Map Name (TypeReferenceId, Decl Symbol Ann))
        hydrateTypes codebaseUser typeReferences = do
          let typeReferenceIds = Map.mapMaybe Reference.toId (BiMultimap.range typeReferences)
          typeIds <-
            PG.pFor typeReferenceIds \refId ->
              (refId,) <$> DefnsQ.pipelinedExpectTypeComponentElementAndTypeId codebaseUser refId
          v1Decls <-
            PG.pFor typeIds \(refId, typeId) ->
              DefnsQ.loadDeclByTypeComponentElementAndTypeId typeId <&> \v2Decl ->
                let v1Decl = Cv.decl2to1 (Reference.idToHash refId) v2Decl
                 in (refId, v1Decl)
          pure v1Decls
        f ::
          Codebase.CodebaseEnv ->
          Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
          PG.Transaction
            e
            ( DefnsF
                (Map Name)
                (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
                (TypeReferenceId, Decl Symbol Ann)
            )
        f codebaseEnv =
          let codebaseUser = Codebase.codebaseOwner codebaseEnv
           in bitraverse (hydrateTerms codebaseUser) (hydrateTypes codebaseUser)

    let -- Here we assume that the LCA is in the same codebase as Alice.
        codebaseEnvs3 :: ThreeWay Codebase.CodebaseEnv
        codebaseEnvs3 =
          ThreeWay
            { alice = codebaseEnvs2.alice,
              bob = codebaseEnvs2.bob,
              lca = codebaseEnvs2.alice
            }
    sequence (f <$> codebaseEnvs3 <*> blob0.defns)

  -- Get a names object that contains just enough names to compute the diff:
  names3 :: TwoOrThreeWay Names <- do
    -- Massage the hydrated definitions into a set of "labeled dependency" that contains the definitions themselves
    -- and their direct references.
    --
    -- FIXME: Mitchell wonders why self is necessary. Aren't direct dependency names enough?
    let labeledDeps3 :: ThreeWay (Set LabeledDependency)
        labeledDeps3 =
          Mergeblob1.hydratedDefnsLabeledDependencies <$> hydratedDefns3
    -- Get a names perspective for Alice/Bob/LCA
    namesPerspectives3 :: TwoOrThreeWay NL.NamesPerspective <-
      for branchHashIds3 \branchHashId ->
        NL.namesPerspectiveForRootAndPath branchHashId (mempty @NL.PathSegments)
    sequence (PGNames.namesForReferences <$> namesPerspectives3 <*> ThreeWay.toTwoOrThreeWay labeledDeps3)

  blob1 :: Mergeblob1 BranchHashId <- do
    let names3' = ThreeWay {alice = names3.alice, bob = names3.bob, lca = fromMaybe (mempty @Names) names3.lca}
    case Merge.makeMergeblob1 blob0 names3' hydratedDefns3 of
      Right blob -> pure blob
      Left err -> throwError (IncoherentDecl err)

  -- Boilerplate conversion: make a "DefinitionDiffs" from the info in a "Mergeblob1".
  --
  -- (Mitchell says: I think Share and UCM should share a type here – perhaps DefinitionDiffs should be pushed down? Or
  -- is it just isomorphic to something that already exists in UCM?)
  --
  -- We start focusing only on Bob here, the contributor, even though Alice could have a diff as well of course (since
  -- the LCA is arbitrarily behind Alice).

  let definitionDiffs :: DefnsF (DefinitionDiffs Name) Referent TypeReference
      definitionDiffs =
        let f :: forall ref. (Ord ref) => Map Name (HumanDiffOp ref) -> DefinitionDiffs Name ref
            f =
              Map.toList >>> foldMap \(name, op) ->
                case op of
                  HumanDiffOp'Add ref -> mempty {added = Map.singleton name ref}
                  HumanDiffOp'Delete ref -> mempty {removed = Map.singleton name ref}
                  HumanDiffOp'Update refs -> mempty {updated = Map.singleton name (refs.old, refs.new)}
                  HumanDiffOp'PropagatedUpdate refs -> mempty {propagated = Map.singleton name (refs.old, refs.new)}
                  HumanDiffOp'AliasOf ref names ->
                    mempty {newAliases = Map.singleton ref (names, NESet.singleton name)}
                  HumanDiffOp'RenamedFrom ref names ->
                    mempty {renamed = Map.singleton ref (names, NESet.singleton name)}
                  HumanDiffOp'RenamedTo ref names ->
                    mempty {renamed = Map.singleton ref (NESet.singleton name, names)}
         in bimap f f blob1.humanDiffsFromLCA.bob

  -- Convert definition diffs to two uncompressed trees of diffs (one for terms, one for types)
  let twoUncompressedTrees ::
        DefnsF3
          (Cofree (Map NameSegment))
          (Map NameSegment)
          Set
          (DefinitionDiff Referent Name Name)
          (DefinitionDiff TypeReference Name Name)
      twoUncompressedTrees =
        bimap definitionDiffsToTree definitionDiffsToTree definitionDiffs

  -- Align terms and types trees into one tree (still uncompressed)
  let oneUncompressedTree :: GNamespaceTreeDiff NameSegment Referent TypeReference Name Name Name Name
      oneUncompressedTree =
        alignDefnsWith combineTermsAndTypes twoUncompressedTrees

  pure
    NamespaceAndLibdepsDiff
      { defns = oneUncompressedTree,
        libdeps = blob1.libdepsDiffs.bob
      }
