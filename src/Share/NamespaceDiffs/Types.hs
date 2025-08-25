{-# LANGUAGE DeriveAnyClass #-}

module Share.NamespaceDiffs.Types
  ( NamespaceDiffError (..),
    DefinitionDiffs (..),
    DefinitionDiff (..),
    DefinitionDiffKind (..),
    NamespaceDiffResult (..),
    NamespaceAndLibdepsDiff,
    GNamespaceAndLibdepsDiff (..),
    GNamespaceTreeDiff,
    GNamespaceTreeOf,
    NamespaceTreeDiff,
    DiffAtPath (..),
    namespaceAndLibdepsDiffDefns_,
    namespaceAndLibdepsDiffLibdeps_,
    namespaceTreeDiffReferents_,
    namespaceTreeDiffReferences_,
    namespaceTreeDiffTermDiffs_,
    mapMaybeNamespaceTreeDiffTermDiffs,
    witherNamespaceTreeDiffTermDiffs,
    namespaceTreeDiffTypeDiffs_,
    namespaceTreeTermDiffKinds_,
    mapMaybeNamespaceTreeTermDiffKinds,
    witherNamespaceTreeTermDiffKinds,
    namespaceTreeTypeDiffKinds_,
    namespaceTreeDiffRenderedTerms_,
    namespaceTreeDiffRenderedTypes_,
    definitionDiffKindRendered_,
    definitionDiffKindRenderedOldNew_,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding ((.=), (:<))
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (ToJSON (..))
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Servant (err400, err404, err500)
import Share.Postgres.IDs (BranchHash)
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors
import Unison.Codebase.Path (Path)
import Unison.DeclCoherencyCheck (IncoherentDeclReason (..))
import Unison.Merge (DiffOp, EitherWay)
import Unison.Merge qualified as Merge
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.Server.Types (DisplayObjectDiff (..), TermDefinition, TermDefinitionDiff (..), TermTag, TypeDefinition, TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)
import Unison.Util.Set qualified as Set

data NamespaceDiffError
  = ImpossibleError Text
  | IncoherentDecl (EitherWay IncoherentDeclReason)
  | LibFoundAtUnexpectedPath Path
  | MissingEntityError EntityMissing
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

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

definitionDiffKindRenderedOldNew_ :: Traversal (DefinitionDiffKind r rendered diff) (DefinitionDiffKind r rendered' diff) (Either rendered rendered) rendered'
definitionDiffKindRenderedOldNew_ f = \case
  Added r rendered -> Added r <$> f (Right rendered)
  NewAlias r ns rendered -> NewAlias r ns <$> f (Right rendered)
  Removed r rendered -> Removed r <$> f (Left rendered)
  Propagated old new diff -> Propagated old new <$> pure diff
  Updated old new diff -> Updated old new <$> pure diff
  RenamedTo r old rendered -> RenamedTo r old <$> f (Left rendered)
  RenamedFrom r old rendered -> RenamedFrom r old <$> f (Right rendered)

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
                [ "tag" .= ("impossibleError" :: Text)
                ]
            IncoherentDecl reason ->
              let f :: Text -> IncoherentDeclReason -> Aeson.Value
                  f which reason =
                    Aeson.object
                      ( "oldOrNewBranch"
                          .= which
                          : case reason of
                            IncoherentDeclReason'ConstructorAlias typeName constructorName1 constructorName2 ->
                              [ "tag" .= ("constructorAlias" :: Text),
                                "typeName" .= typeName,
                                "constructorName1" .= constructorName1,
                                "constructorName2" .= constructorName2
                              ]
                            IncoherentDeclReason'MissingConstructorName typeName ->
                              [ "tag" .= ("missingConstructorName" :: Text),
                                "typeName" .= typeName
                              ]
                            IncoherentDeclReason'NestedDeclAlias constructorName1 constructorName2 ->
                              [ "tag" .= ("constructorAlias" :: Text),
                                "constructorName1" .= constructorName1,
                                "constructorName2" .= constructorName2
                              ]
                            IncoherentDeclReason'StrayConstructor _ constructorName ->
                              [ "tag" .= ("strayConstructor" :: Text),
                                "constructorName" .= constructorName
                              ]
                      )
               in case reason of
                    EitherWay.Alice reason -> f "old" reason
                    EitherWay.Bob reason -> f "new" reason
            LibFoundAtUnexpectedPath _ ->
              Aeson.object
                [ "tag" .= ("libFoundAtUnexpectedPath" :: Text)
                ]
            MissingEntityError _ ->
              Aeson.object
                [ "tag" .= ("missingEntityError" :: Text)
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

data GNamespaceAndLibdepsDiff k referent reference renderedTerm renderedType termDiff typeDiff libdep = NamespaceAndLibdepsDiff
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

-- | Traversal over all the term diffs in a `NamespaceTreeDiff`.
namespaceTreeDiffTermDiffs_ :: (Ord termDiff', Ord referent, Ord renderedTerm) => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff) termDiff termDiff'
namespaceTreeDiffTermDiffs_ = traversed . traversed . diffAtPathTermDiffs_

mapMaybeNamespaceTreeDiffTermDiffs ::
  forall k reference referent renderedTerm renderedType termDiff termDiff' typeDiff.
  (Ord termDiff', Ord referent, Ord renderedTerm) =>
  (termDiff -> Maybe termDiff') ->
  GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
  GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff' typeDiff
mapMaybeNamespaceTreeDiffTermDiffs f =
  runIdentity . witherNamespaceTreeDiffTermDiffs (Identity . f)

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

mapMaybeNamespaceTreeTermDiffKinds ::
  forall k reference referent referent' renderedTerm renderedTerm' renderedType termDiff termDiff' typeDiff.
  (Ord renderedTerm', Ord termDiff', Ord referent') =>
  (DefinitionDiffKind referent renderedTerm termDiff -> Maybe (DefinitionDiffKind referent' renderedTerm' termDiff')) ->
  GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff ->
  GNamespaceTreeDiff k referent' reference renderedTerm' renderedType termDiff' typeDiff
mapMaybeNamespaceTreeTermDiffKinds f =
  runIdentity . witherNamespaceTreeTermDiffKinds (Identity . f)

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

namespaceTreeDiffRenderedTerms_ :: (Ord termDiff, Ord referent, Ord renderedTerm') => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference renderedTerm' renderedType termDiff typeDiff) renderedTerm renderedTerm'
namespaceTreeDiffRenderedTerms_ = traversed . traversed . diffAtPathRenderedTerms_

namespaceTreeDiffRenderedTypes_ :: (Ord typeDiff, Ord reference, Ord renderedType') => Traversal (GNamespaceTreeDiff k referent reference renderedTerm renderedType termDiff typeDiff) (GNamespaceTreeDiff k referent reference renderedTerm renderedType' termDiff typeDiff) renderedType renderedType'
namespaceTreeDiffRenderedTypes_ = traversed . traversed . diffAtPathRenderedTypes_
