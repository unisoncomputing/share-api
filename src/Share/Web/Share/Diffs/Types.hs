{-# LANGUAGE DataKinds #-}

module Share.Web.Share.Diffs.Types where

import Control.Comonad.Cofree qualified as Cofree
import Data.Aeson
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Share.IDs
import Share.NamespaceDiffs (DefinitionDiff (..), DefinitionDiffKind (..), DiffAtPath (..), NamespaceTreeDiff)
import Share.Postgres.IDs (CausalHash)
import Share.Prelude
import Share.Utils.Aeson (MaybeEncoded)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Server.Types (DisplayObjectDiff (..), TermDefinition, TermDefinitionDiff (..), TermTag, TypeDefinition, TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)

type ShareNamespaceDiff = NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinitionDiff TypeDefinitionDiff

data ShareNamespaceDiffResponse = ShareNamespaceDiffResponse
  { project :: ProjectShortHand,
    oldRef :: BranchOrReleaseShortHand,
    oldRefHash :: Maybe (PrefixedHash "#" CausalHash),
    newRef :: BranchOrReleaseShortHand,
    newRefHash :: Maybe (PrefixedHash "#" CausalHash),
    diff :: MaybeEncoded ShareNamespaceDiff
  }

instance ToJSON ShareNamespaceDiffResponse where
  toJSON (ShareNamespaceDiffResponse {diff, project, oldRef, newRef, oldRefHash, newRefHash}) =
    object
      [ "diff" .= (namespaceTreeDiffJSON <$> diff),
        "project" .= project,
        "oldRef" .= oldRef,
        "oldRefHash" .= oldRefHash,
        "newRef" .= newRef,
        "newRefHash" .= newRefHash
      ]
    where
      text :: Text -> Text
      text t = t
      hqNameJSON :: Name -> NameSegment -> ShortHash -> Value
      hqNameJSON fqn name sh = object ["hash" .= sh, "shortName" .= name, "fullName" .= fqn]
      -- The preferred frontend format is a bit clunky to calculate here:
      diffDataJSON :: (ToJSON tag) => NameSegment -> DefinitionDiff (tag, ShortHash) Value -> (tag, Value)
      diffDataJSON shortName (DefinitionDiff {fqn, kind}) = case kind of
        Added (defnTag, r) -> (defnTag, object ["tag" .= text "Added", "contents" .= hqNameJSON fqn shortName r])
        NewAlias (defnTag, r) existingNames ->
          let contents = object ["hash" .= r, "aliasShortName" .= shortName, "aliasFullName" .= fqn, "otherNames" .= toList existingNames]
           in (defnTag, object ["tag" .= text "Aliased", "contents" .= contents])
        Removed (defnTag, r) -> (defnTag, object ["tag" .= text "Removed", "contents" .= hqNameJSON fqn shortName r])
        Updated (oldTag, oldRef) (newTag, newRef) diffVal ->
          let contents = object ["oldHash" .= oldRef, "newHash" .= newRef, "shortName" .= shortName, "fullName" .= fqn, "oldTag" .= oldTag, "newTag" .= newTag, "diff" .= diffVal]
           in (newTag, object ["tag" .= text "Updated", "contents" .= contents])
        RenamedTo (defnTag, r) newNames ->
          let contents = object ["oldShortName" .= shortName, "oldFullName" .= fqn, "newNames" .= newNames, "hash" .= r]
           in (defnTag, object ["tag" .= text "RenamedTo", "contents" .= contents])
        RenamedFrom (defnTag, r) oldNames ->
          let contents = object ["oldNames" .= oldNames, "newShortName" .= shortName, "newFullName" .= fqn, "hash" .= r]
           in (defnTag, object ["tag" .= text "RenamedFrom", "contents" .= contents])

      displayObjectDiffToJSON :: DisplayObjectDiff -> Value
      displayObjectDiffToJSON = \case
        DisplayObjectDiff dispDiff ->
          object ["diff" .= dispDiff, "diffKind" .= ("diff" :: Text)]
        MismatchedDisplayObjects {} ->
          object ["diffKind" .= ("mismatched" :: Text)]

      termDefinitionDiffToJSON :: TermDefinitionDiff -> Value
      termDefinitionDiffToJSON (TermDefinitionDiff {left, right, diff}) = object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]

      typeDefinitionDiffToJSON :: TypeDefinitionDiff -> Value
      typeDefinitionDiffToJSON (TypeDefinitionDiff {left, right, diff}) = object ["left" .= left, "right" .= right, "diff" .= displayObjectDiffToJSON diff]
      namespaceTreeDiffJSON :: NamespaceTreeDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinitionDiff TypeDefinitionDiff -> Value
      namespaceTreeDiffJSON (diffs Cofree.:< children) =
        let changesJSON =
              diffs
                & Map.toList
                & foldMap
                  ( \(name, DiffAtPath {termDiffsAtPath, typeDiffsAtPath}) ->
                      ( Foldable.toList termDiffsAtPath
                          <&> fmap termDefinitionDiffToJSON
                          & fmap (diffDataJSON name)
                          & fmap (\(tag, dJSON) -> object ["tag" .= tag, "contents" .= dJSON])
                      )
                        <> ( Foldable.toList typeDiffsAtPath
                               <&> fmap typeDefinitionDiffToJSON
                               & fmap (diffDataJSON name)
                               & fmap (\(tag, dJSON) -> object ["tag" .= tag, "contents" .= dJSON])
                           )
                  )
                & toJSON @([Value])
            childrenJSON =
              children
                & Map.toList
                & fmap
                  ( \(path, childNode) ->
                      object ["path" .= path, "contents" .= namespaceTreeDiffJSON childNode]
                  )
         in object
              [ "changes" .= changesJSON,
                "children" .= childrenJSON
              ]

data ShareTermDiffResponse = ShareTermDiffResponse
  { project :: ProjectShortHand,
    oldBranch :: BranchOrReleaseShortHand,
    newBranch :: BranchOrReleaseShortHand,
    oldTerm :: TermDefinition,
    newTerm :: TermDefinition,
    diff :: DisplayObjectDiff
  }

instance ToJSON ShareTermDiffResponse where
  toJSON (ShareTermDiffResponse {diff, project, oldBranch, newBranch, oldTerm, newTerm}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]

data ShareTypeDiffResponse = ShareTypeDiffResponse
  { project :: ProjectShortHand,
    oldBranch :: BranchOrReleaseShortHand,
    newBranch :: BranchOrReleaseShortHand,
    oldType :: TypeDefinition,
    newType :: TypeDefinition,
    diff :: DisplayObjectDiff
  }

instance ToJSON ShareTypeDiffResponse where
  toJSON (ShareTypeDiffResponse {diff, project, oldBranch, newBranch, oldType, newType}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]
