{-# LANGUAGE DataKinds #-}

module Share.Web.Share.Diffs.Types where

import Data.Aeson
import Share.IDs
import Share.NamespaceDiffs (NamespaceAndLibdepsDiff, NamespaceDiffResult)
import Share.Postgres.IDs (BranchHash, CausalHash)
import Share.Prelude
import Share.Utils.Aeson (PreEncoded)
import Unison.Server.Types
  ( DisplayObjectDiff (..),
    TermDefinition (termDefinition),
    TermDefinitionDiff (..),
    TermTag,
    TypeDefinition,
    TypeDefinitionDiff (..),
    TypeTag,
    typeDefinition,
  )
import Unison.ShortHash (ShortHash)

type ShareNamespaceDiff =
  NamespaceAndLibdepsDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff BranchHash

data ShareNamespaceDiffStatus
  = ShareNamespaceDiffStatus'Done (PreEncoded NamespaceDiffResult)
  | ShareNamespaceDiffStatus'StillComputing

data ShareNamespaceDiffResponse = ShareNamespaceDiffResponse
  { project :: ProjectShortHand,
    oldRef :: BranchOrReleaseShortHand,
    oldRefHash :: Maybe (PrefixedHash "#" CausalHash),
    newRef :: BranchOrReleaseShortHand,
    newRefHash :: Maybe (PrefixedHash "#" CausalHash),
    diff :: ShareNamespaceDiffStatus
  }

instance ToJSON ShareNamespaceDiffResponse where
  toJSON (ShareNamespaceDiffResponse {diff, project, oldRef, newRef, oldRefHash, newRefHash}) =
    object $
      diffPairs
        ++ [ "project" .= toJSON project,
             "oldRef" .= oldRef,
             "oldRefHash" .= oldRefHash,
             "newRef" .= newRef,
             "newRefHash" .= newRefHash
           ]
    where
      diffPairs :: [(Key, Value)]
      diffPairs =
        case diff of
          ShareNamespaceDiffStatus'Done diff ->
            [ "diff" .= toJSON diff,
              "tag" .= ("done" :: Text)
            ]
          ShareNamespaceDiffStatus'StillComputing ->
            [ "tag" .= ("computing" :: Text)
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

instance FromJSON ShareTermDiffResponse where
  parseJSON = withObject "ShareTermDiffResponse" $ \o -> do
    project <- o .: "project"
    oldBranch <- o .: "oldBranchRef"
    newBranch <- o .: "newBranchRef"
    oldTerm <- o .: "oldTerm"
    newTerm <- o .: "newTerm"
    diffKind <- o .: "diffKind"
    diff <- case (diffKind :: Text) of
      "diff" -> do
        diffValue <- o .: "diff"
        pure $ DisplayObjectDiff diffValue
      "mismatched" -> pure $ MismatchedDisplayObjects (termDefinition oldTerm) (termDefinition newTerm)
      t -> fail $ "Invalid ShareTermDiffResponse diffKind: " <> show t
    pure ShareTermDiffResponse {project, oldBranch, newBranch, oldTerm, newTerm, diff}

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

instance FromJSON ShareTypeDiffResponse where
  parseJSON = withObject "ShareTypeDiffResponse" $ \o -> do
    project <- o .: "project"
    oldBranch <- o .: "oldBranchRef"
    newBranch <- o .: "newBranchRef"
    oldType <- o .: "oldType"
    newType <- o .: "newType"
    diffKind <- o .: "diffKind"
    diff <- case (diffKind :: Text) of
      "diff" -> do
        diffValue <- o .: "diff"
        pure $ DisplayObjectDiff diffValue
      "mismatched" -> pure $ MismatchedDisplayObjects (typeDefinition oldType) (typeDefinition newType)
      t -> fail $ "Invalid ShareTypeDiffResponse diffKind: " <> show t
    pure ShareTypeDiffResponse {project, oldBranch, newBranch, oldType, newType, diff}
