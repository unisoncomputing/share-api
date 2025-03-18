{-# LANGUAGE DataKinds #-}

module Share.Web.Share.Diffs.Types where

import Data.Aeson
import Share.IDs
import Share.NamespaceDiffs (NamespaceAndLibdepsDiff)
import Share.Postgres.IDs (BranchHash, CausalHash)
import Share.Prelude
import Share.Utils.Aeson (PreEncoded)
import Unison.Server.Types (DisplayObjectDiff (..), TermDefinition, TermDefinitionDiff (..), TermTag, TypeDefinition, TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)

type ShareNamespaceDiff =
  NamespaceAndLibdepsDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff BranchHash

data ShareNamespaceDiffResponse = ShareNamespaceDiffResponse
  { project :: ProjectShortHand,
    oldRef :: BranchOrReleaseShortHand,
    oldRefHash :: Maybe (PrefixedHash "#" CausalHash),
    newRef :: BranchOrReleaseShortHand,
    newRefHash :: Maybe (PrefixedHash "#" CausalHash),
    diff :: PreEncoded ShareNamespaceDiff
  }

instance ToJSON ShareNamespaceDiffResponse where
  toJSON (ShareNamespaceDiffResponse {diff, project, oldRef, newRef, oldRefHash, newRefHash}) =
    object
      [ "diff" .= diff,
        "project" .= project,
        "oldRef" .= oldRef,
        "oldRefHash" .= oldRefHash,
        "newRef" .= newRef,
        "newRefHash" .= newRefHash
      ]
    where

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
