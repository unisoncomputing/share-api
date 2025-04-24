{-# LANGUAGE DataKinds #-}

module Share.Web.Share.Diffs.Types where

import Data.Aeson
import Share.IDs
import Share.NamespaceDiffs (NamespaceAndLibdepsDiff, NamespaceDiffError)
import Share.NamespaceDiffs qualified as NamespaceDiffs
import Share.Postgres.IDs (BranchHash, CausalHash)
import Share.Prelude
import Share.Utils.Aeson (PreEncoded)
import Unison.Merge (IncoherentDeclReason (..))
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Server.Types (DisplayObjectDiff (..), TermDefinition, TermDefinitionDiff (..), TermTag, TypeDefinition, TypeDefinitionDiff (..), TypeTag)
import Unison.ShortHash (ShortHash)

type ShareNamespaceDiff =
  NamespaceAndLibdepsDiff (TermTag, ShortHash) (TypeTag, ShortHash) TermDefinition TypeDefinition TermDefinitionDiff TypeDefinitionDiff BranchHash

data ShareNamespaceDiffStatus
  = ShareNamespaceDiffStatus'Ok (PreEncoded ShareNamespaceDiff)
  | ShareNamespaceDiffStatus'Error NamespaceDiffError
  | ShareNamespaceDiffStatus'StillComputing

instance ToJSON ShareNamespaceDiffStatus where
  toJSON = \case
    ShareNamespaceDiffStatus'Ok diff ->
      object
        [ "diff" .= diff,
          "diffKind" .= ("ok" :: Text)
        ]
    ShareNamespaceDiffStatus'Error err ->
      object
        [ "diffKind" .= ("error" :: Text),
          "error"
            .= case err of
              NamespaceDiffs.ImpossibleError _ ->
                object
                  [ "errorKind" .= ("impossibleError" :: Text)
                  ]
              NamespaceDiffs.IncoherentDecl reason ->
                let f :: Text -> IncoherentDeclReason -> Value
                    f which reason =
                      object
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
              NamespaceDiffs.LibFoundAtUnexpectedPath _ ->
                object
                  [ "errorKind" .= ("libFoundAtUnexpectedPath" :: Text)
                  ]
              NamespaceDiffs.MissingEntityError _ ->
                object
                  [ "errorKind" .= ("missingEntityError" :: Text)
                  ]
        ]
    ShareNamespaceDiffStatus'StillComputing ->
      object
        [ "diffKind" .= ("computing" :: Text)
        ]

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
