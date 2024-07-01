{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Share.DefinitionSummary
  ( TermSummaryAPI,
    serveTermSummary,
    TermSummary (..),
    TypeSummaryAPI,
    serveTypeSummary,
    TypeSummary (..),
  )
where

import Data.Aeson
import Servant (Capture, QueryParam, (:>))
import Servant.Server (err500)
import Share.Backend qualified as Backend
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseM)
import Share.Postgres (QueryM (unrecoverableError))
import Share.Postgres.Hashes.Queries qualified as HashQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (ToServerError (..))
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    TermTag (..),
    TypeTag,
    mayDefaultWidth,
  )
import Unison.ShortHash qualified as SH
import Unison.Type qualified as Type
import Unison.Util.Pretty (Width)

data SummaryError = MissingSignatureForTerm Reference
  deriving (Show)

instance ToServerError SummaryError where
  toServerError = \case
    MissingSignatureForTerm _reference ->
      ("missing-term-signature", err500)

instance Logging.Loggable SummaryError where
  toLog = \case
    e@(MissingSignatureForTerm {}) ->
      Logging.withSeverity Logging.Error . Logging.showLog $ e

type TermSummaryAPI =
  "definitions"
    :> "terms"
    :> "by-hash"
    :> Capture "hash" Referent
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

data TermSummary = TermSummary
  { displayName :: HQ.HashQualified Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TermTag
  }
  deriving (Generic, Show)

instance ToJSON TermSummary where
  toJSON (TermSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

serveTermSummary ::
  Referent ->
  Maybe Name ->
  CausalId ->
  Maybe Path.Path ->
  Maybe Width ->
  CodebaseM e TermSummary
serveTermSummary referent mayName rootCausalId relativeTo mayWidth = do
  let shortHash = Referent.toShortHash referent
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = Referent.toReference referent
  let v2Referent = Cv.referent1to2 referent
  rootBranchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id rootCausalId
  sig <- Codebase.loadTypeOfReferent v2Referent
  case sig of
    Nothing ->
      unrecoverableError (MissingSignatureForTerm termReference)
    Just typeSig -> do
      let deps = Type.labeledDependencies typeSig
      namesPerspective <- NLOps.namesPerspectiveForRootAndPath rootBranchHashId (NameLookups.PathSegments . fmap NameSegment.toUnescapedText . Path.toList $ relativeToPath)
      pped <- PPEPostgres.ppedForReferences namesPerspective deps
      let formattedTermSig = Backend.formatSuffixedType pped width typeSig
      let summary = mkSummary termReference formattedTermSig
      tag <- Backend.getTermTag v2Referent typeSig
      pure $ TermSummary displayName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if Reference.isBuiltin reference
        then BuiltinObject termSig
        else UserObject termSig

type TypeSummaryAPI =
  "definitions"
    :> "types"
    :> "by-hash"
    :> Capture "hash" Reference
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

data TypeSummary = TypeSummary
  { displayName :: HQ.HashQualified Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TypeTag
  }
  deriving (Generic, Show)

instance ToJSON TypeSummary where
  toJSON (TypeSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

serveTypeSummary ::
  Reference ->
  Maybe Name ->
  CausalId ->
  Maybe Path.Path ->
  Maybe Width ->
  CodebaseM e TypeSummary
serveTypeSummary reference mayName _root _relativeTo mayWidth = do
  let shortHash = Reference.toShortHash reference
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  tag <- Backend.getTypeTag reference
  displayDecl <- Backend.displayType reference
  let syntaxHeader = Backend.typeToSyntaxHeader width displayName displayDecl
  pure $
    TypeSummary
      { displayName = displayName,
        hash = shortHash,
        summary = bimap Backend.mungeSyntaxText Backend.mungeSyntaxText syntaxHeader,
        tag = tag
      }
  where
    width = mayDefaultWidth mayWidth
