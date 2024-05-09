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
    termSummaryForReferent,
    TermSummary (..),
    TypeSummaryAPI,
    serveTypeSummary,
    typeSummaryForReference,
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
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types (PathSegments (..))
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (ToServerError (..))
import U.Codebase.Referent qualified as V2Referent
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Conversions qualified as CV
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    TermTag (..),
    TypeTag,
    mayDefaultWidth,
  )
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
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

instance FromJSON TermSummary where
  parseJSON = withObject "TermSummary" $ \o -> do
    displayName <- o .: "displayName"
    hash <- o .: "hash"
    summary <- o .: "summary"
    tag <- o .: "tag"
    pure $ TermSummary {..}

serveTermSummary ::
  Referent ->
  Maybe Name ->
  CausalId ->
  Maybe Path.Path ->
  Maybe Width ->
  CodebaseM e TermSummary
serveTermSummary referent mayName rootCausalId relativeTo mayWidth = do
  rootBranchHashId <- HashQ.expectNamespaceIdsByCausalIdsOf id rootCausalId
  let v2Referent = CV.referent1to2 referent
  sig <-
    Codebase.loadTypeOfReferent v2Referent
      `whenNothingM` unrecoverableError (MissingSignatureForTerm $ V2Referent.toReference v2Referent)
  termSummaryForReferent v2Referent sig mayName rootBranchHashId relativeTo mayWidth

termSummaryForReferent ::
  V2Referent.Referent ->
  Type.Type Symbol Ann ->
  Maybe Name ->
  BranchHashId ->
  Maybe Path.Path ->
  Maybe Width ->
  CodebaseM e TermSummary
termSummaryForReferent referent typeSig mayName rootBranchHashId relativeTo mayWidth = do
  let shortHash = V2Referent.toShortHash referent
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = V2Referent.toReference referent
  let deps = Type.labeledDependencies typeSig
  namesPerspective <- NLOps.namesPerspectiveForRootAndPath rootBranchHashId (coerce . Path.toList $ relativeToPath)
  pped <- PPEPostgres.ppedForReferences namesPerspective deps
  let formattedTypeSig = Backend.formatSuffixedType pped width typeSig
  let summary = mkSummary termReference formattedTypeSig
  tag <- Backend.getTermTag referent typeSig
  pure $ TermSummary displayName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference sig =
      if Reference.isBuiltin reference
        then BuiltinObject sig
        else UserObject sig

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

instance FromJSON TypeSummary where
  parseJSON = withObject "TypeSummary" $ \o -> do
    displayName <- o .: "displayName"
    hash <- o .: "hash"
    summary <- o .: "summary"
    tag <- o .: "tag"
    pure $ TypeSummary {..}

serveTypeSummary ::
  Reference ->
  Maybe Name ->
  Maybe Width ->
  CodebaseM e TypeSummary
serveTypeSummary reference mayName mayWidth = do
  typeSummaryForReference reference mayName mayWidth

typeSummaryForReference ::
  Reference ->
  Maybe Name ->
  Maybe Width ->
  CodebaseM e TypeSummary
typeSummaryForReference reference mayName mayWidth = do
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
