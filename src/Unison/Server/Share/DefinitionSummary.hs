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
  ( serveTermSummary,
    termSummaryForReferent,
    serveTypeSummary,
    typeSummaryForReference,
  )
where

import Share.Backend qualified as Backend
import Share.Codebase qualified as Codebase
import Share.Postgres (QueryM, unrecoverableError)
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import U.Codebase.Referent qualified as V2Referent
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.SqliteCodebase.Conversions qualified as CV
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Server.Backend (BackendError (..))
import Unison.Server.Share.DefinitionSummary.Types (TermSummary (..), TypeSummary (..))
import Unison.Server.Types (mayDefaultWidth)
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Util.Pretty (Width)

serveTermSummary ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  Referent ->
  Maybe Name ->
  NamesPerspective m ->
  Maybe Width ->
  m TermSummary
serveTermSummary codebase referent mayName np mayWidth = do
  let v2Referent = CV.referent1to2 referent
  sig <-
    Codebase.loadTypesOfReferentsOf codebase id v2Referent
      `whenNothingM` unrecoverableError (MissingSignatureForTerm $ V2Referent.toReference v2Referent)
  termSummaryForReferent v2Referent sig mayName np mayWidth

termSummaryForReferent ::
  (QueryM m) =>
  V2Referent.Referent ->
  Type.Type Symbol Ann ->
  Maybe Name ->
  NamesPerspective m ->
  Maybe Width ->
  m TermSummary
termSummaryForReferent referent typeSig mayName namesPerspective mayWidth = do
  let shortHash = V2Referent.toShortHash referent
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  let termReference = V2Referent.toReference referent
  let deps = Type.labeledDependencies typeSig
  pped <- PPEPostgres.ppedForReferences namesPerspective deps
  let formattedTypeSig = Backend.formatSuffixedType pped width typeSig
  let summary = mkSummary termReference formattedTypeSig
  tag <- Backend.getTermTagsOf id (referent, typeSig)
  pure $ TermSummary displayName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference sig =
      if Reference.isBuiltin reference
        then BuiltinObject sig
        else UserObject sig

serveTypeSummary ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  Reference ->
  Maybe Name ->
  Maybe Width ->
  m TypeSummary
serveTypeSummary codebase reference mayName mayWidth = do
  typeSummaryForReference codebase reference mayName mayWidth

typeSummaryForReference ::
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  Reference ->
  Maybe Name ->
  Maybe Width ->
  m TypeSummary
typeSummaryForReference codebase reference mayName mayWidth = do
  let shortHash = Reference.toShortHash reference
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  tag <- Backend.getTypeTagsOf id reference
  displayDecl <- Backend.displayTypesOf codebase id reference
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
