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
    termSummaryForReferentsOf,
    serveTypeSummary,
    typeSummaryForReference,
  )
where

import Control.Lens
import Data.List (zipWith4)
import Share.Backend qualified as Backend
import Share.Codebase qualified as Codebase
import Share.Postgres (QueryM, unrecoverableError)
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPEPostgres
import U.Codebase.Referent qualified as V2Referent
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.SqliteCodebase.Conversions qualified as CV
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
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
  termSummaryForReferentsOf np mayWidth id (v2Referent, sig, mayName)

termSummaryForReferentsOf ::
  (QueryM m) =>
  NamesPerspective m ->
  Maybe Width ->
  Traversal s t (V2Referent.Referent, Type.Type Symbol Ann, Maybe Name) TermSummary ->
  s ->
  m t
termSummaryForReferentsOf namesPerspective mayWidth trav s = do
  s
    & asListOf trav %%~ \inputs -> do
      let (refs, typeSigs, mayNames) = unzip3 inputs
      let allDeps = foldMap Type.labeledDependencies typeSigs
      pped <- PPEPostgres.ppedForReferences namesPerspective allDeps
      let shortHashes = V2Referent.toShortHash <$> refs
      let displayNames = zipWith (\mayName shortHash -> maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName) mayNames shortHashes
      let termReferences = V2Referent.toReference <$> refs
      let formattedTypeSigs = Backend.formatSuffixedType pped width <$> typeSigs
      let summaries = zipWith mkSummary termReferences formattedTypeSigs
      tag <- Backend.getTermTagsOf traversed (zip refs typeSigs)
      pure $ zipWith4 TermSummary displayNames shortHashes summaries tag
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

-- TODO: batchify this
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
