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
    termSummariesForReferentsOf,
    serveTypeSummary,
    typeSummariesForReferencesOf,
  )
where

import Control.Lens
import Data.List (zipWith4)
import Share.Backend qualified as Backend
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodeCache)
import Share.Postgres (QueryM, unrecoverableError)
import Share.Postgres.NameLookups.Queries (NameSearchScope (TransitiveDependencies))
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
  termSummariesForReferentsOf np mayWidth id (v2Referent, sig, mayName)

termSummariesForReferentsOf ::
  (QueryM m) =>
  NamesPerspective m ->
  Maybe Width ->
  Traversal s t (V2Referent.Referent, Type.Type Symbol Ann, Maybe Name) TermSummary ->
  s ->
  m t
termSummariesForReferentsOf namesPerspective mayWidth trav s = do
  s
    & asListOf trav %%~ \inputs -> do
      let (refs, typeSigs, mayNames) = unzip3 inputs
      let allDeps = foldMap Type.labeledDependencies typeSigs
      pped <- PPEPostgres.ppedForReferences TransitiveDependencies namesPerspective allDeps
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
  CodeCache scope ->
  Reference ->
  Maybe Name ->
  Maybe Width ->
  m TypeSummary
serveTypeSummary codeCache reference mayName mayWidth = do
  typeSummariesForReferencesOf codeCache mayWidth id (reference, mayName)

typeSummariesForReferencesOf ::
  (QueryM m) =>
  CodeCache scope ->
  Maybe Width ->
  Traversal s t (Reference, Maybe Name) TypeSummary ->
  s ->
  m t
typeSummariesForReferencesOf codeCache mayWidth trav s = do
  s
    & asListOf trav %%~ \inputs -> do
      let (refs, mayNames) = unzip inputs
      let shortHashes = Reference.toShortHash <$> refs
      let displayNames =
            zipWith (\mayName shortHash -> maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName) mayNames shortHashes
      tags <- Backend.getTypeTagsOf traversed refs
      displayDecls <- Backend.displayTypesOf codeCache traversed refs
      let syntaxHeaders =
            zipWith (Backend.typeToSyntaxHeader width) displayNames displayDecls
              <&> bimap Backend.mungeSyntaxText Backend.mungeSyntaxText
      pure $ zipWith4 TypeSummary displayNames shortHashes syntaxHeaders tags
  where
    width = mayDefaultWidth mayWidth
