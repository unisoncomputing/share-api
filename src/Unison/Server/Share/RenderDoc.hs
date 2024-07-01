{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Helper for rendering docs within a given namespace
module Unison.Server.Share.RenderDoc where

import Control.Monad.Except
import Data.Set qualified as Set
import Share.Backend qualified as Backend
import Share.Codebase.Types (CodebaseM, CodebaseRuntime)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.Causal.Queries qualified as HashQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types (PathSegments (..))
import Share.Prelude
import U.Codebase.Causal qualified as V2Causal
import Unison.Codebase.Path qualified as Path
import Unison.LabeledDependency qualified as LD
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.PrettyPrintEnvDecl.Postgres qualified as PostgresPPE
import Unison.Server.Doc (Doc)
import Unison.Server.Doc qualified as Doc
import Unison.Util.Pretty (Width)

-- | Find, eval, and render the first doc we find with any of the provided names within the given namespace
-- If no doc is found, return Nothing
--
-- Requires Name Lookups, currently only usable on Share.
findAndRenderDoc ::
  Set NameSegment ->
  CodebaseRuntime ->
  Path.Path ->
  CausalId ->
  Maybe Width ->
  CodebaseM e (Maybe Doc)
findAndRenderDoc docNames runtime namespacePath rootCausalId _mayWidth = runMaybeT do
  rootNamespaceHashId <- lift $ HashQ.expectNamespaceIdForCausal rootCausalId
  namespaceCausal <- MaybeT $ CausalQ.loadCausalNamespaceAtPath rootCausalId namespacePath
  shallowBranchAtNamespace <- lift $ V2Causal.value namespaceCausal
  namesPerspective <- NLOps.namesPerspectiveForRootAndPath rootNamespaceHashId (coerce $ Path.toList namespacePath)
  docRef <- MaybeT . pure $ Backend.findDocInBranch docNames shallowBranchAtNamespace
  eDoc <- lift $ Backend.evalDocRef runtime docRef
  let docDeps = Doc.dependencies eDoc <> Set.singleton (LD.TermReference docRef)
  docPPE <- PostgresPPE.ppedForReferences namesPerspective docDeps
  pure $ Doc.renderDoc docPPE eDoc
