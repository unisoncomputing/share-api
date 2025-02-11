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

import Data.Set qualified as Set
import Share.Backend qualified as Backend
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseM, CodebaseRuntime)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.IDs (CausalId)
import Share.Postgres.NameLookups.Ops qualified as NLOps
import Share.Postgres.NameLookups.Types (PathSegments (..))
import Share.Prelude
import Share.Utils.Caching.JSON qualified as Caching
import U.Codebase.Causal qualified as V2Causal
import Unison.Codebase.Path qualified as Path
import Unison.LabeledDependency qualified as LD
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.PrettyPrintEnvDecl.Postgres qualified as PostgresPPE
import Unison.Reference qualified as Reference
import Unison.Server.Doc (Doc)
import Unison.Server.Doc qualified as Doc
import Unison.ShortHash qualified as SH
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
  rootNamespaceHashId <- lift $ CausalQ.expectNamespaceIdsByCausalIdsOf id rootCausalId
  namespaceCausal <- MaybeT $ CausalQ.loadCausalNamespaceAtPath rootCausalId namespacePath
  shallowBranchAtNamespace <- lift $ V2Causal.value namespaceCausal
  docRef <- MaybeT . pure $ Backend.findDocInBranch docNames shallowBranchAtNamespace
  codebaseOwnerUserId <- asks Codebase.codebaseOwner
  let cacheKey =
        Caching.CacheKey
          { cacheTopic = "findAndRenderDoc",
            key = [("namespacePath", tShow namespacePath), ("docRef", SH.toText $ Reference.toShortHash docRef)],
            rootCausalId = Just rootCausalId,
            sandbox = Just codebaseOwnerUserId
          }

  lift $ Caching.usingJSONCache cacheKey do
    namesPerspective <- NLOps.namesPerspectiveForRootAndPath rootNamespaceHashId (coerce $ Path.toList namespacePath)
    eDoc <- Backend.evalDocRef runtime docRef
    let docDeps = Doc.dependencies eDoc <> Set.singleton (LD.TermReference docRef)
    docPPE <- PostgresPPE.ppedForReferences namesPerspective docDeps
    pure $ Doc.renderDoc docPPE eDoc
