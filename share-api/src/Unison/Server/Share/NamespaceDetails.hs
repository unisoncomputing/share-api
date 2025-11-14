module Unison.Server.Share.NamespaceDetails (namespaceDetails) where

import Data.Set qualified as Set
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseEnv, CodebaseRuntime)
import Share.Postgres (QueryM)
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Server.Share.RenderDoc qualified as RenderDoc
import Unison.Server.Types
  ( NamespaceDetails (..),
    UnisonHash,
  )
import Unison.Util.Pretty (Width)

namespaceDetails ::
  (QueryM m) =>
  CodebaseEnv ->
  CodebaseRuntime s IO ->
  Path.Path ->
  CausalId ->
  Maybe Width ->
  m (Maybe NamespaceDetails)
namespaceDetails codebase runtime namespacePath rootCausalId mayWidth = runMaybeT $ do
  causalHashAtPath <- Causal.causalHash <$> MaybeT (Codebase.loadCausalNamespaceAtPath codebase rootCausalId namespacePath)
  mayReadme <- lift $ RenderDoc.findAndRenderDoc codebase readmeNames runtime namespacePath rootCausalId mayWidth
  pure $ NamespaceDetails namespacePath ("#" <> from @CausalHash @UnisonHash causalHashAtPath) mayReadme
  where
    readmeNames = Set.fromList . fmap NameSegment $ ["README", "Readme", "ReadMe", "readme"]
