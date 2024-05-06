module Unison.Server.Share.NamespaceDetails (namespaceDetails) where

import Control.Monad.Except
import Data.Set qualified as Set
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseM, CodebaseRuntime)
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Path qualified as Path
import Unison.Server.Share.RenderDoc qualified as RenderDoc
import Unison.Server.Types
  ( NamespaceDetails (..),
    UnisonHash,
  )
import Unison.Util.Pretty (Width)

namespaceDetails ::
  CodebaseRuntime ->
  Path.Path ->
  CausalId ->
  Maybe Width ->
  CodebaseM e (Maybe NamespaceDetails)
namespaceDetails runtime namespacePath rootCausalId mayWidth = runMaybeT $ do
  causalHashAtPath <- Causal.causalHash <$> MaybeT (Codebase.loadCausalNamespaceAtPath rootCausalId namespacePath)
  mayReadme <- lift $ RenderDoc.findAndRenderDoc readmeNames runtime namespacePath rootCausalId mayWidth
  pure $ NamespaceDetails namespacePath ("#" <> from @CausalHash @UnisonHash causalHashAtPath) mayReadme
  where
    readmeNames = Set.fromList ["README", "Readme", "ReadMe", "readme"]
