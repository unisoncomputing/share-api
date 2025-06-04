module Unison.Server.Share.NamespaceDetails (namespaceDetails) where

import Data.Set qualified as Set
import Share.Codebase qualified as Codebase
import Share.Codebase.Types (CodebaseM, CodebaseRuntime)
import Share.Postgres.IDs (CausalId)
import Share.Prelude
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.Server.Share.RenderDoc qualified as RenderDoc
import Unison.Server.Types
  ( NamespaceDetails (..),
    UnisonHash,
  )
import Unison.Util.Pretty qualified as Pretty

namespaceDetails ::
  CodebaseRuntime IO ->
  Path.Path ->
  CausalId ->
  Maybe Pretty.Width ->
  CodebaseM e (Maybe (NamespaceDetails, Maybe (NonEmpty Rt.Error)))
namespaceDetails runtime namespacePath rootCausalId mayWidth = runMaybeT $ do
  causalHashAtPath <- Causal.causalHash <$> MaybeT (Codebase.loadCausalNamespaceAtPath rootCausalId namespacePath)
  readmeResult <- lift $ RenderDoc.findAndRenderDoc readmeNames runtime namespacePath rootCausalId mayWidth
  (mayDoc, mayErrs) <- case readmeResult of
    Nothing -> pure (Nothing, Nothing)
    Just (doc, mayDocErrors) -> do
      pure (Just doc, mayDocErrors)
  pure $ (NamespaceDetails namespacePath ("#" <> from @CausalHash @UnisonHash causalHashAtPath) mayDoc, mayErrs)
  where
    readmeNames = Set.fromList . fmap NameSegment $ ["README", "Readme", "ReadMe", "readme"]
