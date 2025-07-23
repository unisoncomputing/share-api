{-# OPTIONS_GHC -Wno-unused-imports #-}

module Unison.Server.Share.Docs (docsForDefinitionNamesOf) where

import Control.Lens
import Control.Lens qualified as Cons
import Data.Set qualified as Set
import Share.Codebase (loadTypesOfTermsOf)
import Share.Codebase qualified as Codebase
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Prelude
import Share.Web.Errors (SomeServerError)
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.NamesWithHistory (SearchType (ExactName))
import Unison.Reference (TermReference)
import Unison.Referent qualified as V1Referent
import Unison.Runtime.IOSource qualified as DD
import Unison.Server.NameSearch (NameSearch (..), lookupRelativeHQRefs')
import Unison.Server.NameSearch qualified as NameSearch
import Unison.Server.NameSearch.Postgres qualified as NS
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Util.Monoid (foldMapM)

-- | From Unison.Server.Backend
-- Fetch the docs associated with the given name.
-- Returns all references with a Doc type which are at the name provided, or at '<name>.doc'.
docsForDefinitionNamesOf ::
  forall m s t.
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  NamesPerspective m ->
  Traversal s t Name [TermReference] ->
  s ->
  m t
docsForDefinitionNamesOf codebase namesPerspective trav s = do
  s
    & asListOf trav %%~ \names -> do
      let potentialDocNames =
            names <&> \name ->
              [name, name Cons.:> NameSegment "doc"]
                <&> HQ'.NameOnly
      refs <- NS.termRefsByHQNamesOf namesPerspective (traversed . traversed) potentialDocNames
      filterForDocs (Set.toList <$> (fold <$> refs))
  where
    filterForDocs :: [[V1Referent.Referent]] -> m [[TermReference]]
    filterForDocs refs = do
      let references =
            refs <&> mapMaybe \case
              V1Referent.Ref r -> Just r
              _ -> Nothing
      termsWithTypes <- zipWith zip references <$> Codebase.loadTypesOfTermsOf codebase (traversed . traversed) references
      pure $
        termsWithTypes
          <&> fmap
            ( \(r, mayType) ->
                do
                  typ <- mayType
                  if Typechecker.isSubtype typ (Type.ref mempty DD.doc2Ref)
                    then Just r
                    else Nothing
            )
          <&> catMaybes
