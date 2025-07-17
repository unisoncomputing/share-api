{-# OPTIONS_GHC -Wno-unused-imports #-}

module Unison.Server.Share.Docs (docsForDefinitionNamesOf) where

import Control.Lens qualified as Cons
import Share.Codebase qualified as Codebase
import Share.Postgres (QueryM)
import Share.Postgres qualified as PG
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
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Util.Monoid (foldMapM)

-- | From Unison.Server.Backend
-- Fetch the docs associated with the given name.
-- Returns all references with a Doc type which are at the name provided, or at '<name>.doc'.
docsForDefinitionName ::
  forall m.
  (QueryM m) =>
  Codebase.CodebaseEnv ->
  NameSearch m ->
  Name ->
  m [TermReference]
docsForDefinitionName codebase (NameSearch {termSearch}) name = do
  let potentialDocNames = [name, name Cons.:> NameSegment "doc"]
  refs <-
    potentialDocNames & foldMapM \name ->
      lookupRelativeHQRefs' termSearch ExactName (HQ'.NameOnly name)
  filterForDocs (toList refs)
  where
    filterForDocs :: [V1Referent.Referent] -> m [TermReference]
    filterForDocs rs = do
      rts <- fmap join . for rs $ \case
        V1Referent.Ref r ->
          maybe [] (pure . (r,)) <$> Codebase.loadTypeOfTerm codebase r
        _ -> pure []
      pure [r | (r, t) <- rts, Typechecker.isSubtype t (Type.ref mempty DD.doc2Ref)]
