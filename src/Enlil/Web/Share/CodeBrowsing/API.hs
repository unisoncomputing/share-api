{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enlil.Web.Share.CodeBrowsing.API where

import Data.Text (Text)
import Enlil.Utils.Caching
import Enlil.Utils.Servant (OptionalCapture, RequiredQueryParam)
import Servant
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.Share.DefinitionSummary (TermSummary, TypeSummary)
import Unison.Server.Share.FuzzyFind qualified as Fuzzy
import Unison.Server.Share.NamespaceListing (NamespaceListing)
import Unison.Server.Types (DefinitionDisplayResults, NamespaceDetails)
import Unison.Util.Pretty

-- | Shared between user codebase and project-branch code browsing
type CodeBrowseAPI =
  ( ("browse" :> BrowseEndpoint)
      :<|> ("definitions" :> "by-name" :> DefinitionsByNameEndpoint)
      :<|> ("definitions" :> "by-hash" :> DefinitionsByHashEndpoint)
      :<|> ("definitions" :> "terms" :> "by-hash" :> TermSummaryEndpoint)
      :<|> ("definitions" :> "types" :> "by-hash" :> TypeSummaryEndpoint)
      :<|> ("find" :> FindEndpoint)
      :<|> ("namespaces" :> "by-name" :> NamespaceByNameEndpoint)
  )

type TermSummaryEndpoint =
  Capture "hash" Referent
    :> "summary"
    :> QueryParam "name" Name
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON TermSummary)

type TypeSummaryEndpoint =
  Capture "hash" Reference
    :> "summary"
    :> QueryParam "name" Name
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON TypeSummary)

-- GET /codebases/:user_handle/browse?within=base.List
type BrowseEndpoint =
  QueryParam "relativeTo" Path.Path
    :> QueryParam "namespace" Path.Path
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON NamespaceListing)

type DefinitionsByNameEndpoint =
  Capture "name" (HQ.HashQualified Name)
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON DefinitionDisplayResults)

type DefinitionsByHashEndpoint =
  Capture "hash" Referent
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON DefinitionDisplayResults)

type FindEndpoint =
  QueryParam "relativeTo" Path.Path
    :> QueryParam "limit" Int
    :> QueryParam "renderWidth" Width
    :> RequiredQueryParam "query" Text
    :> QueryFlag "search-dependencies"
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] [(Fuzzy.Alignment, Fuzzy.FoundResult)]

type NamespaceByNameEndpoint =
  OptionalCapture "namespace" Path.Path
    :> QueryParam "renderWidth" Width
    :> QueryParam "rootHash" CausalHash
    :> Get '[JSON] (Cached JSON NamespaceDetails)
