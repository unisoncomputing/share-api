{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Share.DefinitionSummary.Types
  ( TermSummaryAPI,
    TermSummary (..),
    TypeSummaryAPI,
    TypeSummary (..),
  )
where

import Data.Aeson
import Servant (Capture, QueryParam, (:>))
import Servant.Server (err500)
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors (ToServerError (..))
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    TermTag (..),
    TypeTag,
  )
import Unison.ShortHash qualified as SH
import Unison.Util.Pretty (Width)

data SummaryError = MissingSignatureForTerm Reference
  deriving (Show)

instance ToServerError SummaryError where
  toServerError = \case
    MissingSignatureForTerm _reference ->
      ("missing-term-signature", err500)

instance Logging.Loggable SummaryError where
  toLog = \case
    e@(MissingSignatureForTerm {}) ->
      Logging.withSeverity Logging.Error . Logging.showLog $ e

type TermSummaryAPI =
  "definitions"
    :> "terms"
    :> "by-hash"
    :> Capture "hash" Referent
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

data TermSummary = TermSummary
  { displayName :: HQ.HashQualified Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TermTag
  }
  deriving (Generic, Show)

instance ToJSON TermSummary where
  toJSON (TermSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

instance FromJSON TermSummary where
  parseJSON = withObject "TermSummary" $ \o -> do
    displayName <- o .: "displayName"
    hash <- o .: "hash"
    summary <- o .: "summary"
    tag <- o .: "tag"
    pure $ TermSummary {..}

type TypeSummaryAPI =
  "definitions"
    :> "types"
    :> "by-hash"
    :> Capture "hash" Reference
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

data TypeSummary = TypeSummary
  { displayName :: HQ.HashQualified Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TypeTag
  }
  deriving (Generic, Show)

instance ToJSON TypeSummary where
  toJSON (TypeSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

instance FromJSON TypeSummary where
  parseJSON = withObject "TypeSummary" $ \o -> do
    displayName <- o .: "displayName"
    hash <- o .: "hash"
    summary <- o .: "summary"
    tag <- o .: "tag"
    pure $ TypeSummary {..}
