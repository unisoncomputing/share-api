{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Share.FuzzyFind where

import Control.Lens
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord qualified as Ord
import Data.Text qualified as Text
import Safe (lastMay)
import Servant
  ( QueryParam,
    (:>),
  )
import Share.Backend qualified as Backend
import Share.Codebase (CodebaseEnv)
import Share.Codebase qualified as Codebase
import Share.Postgres (QueryM)
import Share.Postgres.Causal.Queries qualified as CausalQ
import Share.Postgres.IDs (BranchHashId, CausalId)
import Share.Postgres.NameLookups.Ops qualified as NameLookupOps
import Share.Postgres.NameLookups.Queries qualified as Q
import Share.Postgres.NameLookups.Types (NamedRef (..), PathSegments (..))
import Share.Postgres.NameLookups.Types qualified as NameLookups
import Share.Postgres.NamesPerspective.Ops qualified as NPOps
import Share.Postgres.NamesPerspective.Types (NamesPerspective)
import Share.Prelude
import Share.PrettyPrintEnvDecl.Postgres qualified as PPED
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Server.Backend (termEntryLabeledDependencies, typeEntryLabeledDependencies)
import Unison.Server.Backend qualified as UBackend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    HashQualifiedName,
    NamedTerm,
    NamedType,
    UnisonName,
  )
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)

type FuzzyFindAPI =
  "find"
    :> QueryParam "rootBranch" SCH.ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "limit" Int
    :> QueryParam "renderWidth" Width
    :> QueryParam "query" String
    :> APIGet [(Alignment, FoundResult)]

data FoundTerm = FoundTerm
  { bestFoundTermName :: HashQualifiedName,
    namedTerm :: NamedTerm
  }
  deriving (Generic, Show)

data FoundType = FoundType
  { bestFoundTypeName :: HashQualifiedName,
    typeDef :: DisplayObject SyntaxText SyntaxText,
    namedType :: NamedType
  }
  deriving (Generic, Show)

instance ToJSON FoundType where
  toJSON (FoundType {bestFoundTypeName, typeDef, namedType}) =
    object
      [ "bestFoundTypeName" Aeson..= bestFoundTypeName,
        "typeDef" Aeson..= typeDef,
        "namedType" Aeson..= namedType
      ]

instance FromJSON FoundType where
  parseJSON = withObject "FoundType" $ \o -> do
    bestFoundTypeName <- o .: "bestFoundTypeName"
    typeDef <- o .: "typeDef"
    namedType <- o .: "namedType"
    pure FoundType {bestFoundTypeName, typeDef, namedType}

instance ToJSON FoundTerm where
  toJSON (FoundTerm {bestFoundTermName, namedTerm}) =
    object
      [ "bestFoundTermName" Aeson..= bestFoundTermName,
        "namedTerm" Aeson..= namedTerm
      ]

instance FromJSON FoundTerm where
  parseJSON = withObject "FoundTerm" $ \o -> do
    bestFoundTermName <- o .: "bestFoundTermName"
    namedTerm <- o .: "namedTerm"
    pure FoundTerm {bestFoundTermName, namedTerm}

data FoundResult
  = FoundTermResult FoundTerm
  | FoundTypeResult FoundType
  deriving (Generic, Show)

instance ToJSON FoundResult where
  toJSON = \case
    FoundTermResult ft -> object ["tag" Aeson..= String "FoundTermResult", "contents" Aeson..= ft]
    FoundTypeResult ft -> object ["tag" Aeson..= String "FoundTypeResult", "contents" Aeson..= ft]

instance FromJSON FoundResult where
  parseJSON = withObject "FoundResult" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "FoundTermResult" -> FoundTermResult <$> o .: "contents"
      "FoundTypeResult" -> FoundTypeResult <$> o .: "contents"
      _ -> fail $ "Unknown FoundResult tag: " <> show tag

serveFuzzyFind ::
  forall m.
  (QueryM m) =>
  CodebaseEnv ->
  -- | Whether the root is a scratch root
  Bool ->
  -- | Whether to search in dependencies
  Bool ->
  CausalId ->
  Path.Path ->
  Maybe Int ->
  Maybe Width ->
  Text ->
  m [(Alignment, FoundResult)]
serveFuzzyFind codebase inScratch searchDependencies rootCausal perspective mayLimit typeWidth query = fromMaybeT (pure []) do
  bhId <- CausalQ.expectNamespaceIdsByCausalIdsOf id rootCausal
  namesPerspective <- lift $ NPOps.namesPerspectiveForRootAndPath bhId (coerce $ Path.toList perspective)
  -- If were browsing at a scratch root we need to include one level of dependencies even if
  -- the 'include-dependencies' flag is not set
  -- since the projects are all "dependencies" of the scratch root as far as name-lookups
  -- are concerned.
  let isScratchRootSearch = inScratch
  -- Include dependencies if they were explicitly requested OR if we're running a search
  -- from a scratch root
  let includeDependencies = isScratchRootSearch || searchDependencies
  let (querySegments, mayLastQuerySegment) = prepareQuery (Text.unpack query)
  (dbTermMatches, dbTypeMatches) <- case (NonEmpty.nonEmpty querySegments, mayLastQuerySegment) of
    -- Just return no results if the query is empty
    (Nothing, _) -> empty
    (_, Nothing) -> empty
    (Just preparedQuery, Just lastSegment) -> do
      (terms, types) <- lift $ NameLookupOps.fuzzySearchDefinitions includeDependencies namesPerspective limit preparedQuery lastSegment
      pure (terms, types)
  let prepareMatch :: NamedRef Backend.FoundRef -> (PathSegments, Alignment, UnisonName, [Backend.FoundRef])
      prepareMatch name@(NamedRef {reversedSegments}) =
        let renderedName = NameLookups.reversedNameToNamespaceText reversedSegments
            segments = computeMatchSegments querySegments name
            alignment =
              Alignment
                { -- We used to return a score, but now we just sort all the results server-side.
                  -- The score we return is ignored by the frontend.
                  score = 0,
                  result = MatchResult {segments}
                }
         in (NameLookups.reversedNameToPathSegments reversedSegments, alignment, renderedName, [NameLookups.ref name])
  let preparedTerms :: [(Q.FuzzySearchScore, (PathSegments, Alignment, UnisonName, [Backend.FoundRef]))]
      preparedTerms =
        dbTermMatches
          & traversed . _2 %~ \match ->
            match
              & (fmap (\(ref, ct) -> Backend.FoundTermRef $ Cv.referent2to1UsingCT (fromMaybe (error "serveFuzzyFind: CT required but not found") ct) ref))
              & prepareMatch
  let preparedTypes :: [(Q.FuzzySearchScore, (PathSegments, Alignment, UnisonName, [Backend.FoundRef]))]
      preparedTypes =
        dbTypeMatches
          & traversed . _2 %~ \match ->
            match
              & (fmap (Backend.FoundTypeRef . Cv.reference2to1))
              & prepareMatch
  let alignments ::
        ( [ ( PathSegments,
              Alignment,
              UnisonName,
              [Backend.FoundRef]
            )
          ]
        )
      alignments =
        (preparedTerms <> preparedTypes)
          & List.sortOn (\(score, _) -> Ord.Down score)
          & fmap snd
  (join <$> traverse (lift . loadEntry includeDependencies bhId namesPerspective) alignments)
  where
    limit = fromMaybe 10 mayLimit
    loadEntry :: Bool -> BranchHashId -> NamesPerspective m -> (PathSegments, Alignment, Text, [Backend.FoundRef]) -> m [(Alignment, FoundResult)]
    loadEntry includeDependencies bhId searchPerspective (pathToMatch, a, n, refs) = do
      namesPerspective <-
        -- If we're including dependencies we need to ensure each match's type signature is
        -- rendered using a ppe with that dependency's names.
        -- So we re-compute the perspective for each match.
        --
        -- If not we can use the same perspective for every match.
        if includeDependencies
          then do
            NPOps.namesPerspectiveForRootAndPath bhId (coerce (Path.toList perspective) <> pathToMatch)
          else pure searchPerspective
      let partitionedRefs =
            refs <&> \case
              Backend.FoundTermRef r -> Left (Cv.referent1to2 r)
              Backend.FoundTypeRef r -> Right r
      refsWithTypeInfo <-
        partitionedRefs
          & asListOf (traversed . _Left) %%~ \referents -> do
            typeInfo <- Codebase.expectTypesOfReferentsOf codebase traversed referents
            pure $ zip typeInfo referents
      entries <-
        refsWithTypeInfo
          & (traversed . _Left)
            %%~ ( \(typ, r) -> do
                    (r,) <$> Backend.termListEntry typ (ExactName (NameSegment n) r)
                )
          >>= (traversed . _Right)
            %%~ ( \r -> do
                    (r,) <$> Backend.typeListEntry (ExactName (NameSegment n) r)
                )
      let allLabeledDependencies = foldMap (either (termEntryLabeledDependencies . snd) (typeEntryLabeledDependencies . snd)) entries
      pped <- PPED.ppedForReferences namesPerspective allLabeledDependencies
      let ppe = PPED.suffixifiedPPE pped
      for entries \case
        Left (_r, termEntry) ->
          pure
            ( a,
              FoundTermResult
                . FoundTerm
                  -- Use the name from the search here rather than the pretty printer best-name
                  (Name.toText $ HQ'.toName $ UBackend.termEntryHQName termEntry)
                $ Backend.termEntryToNamedTerm ppe typeWidth termEntry
            )
        Right (r, typeEntry) -> do
          let namedType = Backend.typeEntryToNamedType typeEntry
          -- Use the name from the search here rather than the pretty printer best-name
          let typeName = (Name.toText $ HQ'.toName $ UBackend.typeEntryHQName typeEntry)
          typeHeader <- Backend.typeDeclHeader codebase ppe r
          let ft = FoundType typeName typeHeader namedType
          pure (a, FoundTypeResult ft)

data Alignment = Alignment
  { score :: Int,
    result :: MatchResult
  }
  deriving stock (Generic)

data MatchResult = MatchResult
  { segments :: [MatchSegment]
  }
  deriving stock (Generic)

data MatchSegment
  = Gap Text
  | Match Text
  deriving stock (Show, Generic)

instance ToJSON Alignment where
  toJSON (Alignment {score, result}) =
    object ["score" Aeson..= score, "result" Aeson..= result]

instance FromJSON Alignment where
  parseJSON = withObject "Alignment" $ \o -> do
    score <- o .: "score"
    result <- o .: "result"
    pure Alignment {score, result}

instance ToJSON MatchResult where
  toJSON (MatchResult {segments}) = object ["segments" Aeson..= toJSON segments]

instance FromJSON MatchResult where
  parseJSON = withObject "MatchResult" $ \o -> do
    segments <- o .: "segments"
    pure MatchResult {segments}

instance ToJSON MatchSegment where
  toJSON = \case
    Gap s -> object ["tag" Aeson..= String "Gap", "contents" Aeson..= s]
    Match s -> object ["tag" Aeson..= String "Match", "contents" Aeson..= s]

instance FromJSON MatchSegment where
  parseJSON = withObject "MatchSegment" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "Gap" -> Gap <$> o .: "contents"
      "Match" -> Match <$> o .: "contents"
      _ -> fail $ "Unknown MatchSegment tag: " <> show tag

-- After finding a search results with fuzzy find we do some post processing to
-- refine the result:
--  * Sort:
--      we sort both on the FZF score and the number of segments in the FQN
--      preferring shorter FQNs over longer. This helps with things like forks
--      of base.
--  * Dedupe:
--      we dedupe on the found refs to avoid having several rows of a
--      definition with different names in the result set.
--
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> computeMatchSegments ["foo", "baz"] (S.NamedRef (NameLookups.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- [Match "foo",Gap ".bar.",Match "baz"]
--
-- >>> computeMatchSegments ["Li", "Ma"] (S.NamedRef (NameLookups.ReversedName ("foldMap" NonEmpty.:| ["List", "data"])) ())
-- [Gap "data.",Match "Li",Gap "st.fold",Match "Ma",Gap "p"]
computeMatchSegments ::
  [Text] ->
  (NamedRef r) ->
  [MatchSegment]
computeMatchSegments query (NamedRef {reversedSegments}) =
  let nameText = NameLookups.reversedNameToNamespaceText reversedSegments
      -- This will be a list of _lower-cased_ match segments, but we need to reclaim the
      -- casing from the actual name.
      matchSegmentShape = List.unfoldr splitIntoSegments (filter (not . Text.null) . map Text.toLower $ query, Text.toLower nameText)
   in List.unfoldr reCasifySegments (matchSegmentShape, nameText)
  where
    -- The actual matching is case-insensitive but we want to preserve the casing of the
    -- actual name, so we use the size of match segments to segment the actual name which has
    -- the correct case.
    reCasifySegments :: ([MatchSegment], Text) -> Maybe (MatchSegment, ([MatchSegment], Text))
    reCasifySegments = \case
      ([], _) -> Nothing
      (Gap gap : restShape, name) ->
        let (actualGap, restName) = Text.splitAt (Text.length gap) name
         in Just (Gap actualGap, (restShape, restName))
      (Match match : restShape, name) ->
        let (actualMatch, restName) = Text.splitAt (Text.length match) name
         in Just (Match actualMatch, (restShape, restName))
    -- Using the query, split the match into chunks of 'match' or 'gap'
    splitIntoSegments :: ([Text], Text) -> Maybe (MatchSegment, ([Text], Text))
    splitIntoSegments = \case
      (_, "") -> Nothing
      ([], rest) -> Just (Gap rest, ([], ""))
      (q : qs, name) ->
        Text.breakOn q name
          & \case
            ("", rest) ->
              case Text.stripPrefix q rest of
                Nothing -> Nothing
                Just remainder ->
                  Just (Match q, (qs, remainder))
            (gap, rest) ->
              Just (Gap gap, (q : qs, rest))

-- | Splits a query into segments, where each segment must appear in order in any matching
-- names.
--
-- >>> prepareQuery "foo bar baz"
-- (["foo","bar","baz"],Just "baz")
--
-- Split camel-case style words into segments.
-- >>> prepareQuery "fMap"
-- (["f","Map"],Just "fMap")
--
-- Collapse multiple spaces
-- >>> prepareQuery "foo barBaz    boom"
-- (["foo","bar","Baz","boom"],Just "boom")
--
-- Split namespaces into segments with a required dot in between.
-- >>> prepareQuery "List.map"
-- (["List",".","map"],Just "map")
--
-- Shouldn't get multiple splits for capitalized letters
-- >>> prepareQuery "List.Map"
-- (["List",".","Map"],Just "Map")
prepareQuery :: String -> ([Text], Maybe Text)
prepareQuery query = (querySegments, lastSegment)
  where
    lastSegment = lastMay (Text.words (Text.pack query) >>= Text.splitOn ".")
    querySegments = do
      word <- words query
      xs <-
        word
          & List.foldl'
            ( \acc next -> case next of
                c
                  | Char.isUpper c -> [c] : acc
                  | Char.isSpace c -> "" : acc
                  | c == '.' -> "" : "." : acc
                  | otherwise -> case acc of
                      [] -> [[c]]
                      (last : rest) -> (last ++ [c]) : rest
            )
            []
          & reverse
          & filter (not . null)
      pure $ (Text.pack xs)
