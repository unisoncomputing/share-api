{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Share.BackgroundJobs.Search.DefinitionSync.Types
  ( TermOrTypeSummary (..),
    TermOrTypeTag (..),
    DefinitionDocument (..),
    DefnSearchToken (..),
    Occurrence (..),
    OccurrenceKind (..),
    VarId (..),
    Arity (..),
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Monoid (Sum (..))
import Data.Text qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Share.Postgres.IDs (BranchHashId)
import Share.Prelude
import Unison.DataDeclaration qualified as DD
import Unison.Name (Name)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary (..), TypeSummary (..))
import Unison.Server.Types (TermTag (..), TypeTag (..))
import Unison.ShortHash (ShortHash)

data TermOrTypeSummary = ToTTermSummary TermSummary | ToTTypeSummary TypeSummary
  deriving (Show)

instance ToJSON TermOrTypeSummary where
  toJSON (ToTTermSummary ts) = object ["kind" .= ("term" :: Text), "payload" .= ts]
  toJSON (ToTTypeSummary ts) = object ["kind" .= ("type" :: Text), "payload" .= ts]

instance FromJSON TermOrTypeSummary where
  parseJSON = withObject "TermOrTypeSummary" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "term" -> do
        ts <- o .: "payload"
        ts & withObject "TermSummary" \o -> do
          displayName <- o .: "displayName"
          hash <- o .: "hash"
          summary <- o .: "summary"
          tag <- o .: "tag"
          pure $ ToTTermSummary $ TermSummary {..}
      "type" -> do
        ts <- o .: "payload"
        ts & withObject "TypeSummary" \o -> do
          displayName <- o .: "displayName"
          hash <- o .: "hash"
          summary <- o .: "summary"
          tag <- o .: "tag"
          pure $ ToTTypeSummary $ TypeSummary {..}
      _ -> fail $ "Invalid kind: " <> Text.unpack kind

data TermOrTypeTag = ToTTermTag TermTag | ToTTypeTag TypeTag
  deriving stock (Show, Eq, Ord)

instance FromHttpApiData TermOrTypeTag where
  parseQueryParam = \case
    "doc" -> Right $ ToTTermTag Doc
    "test" -> Right $ ToTTermTag Test
    "plain" -> Right $ ToTTermTag Plain
    "data-constructor" -> Right $ ToTTermTag $ Constructor Data
    "ability-constructor" -> Right $ ToTTermTag $ Constructor Ability
    "data" -> Right $ ToTTypeTag Data
    "ability" -> Right $ ToTTypeTag Ability
    _ -> Left "Invalid TermOrTypeTag"

instance ToHttpApiData TermOrTypeTag where
  toQueryParam = \case
    ToTTermTag Doc -> "doc"
    ToTTermTag Test -> "test"
    ToTTermTag Plain -> "plain"
    ToTTermTag (Constructor Data) -> "data-constructor"
    ToTTermTag (Constructor Ability) -> "ability-constructor"
    ToTTypeTag Data -> "data"
    ToTTypeTag Ability -> "ability"

instance ToJSON TermOrTypeTag where
  toJSON = String . toQueryParam

instance FromJSON TermOrTypeTag where
  parseJSON = withText "TermOrTypeTag" $ \txt ->
    case parseQueryParam txt of
      Left err -> fail $ Text.unpack err
      Right tag -> pure tag

instance Hasql.EncodeValue TermOrTypeTag where
  encodeValue =
    Encoders.enum
      ( \case
          ToTTermTag tt -> encodeTermTag tt
          ToTTypeTag tt -> encodeTypeTag tt
      )
    where
      encodeTermTag = \case
        Doc -> "doc"
        Test -> "test"
        Plain -> "plain"
        Constructor Data -> "data-constructor"
        Constructor Ability -> "ability-constructor"
      encodeTypeTag = \case
        Data -> "data"
        Ability -> "ability"

instance Hasql.DecodeValue TermOrTypeTag where
  decodeValue = Decoders.enum $ \case
    "doc" -> pure $ ToTTermTag Doc
    "test" -> pure $ ToTTermTag Test
    "plain" -> pure $ ToTTermTag Plain
    "data-constructor" -> pure $ ToTTermTag $ Constructor Data
    "ability-constructor" -> pure $ ToTTermTag $ Constructor Ability
    "data" -> pure $ ToTTypeTag Data
    "ability" -> pure $ ToTTypeTag Ability
    _ -> fail "Invalid TermOrTypeTag"

-- | The number of occurences of this token in the search query.
-- E.g. for the query: 'Text -> Text -> Text', the Text type mention token would
-- occur 3 times, and the set would be:
-- {NameMention "Text" (Occurrence 1), NameMention "Text" (Occurrence 2), NameMention "Text" (Occurrence 3)}
newtype Occurrence = Occurrence Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON, Enum)
  deriving (Semigroup) via Sum Int
  deriving (Monoid) via Sum Int

data OccurrenceKind = ReturnPosition | Count Occurrence
  deriving stock (Show, Eq, Ord)

-- | An id for identifying unique type variables mentioned in a query.
-- E.g. 'map : (a -> b) -> List a -> List b' would have two type var Ids, one for a, one
-- for b, and would have occurrences 1 and 2 for each respectively.
newtype VarId = VarId Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON, Enum)

newtype Arity = Arity Int32
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON, Enum, Hasql.EncodeValue, Hasql.DecodeValue)

-- | Represents the possible ways we can search the global definitions index.
data DefnSearchToken typeRef
  = -- Allows searching by literal name
    NameToken Name
  | -- A mention of some external type or ability
    TypeMentionToken typeRef OccurrenceKind
  | -- Allows searching for type sigs with type variables
    TypeVarToken VarId OccurrenceKind
  | -- Allows searching by component hash
    -- Note: not actually a _short_ hash, it's a full hash with the referent info tagged
    -- on.
    HashToken ShortHash
  | TermTagToken TermTag
  | TypeTagToken TypeTag
  | TypeModToken DD.Modifier
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data DefinitionDocument name typeRef = DefinitionDocument
  { rootBranchHashId :: BranchHashId,
    fqn :: Name,
    hash :: ShortHash,
    -- For now we only index types by their final name segment, may need to revisit this
    -- in the future.
    tokens :: Set (DefnSearchToken typeRef),
    arity :: Arity,
    tag :: TermOrTypeTag,
    metadata :: TermOrTypeSummary
  }
  deriving (Show, Generic)

data SearchDefinition = SearchDefinition
  { fqn :: Name,
    hash :: ShortHash
  }
  deriving (Show)
