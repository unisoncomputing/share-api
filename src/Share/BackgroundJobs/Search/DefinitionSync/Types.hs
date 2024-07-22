{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Share.BackgroundJobs.Search.DefinitionSync.Types
  ( TermOrTypeSummary (..),
    DefinitionDocument (..),
    DefnSearchToken (..),
    Occurrence (..),
    VarId (..),
  )
where

import Data.Aeson
import Data.Monoid (Sum (..))
import Data.Text qualified as Text
import Share.Prelude
import Unison.DataDeclaration qualified as DD
import Unison.Name (Name)
import Unison.Server.Share.DefinitionSummary.Types (TermSummary (..), TypeSummary (..))
import Unison.Server.Types (TermTag, TypeTag)
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

-- | The number of occurences of this token in the search query.
-- E.g. for the query: 'Text -> Text -> Text', the Text type mention token would
-- occur 3 times, and the set would be:
-- {NameMention "Text" (Occurrence 1), NameMention "Text" (Occurrence 2), NameMention "Text" (Occurrence 3)}
newtype Occurrence = Occurrence Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON, Enum)
  deriving (Semigroup) via Sum Int
  deriving (Monoid) via Sum Int

-- | An id for identifying unique type variables mentioned in a query.
-- E.g. 'map : (a -> b) -> List a -> List b' would have two type var Ids, one for a, one
-- for b, and would have occurrences 1 and 2 for each respectively.
newtype VarId = VarId Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON, Enum)

-- | Represents the possible ways we can search the global definitions index.
data DefnSearchToken typeRef
  = -- Allows searching by literal name
    NameToken Name
  | -- A mention of some external type or ability
    TypeMentionToken typeRef (Maybe Occurrence {- Nothing means it's a return value -})
  | -- Allows searching for type sigs with type variables
    TypeVarToken VarId (Maybe Occurrence {- Nothing means it's a return value -})
  | -- Allows searching by component hash
    -- Note: not actually a _short_ hash, it's a full hash with the referent info tagged
    -- on.
    HashToken ShortHash
  | TermTagToken TermTag
  | TypeTagToken TypeTag
  | TypeModToken DD.Modifier
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Converts a DefnSearchToken to a prefix-searchable text string.
--
-- >>> tokenToText (NameToken (Name.unsafeParseText "List.map"))
-- "List.map:name"
--
-- >>> tokenToText (TypeMentionToken (Name.unsafeParseText "List.map") (Occurrence 1))
-- "List.map:mention:1"
--
-- >>> tokenToText (TypeVarToken (VarId 1) (Occurrence 1))
-- "_:var:1:1"
--
-- >>> import Unison.Hash qualified as Hash
-- >>> import U.Codebase.HashTags (ComponentHash (..))
-- >>> hash = ComponentHash $ Hash.unsafeFromBase32HexText "abcd"
-- >>> tokenToText (HashToken hash)
-- "#abc0:hash"
-- tokenToText :: DefnSearchToken Name -> Text
-- tokenToText = \case
--   (NameToken n) -> Text.intercalate ":" [Name.toText n, "name"]
--   (TypeMentionToken n o) -> Text.intercalate ":" [Name.toText n, "mention", tShow o]
--   (TypeVarToken v o) -> Text.intercalate ":" ["_", "var", tShow v, tShow o]
--   (HashToken h) -> Text.intercalate ":" [into @Text $ PrefixedHash @"#" h, "hash"]

-- tokenFromText :: Text -> Maybe (DefnSearchToken Name)
-- tokenFromText t = case Text.splitOn ":" t of
--   [name, "name"] -> NameToken <$> Name.parseText name
--   [name, "mention", occ] -> TypeMentionToken <$> (Name.parseText name) <*> readMaybe (Text.unpack occ)
--   [_, "var", vid, occ] -> TypeVarToken <$> readMaybe (Text.unpack vid) <*> readMaybe (Text.unpack occ)
--   [prefixedHash, "hash"] ->
--     case Text.stripPrefix "#" prefixedHash of
--       Just hash -> HashToken . into @ComponentHash <$> Hash.fromBase32HexText hash
--       Nothing -> Nothing
--   _ -> Nothing

data DefinitionDocument proj release name typeRef = DefinitionDocument
  { project :: proj,
    release :: release,
    fqn :: Name,
    hash :: ShortHash,
    -- For now we only index types by their final name segment, may need to revisit this
    -- in the future.
    tokens :: Set (DefnSearchToken typeRef),
    arity :: Int,
    metadata :: TermOrTypeSummary
  }
  deriving (Show, Generic)

data SearchDefinition = SearchDefinition
  { fqn :: Name,
    hash :: ShortHash
  }
  deriving (Show)
