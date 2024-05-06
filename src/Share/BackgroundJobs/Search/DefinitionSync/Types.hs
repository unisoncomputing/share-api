{-# LANGUAGE DataKinds #-}

module Share.BackgroundJobs.Search.DefinitionSync.Types
  ( TermOrTypeSummary (..),
    DefinitionDocument (..),
  )
where

import Data.Aeson
import Data.Text qualified as Text
import Share.IDs (PrefixedHash (..), ProjectShortHand, ReleaseVersion)
import Share.IDs qualified as IDs
import Share.Prelude
import U.Codebase.HashTags (ComponentHash)
import Unison.Hash qualified as Hash
import Unison.Name (Name)
import Unison.Server.Share.DefinitionSummary (TermSummary, TypeSummary)
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.Name qualified as Name

data TermOrTypeSummary = TermSummary TermSummary | TypeSummary TypeSummary
  deriving (Show)

instance ToJSON TermOrTypeSummary where
  toJSON (TermSummary ts) = object ["kind" .= ("term" :: Text), "payload" .= ts]
  toJSON (TypeSummary ts) = object ["kind" .= ("type" :: Text), "payload" .= ts]

instance FromJSON TermOrTypeSummary where
  parseJSON = withObject "TermOrTypeSummary" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "term" -> TermSummary <$> o .: "payload"
      "type" -> TypeSummary <$> o .: "payload"
      _ -> fail $ "Invalid kind: " <> Text.unpack kind

-- | The number of occurences of this token in the search query.
-- E.g. for the query: 'Text -> Text -> Text', the Text type mention token would
-- occur 3 times, and the set would be:
-- {NameMention "Text" (Occurrence 1), NameMention "Text" (Occurrence 2), NameMention "Text" (Occurrence 3)}
newtype Occurrence = Occurrence Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON)

-- | An id for identifying unique type variables mentioned in a query.
-- E.g. 'map : (a -> b) -> List a -> List b' would have two type var Ids, one for a, one
-- for b, and would have occurrences 1 and 2 for each respectively.
newtype VarId = VarId Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToJSON)

data DefnToken
  = -- Allows searching by literal name
    NameToken Name
  | -- Also includes ability mentions
    NameMentionToken Name Occurrence
  | -- Allows searching for type sigs with type variables
    TypeVarToken VarId Occurrence
  | -- Allows searching by component hash
    HashToken ComponentHash
  deriving (Show, Eq, Ord)

-- | Converts a DefnToken to a prefix-searchable text string.
--
-- >>> tokenToText (NameToken (Name.unsafeParseText "List.map"))
-- "List.map:name"
--
-- >>> tokenToText (NameMentionToken (Name.unsafeParseText "List.map") (Occurrence 1))
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
tokenToText :: DefnToken -> Text
tokenToText = \case
  (NameToken n) -> Text.intercalate ":" [Name.toText n, "name"]
  (NameMentionToken n o) -> Text.intercalate ":" [Name.toText n, "mention", tShow o]
  (TypeVarToken v o) -> Text.intercalate ":" ["_", "var", tShow v, tShow o]
  (HashToken h) -> Text.intercalate ":" [into @Text $ PrefixedHash @"#" h, "hash"]

tokenFromText :: Text -> Maybe DefnToken
tokenFromText t = case Text.splitOn ":" t of
  [name, "name"] -> NameToken <$> Name.parseText name
  [name, "mention", occ] -> NameMentionToken <$> (Name.parseText name) <*> readMaybe (Text.unpack occ)
  [_, "var", vid, occ] -> TypeVarToken <$> readMaybe (Text.unpack vid) <*> readMaybe (Text.unpack occ)
  [prefixedHash, "hash"] ->
    case Text.stripPrefix "#" prefixedHash of
      Just hash -> HashToken . into @ComponentHash <$> Hash.fromBase32HexText hash
      Nothing -> Nothing
  _ -> Nothing

data DefinitionDocument = DefinitionDocument
  { projectShortHand :: ProjectShortHand,
    releaseVersion :: ReleaseVersion,
    fqn :: Name,
    hash :: ShortHash,
    tokens :: Set DefnToken,
    payload :: TermOrTypeSummary
  }
  deriving (Show)

instance ToJSON DefnToken where
  toJSON = String . tokenToText

instance FromJSON DefnToken where
  parseJSON = withText "DefnToken" $ \t ->
    maybe (fail $ "Invalid DefnToken: " <> Text.unpack t) pure $ tokenFromText t

-- | Formats a DefinitionDocument into a documentName
--
-- >>> projectShortHand = IDs.ProjectShortHand "unison" "base"
-- >>> releaseVersion = IDs.ReleaseVersion 1 2 3
-- >>> fqn = Name.unsafeFromText "data.List.map"
-- >>> hash = ShortHash "abcdef"
-- >>> formatDocName DefinitionDocument {projectShortHand, releaseVersion, fqn, hash, tokens = mempty, payload = undefined}
formatDocName :: DefinitionDocument -> Text
formatDocName DefinitionDocument {projectShortHand, fqn, hash} =
  Text.unwords [IDs.toText projectShortHand, Name.toText fqn, SH.toText hash]

instance ToJSON DefinitionDocument where
  toJSON dd@DefinitionDocument {releaseVersion, tokens, payload} =
    object
      [ "documentName" .= formatDocName dd,
        "releaseVersion" .= IDs.toText releaseVersion,
        "tokens" .= tokens,
        "metadata" .= payload
      ]

-- [{ "documentName": "@unison/base data.List.map #abcdef"
-- , "releaseVersion": "1.0.0"
-- , "tokens": ["Remote:1", "Optional:1", "map:name", "List.map:name", "data.List.map:name"]
-- , "metadata": {
--     "project": "base"
--   , "branchRef": "releases/1.2.3"
--   , "definitionKind": "data|ability|term (or constructors)"
--   , "definitionName": "data.Optional.map"
--   , "definitionTypeSignature": "(a -> b) -> Optional a -> Optional b"
--   }
-- }]
