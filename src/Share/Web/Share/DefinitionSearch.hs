{-# LANGUAGE DataKinds #-}

module Share.Web.DefinitionSearch (queryToTokens) where

import Control.Lens
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonMap
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Share.BackgroundJobs.Search.DefinitionSync.Types (DefnSearchToken (..), Occurrence, VarId (..))
import Share.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP hiding (space)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment

data QueryError
  = InvalidHash Text
  | InvalidName Text
  deriving stock (Show, Eq, Ord)

instance MP.ShowErrorComponent QueryError where
  showErrorComponent = \case
    InvalidHash hash -> "Encountered invalid hash: " <> Text.unpack hash
    InvalidName name -> "Encountered Invalid name: " <> Text.unpack name

data MentionRef
  = HashMention ShortHash
  | NameMention Name
  | TypeNameMention Name
  | TypeVarMention Text
  deriving stock (Show, Eq, Ord)

type P = MP.Parsec QueryError Text

-- | A very lax parser which converts a query into structured tokens for searching definitions.
--
-- A query may look like:
--
-- Simple definition name query:
-- E.g. foldMap
--
-- Type signature query
-- E.g.:
-- k -> v -> Map k v -> Map k v
--
-- Ad-hoc query:
--
-- Nat Text Abort
--
-- Hash-query
--
-- #abc1234
--
-- >>> queryToTokens "foldMap"
-- Right (fromList [NameToken (Name Relative (NameSegment {toUnescapedText = "foldMap"} :| []))],Nothing)
--
-- >>> queryToTokens "#abc1234"
-- Right (fromList [HashToken (ShortHash {prefix = "abc1234", cycle = Nothing, cid = Nothing})],Nothing)
--
-- >>> queryToTokens "##Nat"
-- Right (fromList [HashToken (Builtin "Nat")],Nothing)
--
-- >>> queryToTokens "Nat Text #deadbeef Abort"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "Abort"})) (Just 1),TypeMentionToken (Left (NameSegment {toUnescapedText = "Nat"})) (Just 1),TypeMentionToken (Left (NameSegment {toUnescapedText = "Text"})) (Just 1),HashToken (ShortHash {prefix = "deadbeef", cycle = Nothing, cid = Nothing})],Nothing)
--
-- >>> queryToTokens "k -> v -> Map k v -> Map k v"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "Map"})) (Just 2),TypeVarToken 0 (Just 3),TypeVarToken 1 (Just 3)],Just 3)
--
-- >>> queryToTokens ": b -> a -> b"
-- Right (fromList [TypeVarToken 0 (Just 1),TypeVarToken 1 (Just 2)],Just 2)
--
-- >>> queryToTokens "(a ->{𝕖} b) -> [a] ->{𝕖} [b]"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "List"})) (Just 2),TypeVarToken 0 (Just 2),TypeVarToken 1 (Just 2)],Just 2)
--
-- >>> queryToTokens "'{Abort} ()"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "Abort"})) (Just 1)],Nothing)
--
-- Unfinished query:
-- >>> queryToTokens "(Text -> Text"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "Text"})) (Just 2)],Nothing)
--
-- Horribly mishapen query:
-- >>> queryToTokens "[{ &Text !{𝕖} (Optional)"
-- Right (fromList [TypeMentionToken (Left (NameSegment {toUnescapedText = "Optional"})) (Just 1),TypeMentionToken (Left (NameSegment {toUnescapedText = "Text"})) (Just 1)],Nothing)
queryToTokens :: Text -> Either Text (Set (DefnSearchToken (Either NameSegment ShortHash)), Maybe Int)
queryToTokens query =
  let cleanQuery =
        query
          & Text.filter Char.isAscii
   in MP.runParser queryParser "query" cleanQuery
        & \case
          (Left _err) ->
            let simpleQuery =
                  query
                    & Text.map (\c -> if Char.isAlphaNum c || c `elem` ("#." :: String) then c else ' ')
                    & Text.words
                    & Text.unwords
             in -- If even the lax parser fails, try simplifying the query even further to see if
                -- we can parse anything at all.
                queryToTokens simpleQuery
          (Right (mayArity, occurrences)) ->
            let (hashAndNameTokens, typeVarMentions) =
                  MonMap.toList
                    occurrences
                    & foldMap \case
                      (HashMention hash, _occ) -> ([HashToken hash], [])
                      (NameMention name, _occ) -> ([NameToken name], [])
                      (TypeNameMention name, occ) -> ([TypeMentionToken (Left $ Name.lastSegment name) $ Just occ], [])
                      (TypeVarMention var, occ) -> ([], [(var, occ)])

                -- Normalize type vars so varIds are sorted according to number of occurences.
                normalizedTypeVarTokens =
                  List.sortOn snd typeVarMentions
                    & imap (\i (_vId, occ) -> TypeVarToken (VarId i) $ Just occ)
                -- if there's no indication the user is trying to do a 'real' type query then
                -- ignore arity.
                arity = do
                  Sum n <- mayArity
                  if n <= 0
                    then Nothing
                    else Just n
             in Right (Set.fromList $ hashAndNameTokens <> normalizedTypeVarTokens, arity)

queryParser :: P (Maybe (Sum Int), MonoidalMap MentionRef Occurrence)
queryParser = do
  MP.choice
    [ (Nothing,) <$> MP.try simpleHashQueryP,
      (Nothing,) <$> MP.try simpleNameQueryP,
      first Just <$> typeQueryP
    ]
    <* MP.eof

lexeme :: P a -> P a
lexeme = MP.lexeme MP.space

simpleHashQueryP :: P (MonoidalMap MentionRef Occurrence)
simpleHashQueryP = do
  possibleHash <- lexeme hashP
  -- Simple queries have ONLY the hash
  MP.eof
  pure $ MonMap.singleton (HashMention possibleHash) 1

simpleNameQueryP :: P (MonoidalMap MentionRef Occurrence)
simpleNameQueryP = do
  name <- nameP False
  -- Simple queries have ONLY the name
  MP.eof
  pure $ MonMap.singleton (NameMention name) 1

-- | Parse a type query, returning the arity of the top-level type
typeQueryP :: P (Sum Int, MonoidalMap MentionRef Occurrence)
typeQueryP = do
  _ <- optional $ lexeme (MP.char ':')
  fmap fold . many $ do
    tokens <-
      lexeme $
        MP.choice
          [ typeQueryTokenP,
            listP,
            MP.try unitP,
            MP.try tupleP,
            -- We do anything smart with bracketed types yet.
            MP.between (lexeme (MP.char '(')) (optional $ lexeme (MP.char ')')) (snd <$> typeQueryP),
            -- Remove type var mentions from ability lists, we don't consider them when building
            -- the index so they just wreck search results.
            removeTypeVarMentions <$> MP.between (lexeme (MP.char '{')) (optional $ lexeme (MP.char '}')) (foldMap snd <$> MP.sepBy typeQueryP (lexeme $ MP.char ','))
          ]
    arityBump <-
      optional (lexeme (MP.string "->"))
        <&> \case
          Nothing -> Sum 0
          Just _ -> Sum 1
    pure (arityBump, tokens)
  where
    removeTypeVarMentions :: MonoidalMap MentionRef Occurrence -> MonoidalMap MentionRef Occurrence
    removeTypeVarMentions = MonMap.filterWithKey \k _v -> case k of
      TypeVarMention _ -> False
      _ -> True

-- We just ignore units for now, they don't contribute much to the search.
unitP :: P (MonoidalMap MentionRef Occurrence)
unitP = MP.choice [MP.string "()", MP.string "Unit", MP.string "'"] $> mempty

tupleP :: P (MonoidalMap MentionRef Occurrence)
tupleP = MP.between (MP.char '(') (MP.char ')') do
  typeQueryP
  _ <- MP.char ','
  typeQueryP
  pure $ MonMap.singleton (TypeNameMention (Name.unsafeParseText "Tuple")) 1

listP :: P (MonoidalMap MentionRef Occurrence)
listP = MP.between (lexeme (MP.char '[')) (optional $ lexeme (MP.char ']')) do
  (_, tokens) <- typeQueryP
  pure $ tokens <> MonMap.singleton (TypeNameMention (Name.unsafeParseText "List")) 1

typeQueryTokenP :: P (MonoidalMap MentionRef Occurrence)
typeQueryTokenP = do
  MP.choice
    [ hashMentionTokenP,
      typeVarMentionTokenP,
      typeNameMentionTokenP
    ]
  where
    hashMentionTokenP :: P (MonoidalMap MentionRef Occurrence)
    hashMentionTokenP = do
      hash <- hashP
      pure $ MonMap.singleton (HashMention hash) 1

    typeNameMentionTokenP :: P (MonoidalMap MentionRef Occurrence)
    typeNameMentionTokenP = do
      name <- nameP True
      pure $ MonMap.singleton (TypeNameMention name) 1

    typeVarMentionTokenP :: P (MonoidalMap MentionRef Occurrence)
    typeVarMentionTokenP = do
      varText <- typeVarP
      pure $ MonMap.singleton (TypeVarMention varText) 1

typeVarP :: P Text
typeVarP = Text.pack <$> liftA2 (:) (MP.oneOf $ ['a' .. 'z'] <> "_") (many $ MP.alphaNumChar <|> MP.char '_')

hashP :: P ShortHash
hashP = do
  -- Start with at least one hash;
  _ <- MP.char '#'
  possibleHash <- ('#' :) <$> (some $ MP.alphaNumChar <|> MP.char '#')
  case SH.fromText (Text.pack possibleHash) of
    Nothing -> MP.customFailure . InvalidHash $ Text.pack possibleHash
    Just hash -> pure hash

nameP :: Bool -> P Name
nameP mustBeType = do
  firstChar <-
    if mustBeType
      then MP.satisfy Char.isUpper
      else MP.satisfy (\c -> NameSegment.symbolyIdChar c || NameSegment.wordyIdChar c)
  nameRemainder <-
    if mustBeType
      then many (MP.satisfy $ \c -> NameSegment.wordyIdChar c || c == '.')
      else (many (MP.satisfy $ \c -> NameSegment.symbolyIdChar c || NameSegment.wordyIdChar c || c == '.'))
  case Name.parseTextEither (Text.pack $ firstChar : nameRemainder) of
    Left _ -> MP.customFailure . InvalidName $ Text.pack (firstChar : nameRemainder)
    Right name -> pure name
