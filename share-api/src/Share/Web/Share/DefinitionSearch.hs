{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}

-- | This module provides a lax parser for queries that are used to search for definitions.
--
-- It supports search-by-type queries, search-by-name queries, and search-by-hash queries.
--
-- The parser is deliberately lax so users can type basically whatever they want into search
-- and still get _some_ result.
module Share.Web.Share.DefinitionSearch (queryToTokens) where

import Control.Lens
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonMap
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Share.BackgroundJobs.Search.DefinitionSync.Types (Arity, DefnSearchToken (..), Occurrence (..), OccurrenceKind (..), VarId (..))
import Share.Prelude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP hiding (space)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Monoid qualified as Monoid

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
  | TypeHashMention ShortHash
  deriving stock (Show, Eq, Ord)

type Tokens = MonoidalMap MentionRef Occurrence

type ReturnTokens = Set MentionRef

type P = MP.Parsec QueryError Text

-- | A very lax parser which converts a query into structured tokens for searching definitions.
--
-- If we can't parse something that looks like a name search, hash search, or type search,
-- we fall back to stripping out all special characters and trying to just tokenize type
-- mentions from whatever is left.
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
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Abort"} :| []))) (Count 1),TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Nat"} :| []))) (Count 1),TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) (Count 1),TypeMentionToken (Right (ShortHash {prefix = "deadbeef", cycle = Nothing, cid = Nothing})) (Count 1)],Nothing)
--
-- >>> queryToTokens "k -> v -> Map k v -> Map k v"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Map"} :| []))) (Count 2),TypeVarToken 0 (Count 3),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 3)],Just 3)
--
-- >>> queryToTokens ": b -> a -> b"
-- Right (fromList [TypeVarToken 0 (Count 1),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 2)],Just 2)
--
-- >>> queryToTokens "(a ->{𝕖} b) -> [a] ->{𝕖} [b]"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "List"} :| []))) ReturnPosition,TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "List"} :| []))) (Count 2),TypeVarToken 0 (Count 2),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 2)],Just 2)
--
-- >>> queryToTokens "'{Abort} ()"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Abort"} :| []))) (Count 1)],Nothing)
--
-- Unfinished query:
-- >>> queryToTokens "(Text -> Text"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) (Count 2)],Nothing)
--
-- Horribly misshapen query:
-- >>> queryToTokens "[{ &Text !{𝕖} (Optional)"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Optional"} :| []))) (Count 1),TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) (Count 1)],Nothing)
--
-- >>> queryToTokens "e -> abilities.Exception"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Exception"} :| [NameSegment {toUnescapedText = "abilities"}]))) ReturnPosition,TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Exception"} :| [NameSegment {toUnescapedText = "abilities"}]))) (Count 1),TypeVarToken 0 (Count 1)],Just 1)
--
-- >>> queryToTokens "Json.Text"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| [NameSegment {toUnescapedText = "Json"}]))) (Count 1)],Nothing)
--
-- >>> queryToTokens "Nat -> Text"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Nat"} :| []))) (Count 1),TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) ReturnPosition,TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) (Count 1)],Just 1)
--
-- >>> queryToTokens "(a, b) -> (a -> c) -> (c, b)"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Tuple"} :| []))) ReturnPosition,TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Tuple"} :| []))) (Count 2),TypeVarToken 0 (Count 2),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 2),TypeVarToken 2 ReturnPosition,TypeVarToken 2 (Count 2)],Just 2)
--
-- >>> queryToTokens "Text -> ()"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Text"} :| []))) (Count 1)],Just 1)
--
-- >>> queryToTokens "(a,b) -> a "
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Tuple"} :| []))) (Count 1),TypeVarToken 0 (Count 1),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 2)],Just 1)
--
-- >>> queryToTokens "Tuple a b -> a"
-- Right (fromList [TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Tuple"} :| []))) (Count 1),TypeVarToken 0 (Count 1),TypeVarToken 1 ReturnPosition,TypeVarToken 1 (Count 2)],Just 1)
--
-- Allow a name search in the middle of a type query.
-- Any name starting in lowercase that's not just a single letter is a name filter.
-- >>> queryToTokens "Nat -> Nat plus"
-- Right (fromList [NameToken (Name Relative (NameSegment {toUnescapedText = "plus"} :| [])),TypeMentionToken (Left (Name Relative (NameSegment {toUnescapedText = "Nat"} :| []))) (Count 2)],Just 1)
queryToTokens :: Text -> Either Text (Set (DefnSearchToken (Either Name ShortHash)), Maybe Arity)
queryToTokens query =
  let cleanQuery =
        -- Queries may contain unicode chars, but they're usually not useful for search, e.g.
        -- "∀" or "𝕖" so we just filter them out. The parser is lax enough it should still
        -- manage to parse.
        query
          & Text.filter Char.isAscii
      parseResult =
        MP.runParser queryParser "query" cleanQuery
          & \case
            (Left _err) ->
              -- If the parser fails because a type is mis-formed, try to simplify the query
              -- further to see if we can parse anything at all.
              let simpleQuery =
                    cleanQuery
                      -- Keep only name chars, hash chars, and dots.
                      & Text.map (\c -> if Char.isAlphaNum c || c `elem` ("#." :: String) then c else ' ')
                      & Text.words
                      & Text.unwords
               in mapLeft (Text.pack . MP.errorBundlePretty) $ MP.runParser queryParser "query" simpleQuery
            (Right r) -> Right r
   in case parseResult of
        Left err -> Left err
        Right (mayArity, occurrences, returnMentions) ->
          let (hashAndNameTokens, typeVarMentions) =
                MonMap.toList
                  occurrences
                  & foldMap \case
                    (HashMention hash, _occ) -> ([HashToken hash], [])
                    (NameMention name, _occ) -> ([NameToken name], [])
                    (TypeNameMention name, occ) -> ([TypeMentionToken (Left name) (Count occ)], [])
                    (TypeVarMention var, occ) -> ([], [(var, Count occ)])
                    (TypeHashMention hash, occ) -> ([TypeMentionToken (Right hash) (Count occ)], [])
              (returnTokens, typeVarReturnTokens) =
                returnMentions
                  & foldMap \case
                    HashMention {} -> mempty
                    NameMention {} -> mempty
                    TypeNameMention name -> ([TypeMentionToken (Left name) ReturnPosition], mempty)
                    TypeVarMention var -> (mempty, Set.singleton var)
                    TypeHashMention hash -> ([TypeMentionToken (Right hash) ReturnPosition], mempty)

              -- Normalize type vars so varIds are sorted according to number of occurences.
              normalizedTypeVarTokens =
                List.sortOn snd typeVarMentions
                  & ifoldMap
                    ( \i (vId, occ) ->
                        [TypeVarToken (VarId i) occ]
                          -- Add a return token for type vars that are used in the return position.
                          <> Monoid.whenM (Set.member vId typeVarReturnTokens) [TypeVarToken (VarId i) ReturnPosition]
                    )
              -- if there's no indication the user is trying to do a 'real' type query then
              -- ignore arity.
              arity = do
                Sum n <- mayArity
                if n <= 0
                  then Nothing
                  else Just n
           in Right (Set.fromList $ hashAndNameTokens <> normalizedTypeVarTokens <> returnTokens, arity)

queryParser :: P (Maybe (Sum Arity), Tokens, ReturnTokens)
queryParser = do
  MP.choice
    [ MP.try simpleHashQueryP <&> \(tokens, rTokens) -> (Nothing, tokens, rTokens),
      typeQueryP <&> \(arity, tokens, rTokens) -> (Just arity, tokens, rTokens)
    ]
    <* MP.eof

lexeme :: P a -> P a
lexeme = MP.lexeme MP.space

simpleHashQueryP :: P (Tokens, ReturnTokens)
simpleHashQueryP = do
  possibleHash <- lexeme hashP
  -- Simple queries have ONLY the hash
  MP.eof
  pure $ (MonMap.singleton (HashMention possibleHash) 1, mempty)

-- | Parse a simple name query, e.g. 'foldMap'
simpleNameQueryP :: P (Tokens)
simpleNameQueryP = do
  name <- initialNameP
  pure $ (MonMap.singleton (NameMention name) 1)

-- | Parse a type query, returning the arity of the top-level type
typeQueryP :: P (Sum Arity, Tokens, ReturnTokens)
typeQueryP = do
  _ <- optional $ lexeme (MP.char ':')
  topLevelTypeSegments <- some $ do
    tokens <-
      lexeme $
        MP.choice
          [ MP.try typeQueryTokenP,
            MP.try simpleNameQueryP,
            listP,
            MP.try unitP,
            MP.try tupleP,
            -- We do anything smart with bracketed types yet.
            MP.between (lexeme (MP.char '(')) (optional $ lexeme (MP.char ')')) (view _2 <$> typeQueryP),
            -- Remove type var mentions from ability lists, we don't consider them when building
            -- the index so they just wreck search results.
            removeTypeVarMentions <$> MP.between (lexeme (MP.char '{')) (optional $ lexeme (MP.char '}')) (foldMap (view _2) <$> MP.sepBy typeQueryP (lexeme $ MP.char ','))
          ]
    arityBump <-
      optional (lexeme (MP.string "->"))
        <&> \case
          Nothing -> Sum 0
          Just _ -> Sum 1
    pure (arityBump, tokens)
  let (arity, foldedTokens) = fold topLevelTypeSegments
  -- If we actually have some semblence of arguments, we can add return annotations to
  -- mentions.
  pure $
    -- If we've got at least one well-formed argument, then assume that the last segment is
    -- meant to be the return type.
    if arity >= 1
      then (arity, foldedTokens, foldOf (_last . _2 . to MonMap.keysSet) topLevelTypeSegments)
      else (arity, foldedTokens, mempty)
  where
    removeTypeVarMentions :: Tokens -> Tokens
    removeTypeVarMentions = MonMap.filterWithKey \k _v -> case k of
      TypeVarMention _ -> False
      _ -> True

-- We just ignore units for now, they don't contribute much to the search.
unitP :: P Tokens
unitP = lexeme $ MP.choice [MP.string "()", MP.string "Unit", MP.string "'"] $> mempty

tupleP :: P Tokens
tupleP = MP.between (MP.char '(') (MP.char ')') do
  (_, before, _) <- typeQueryP
  _ <- lexeme (MP.char ',')
  (_, after, _) <- typeQueryP
  let tupleTokens = MonMap.singleton (TypeNameMention (Name.unsafeParseText "Tuple")) 1
  pure $ before <> tupleTokens <> after

listP :: P Tokens
listP = MP.between (lexeme (MP.char '[')) (lexeme (MP.char ']')) do
  (_, tokens, _rTokens) <- typeQueryP
  pure $ tokens <> MonMap.singleton (TypeNameMention (Name.unsafeParseText "List")) 1

typeQueryTokenP :: P Tokens
typeQueryTokenP = do
  MP.choice
    [ hashMentionTokenP,
      typeMentionP
    ]
  where
    hashMentionTokenP :: P Tokens
    hashMentionTokenP = do
      hash <- hashP
      pure $ MonMap.singleton (TypeHashMention hash) 1

typeMentionP :: P Tokens
typeMentionP = do
  name <- nameP
  if
    | -- Type var must be just a single letter
      [c] <- Text.unpack . Name.toText $ name,
      Char.isLower c ->
        pure $ MonMap.singleton (TypeVarMention (Name.toText name)) 1
    | Just (c, _) <- Text.uncons $ NameSegment.toEscapedText (Name.lastSegment name),
      Char.isUpper c ->
        pure $ MonMap.singleton (TypeNameMention name) 1
    | otherwise -> fail "Invalid type mention"

hashP :: P ShortHash
hashP = do
  -- Start with at least one hash;
  _ <- MP.char '#'
  possibleHash <- ('#' :) <$> (some $ MP.alphaNumChar <|> MP.char '#')
  case SH.fromText (Text.pack possibleHash) of
    Nothing -> MP.customFailure . InvalidHash $ Text.pack possibleHash
    Just hash -> pure hash

nameP :: P Name
nameP = do
  name <- List.intercalate "." <$> MP.sepBy (liftA2 (:) (MP.satisfy NameSegment.wordyIdStartChar) (many (MP.satisfy NameSegment.wordyIdChar))) (MP.char '.')
  case Name.parseTextEither (Text.pack name) of
    Left _ -> MP.customFailure . InvalidName $ Text.pack name
    Right name -> pure name

initialNameP :: P Name
initialNameP = do
  name <- List.intercalate "." <$> MP.sepBy (some (MP.satisfy NameSegment.symbolyIdChar) <|> (liftA2 (:) (MP.satisfy NameSegment.wordyIdStartChar) (many (MP.satisfy NameSegment.wordyIdChar)))) (MP.char '.')
  case Name.parseTextEither (Text.pack name) of
    Left _ -> MP.customFailure . InvalidName $ Text.pack name
    Right name -> pure name
