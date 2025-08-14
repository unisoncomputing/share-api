{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | Contains generic helpers for REST-style APIs
module Share.Utils.API
  ( NullableUpdate (..),
    SetUpdate (..),
    AddOrRemove (..),
    fromNullableUpdate,
    applySetUpdate,
    parseNullableUpdate,
    emptySetUpdate,
    Cursor (..),
    Paged (..),
    guardPaged,
    Query (..),
    Limit (..),
    (:++) (..),
    AtKey (..),
    CursorDirection (..),
    pagedOn,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base64.URL.Lazy qualified as Base64URL
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String qualified as String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (Symbol)
import GHC.TypeLits qualified as TypeLits
import Hasql.Interpolate (EncodeValue)
import Safe (headMay, lastMay)
import Servant

-- | Type for updating of nullable fields.
--
-- This helps handle the tricky case of 'PATCH'-ing fields which are nullable, since there are
-- 3 possible things the client might want to do:
--
-- 1. Set the field to a new value
-- 2. Set the field to 'Nothing'
-- 3. Leave the field unchanged
--
-- To encode this in JSON, we use the following encoding:
--
-- If a json field is NOT provided, it means to leave the field as-is.
-- If explicitly set to 'null', the field should be updated to be Nothing.
-- If set to a value, perform that update.
data NullableUpdate a
  = Unchanged
  | Nullify
  | UpdateTo a
  deriving (Show, Eq, Ord)

-- | Parse a nullable update for a specific field on an Aeson object.
--
-- Usage:
--
-- @
-- parseJSON = withObject "Foo" $ \obj -> do
--   parseNullableUpdate obj "key"
-- @
parseNullableUpdate :: forall a. (FromJSON a) => Aeson.Object -> Text -> Parser (NullableUpdate a)
parseNullableUpdate obj key =
  obj .:! Aeson.Key.fromText key >>= \case
    Nothing -> pure Unchanged
    Just Aeson.Null -> pure Nullify
    Just v -> UpdateTo <$> parseJSON v

-- | Perform a nullable update to an existing value and return the result.
--
-- >>> fromNullableUpdate (Just 1) Unchanged
-- Just 1
--
-- >>> fromNullableUpdate (Just 1) Nullify
-- Nothing
--
-- >>> fromNullableUpdate (Just 1) (UpdateTo 2)
-- Just 2
fromNullableUpdate :: Maybe a -> NullableUpdate a -> Maybe a
fromNullableUpdate fallback = \case
  Unchanged -> fallback
  Nullify -> Nothing
  UpdateTo a -> Just a

-- | Type for updating of fields which contain sets of values (e.g. tags).
--
-- The value can either be _replaced_ with a new set of values, or explicitly specify that specific
-- tags should be added or removed.
--
-- Valid json formats are:
--
-- Updates, the same value must not appear in both "add" and "remove".
-- @
-- { "myField": { "add": ["foo", "bar"], "remove": ["baz"] } }
-- }
-- @
--
-- Replacement:
-- @
-- { "myField": { "replaceWith": ["foo", "bar"] } }
-- @
--
-- >>> Aeson.eitherDecode "{ \"add\": [\"foo\", \"bar\"], \"remove\": [\"baz\"]}" :: Either String (SetUpdate Text)
-- Right (SetUpdate (fromList [("bar",Add),("baz",Remove),("foo",Add)]))
--
-- >>> Aeson.eitherDecode "{ \"replaceWith\": [\"foo\", \"bar\"]}" :: Either String (SetUpdate Text)
-- Right (SetReplacement (fromList ["bar","foo"]))
data SetUpdate a
  = SetUpdate (Map a AddOrRemove)
  | SetReplacement (Set a)
  deriving (Show)

-- | Type for specifying whether a value should be added or removed from a set.
data AddOrRemove = Add | Remove
  deriving (Show)

emptySetUpdate :: SetUpdate a
emptySetUpdate = SetUpdate Map.empty

instance forall a. (FromJSON a, Ord a, Typeable a) => FromJSON (SetUpdate a) where
  parseJSON = Aeson.withObject ("(SetUpdate" <> show (typeRep (Proxy @a)) <> ")") $ \obj -> do
    parseReplacement obj <|> parseAddOrRemove obj
    where
      parseReplacement :: Aeson.Object -> Parser (SetUpdate a)
      parseReplacement obj = do
        replacement <- obj .: "replaceWith"
        pure (SetReplacement (Set.fromList replacement))
      parseAddOrRemove :: Aeson.Object -> Parser (SetUpdate a)
      parseAddOrRemove obj = do
        add <- obj .:? "add"
        remove <- obj .:? "remove"
        when (isNothing add && isNothing remove) $ fail "Either ('add' and/or 'remove') or 'replaceWith' must be specified"
        let adds = Map.fromList (fromMaybe [] add <&> (,Add))
        let removals = Map.fromList (fromMaybe [] remove <&> (,Remove))
        when (not (Map.null (adds `Map.intersection` removals))) $
          fail "Cannot add and remove the same value"
        pure (SetUpdate (adds <> removals))

-- | Apply a 'SetUpdate' to an existing set of values.
--
-- >>> applySetUpdate (Set.fromList ["foo", "bar"]) (SetUpdate (Map.fromList [("baz", Add), ("bar", Remove)]))
-- fromList ["baz","foo"]
--
-- >>> applySetUpdate (Set.fromList ["foo", "bar"]) (SetReplacement (Set.fromList ["baz"]))
-- fromList ["baz"]
applySetUpdate :: (Ord a) => Set a -> SetUpdate a -> Set a
applySetUpdate existing = \case
  SetUpdate updates ->
    let go acc (a, Add) = Set.insert a acc
        go acc (a, Remove) = Set.delete a acc
     in Foldable.foldl' go existing (Map.toList updates)
  SetReplacement new -> new

data CursorDirection = Previous | Next
  deriving (Eq, Ord, Show)

-- | A cursor for a pageable endpoint.
-- This is rendered to an opaque base64URL string and included in paged responses,
-- if provided back to the endpoint it will be used to determine the starting
-- point of the next page.
data Cursor a = Cursor {location :: a, direction :: CursorDirection}
  deriving stock (Eq, Ord, Show)

-- |
-- >>> toUrlPiece (Cursor (1 :: Int, "hello" :: String) Next)
-- "n.WzEsImhlbGxvIl0"
--
-- >>> parseUrlPiece (toUrlPiece (Cursor (1 :: Int, "hello" :: String) Next)) :: Either Text (Cursor (Int, String))
-- Right (Cursor {location = (1,"hello"), direction = Next})
instance (ToJSON a) => ToHttpApiData (Cursor a) where
  toUrlPiece (Cursor {location, direction}) =
    let loc = TL.decodeUtf8 . Base64URL.encodeUnpadded . Aeson.encode $ location
        dir :: TL.Text
        dir = case direction of
          Previous -> "p"
          Next -> "n"
     in toUrlPiece $ dir <> "." <> loc

instance (FromJSON a) => FromHttpApiData (Cursor a) where
  parseUrlPiece txt = do
    let (dirTxt, locTxt) = second (Text.drop 1) $ Text.breakOn "." txt
    dir <- case dirTxt of
      "p" -> pure Previous
      "n" -> pure Next
      _ -> Left $ "Invalid or missing cursor direction: " <> dirTxt
    jsonBytes <- first Text.pack . Base64URL.decodeUnpadded . TL.encodeUtf8 . TL.fromStrict $ locTxt
    loc <- first Text.pack (Aeson.eitherDecode jsonBytes)
    pure $ Cursor loc dir

-- |
-- >>> toJSON (Cursor (1 :: Int, "hello" :: String) Next)
-- String "n.WzEsImhlbGxvIl0"
--
-- >>> toJSON (Cursor (1 :: Int, "hello" :: String) Previous)
-- String "p.WzEsImhlbGxvIl0"
--
-- >>> decode (encode (Cursor (1 :: Int, "hello" :: String) Next)) :: Maybe (Cursor (Int, String))
-- Just (Cursor {location = (1,"hello"), direction = Next})
instance (ToJSON a) => ToJSON (Cursor a) where
  toJSON = toJSON . toUrlPiece

instance (FromJSON a) => FromJSON (Cursor a) where
  parseJSON = withText "Cursor" $ \txt ->
    either (fail . Text.unpack) pure $ parseUrlPiece txt

data Paged cursor a = Paged
  { items :: [a],
    prevCursor :: Maybe (Cursor cursor),
    nextCursor :: Maybe (Cursor cursor)
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (ToJSON a, ToJSON cursor) => ToJSON (Paged cursor a) where
  toJSON (Paged {items, prevCursor, nextCursor}) =
    object
      [ "items" .= items,
        "prevCursor" .= prevCursor,
        "nextCursor" .= nextCursor
      ]

guardPaged :: Bool -> Bool -> Paged cursor a -> Paged cursor a
guardPaged hasPrev hasNext paged@(Paged {prevCursor, nextCursor}) =
  paged
    { prevCursor = if hasPrev then prevCursor else Nothing,
      nextCursor = if hasNext then nextCursor else Nothing
    }

-- | The maximum page size for pageable endpoints
maxLimit :: Int64
maxLimit = 100

newtype Limit = Limit {getLimit :: Int64}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, ToHttpApiData, EncodeValue, Num)

instance FromHttpApiData Limit where
  parseUrlPiece txt = do
    n <- parseUrlPiece @Int64 txt
    case n of
      0 -> Left "limit must be positive"
      n | n > maxLimit -> Left $ "limit must be less than " <> Text.pack (show maxLimit)
      _ -> Right $ Limit n

instance FromJSON Limit where
  parseJSON = withScientific "limit" $ \n ->
    case round n of
      0 -> fail "limit must be positive"
      n | n > maxLimit -> fail $ "limit must be less than " <> show maxLimit
      _ -> pure $ Limit $ round n

newtype Query = Query Text
  deriving newtype (ToHttpApiData, FromHttpApiData)
  deriving stock (Show)

-- | Helper for constructing responses which consist of a standard base object but with
-- additional data fields. E.g. (APIProject :++ APIFavData :++ APIProjectOwner)
--
-- In cases of conflicting fields, the first value for that field wins, but it's recommended
-- that you ensure your field names are distinct amongst all inclusions.
--
-- >>> toJSON (Map.fromList [("foo", "bar")] :++ (Map.fromList [("baz", 33 :: Int)]))
-- Object (fromList [("baz",Number 33.0),("foo",String "bar")])
--
-- Ignores nulls, which allows providing 'optional' inclusions.
-- >>> toJSON (Map.fromList [("foo", "bar")] :++ (Nothing :: Maybe (Map String String)) :++ Map.fromList [("baz", 33 :: Int)])
-- Object (fromList [("baz",Number 33.0),("foo",String "bar")])
-- >>> toJSON ((Nothing :: Maybe (Map String String)) :++ Map.fromList [("foo", "bar")])
-- Object (fromList [("foo",String "bar")])
--
-- Can't merge non-objects.
-- >>> toJSON (["foo", "bar"] :++ (Map.fromList [("foo", 33 :: Int)]))
-- Cannot merge JSON representation of [[Char]] with Map [Char] Int
data a :++ b = a :++ b
  deriving stock (Eq, Ord, Show)

infixr 5 :++

instance (Typeable a, Typeable b, ToJSON a, ToJSON b) => ToJSON (a :++ b) where
  toJSON (a :++ b) =
    case (toJSON a, toJSON b) of
      (Object a', Object b') -> Object (a' <> b')
      (Object a', Null) -> Object a'
      (Null, Object b') -> Object b'
      -- If either is not an object, error, showing the type of the non-object value
      _ -> error $ "Cannot merge JSON representation of " <> show (typeRep (Proxy @a)) <> " with " <> show (typeRep (Proxy @b))

instance (Typeable a, Typeable b, FromJSON a, FromJSON b) => FromJSON (a :++ b) where
  parseJSON v = do
    a <- parseJSON v
    b <- parseJSON v
    pure $ a :++ b

-- | Wrapper useful in combination with `:++` to include the given payload at a specific key.
newtype AtKey (key :: Symbol) a = AtKey a
  deriving stock (Eq, Ord, Show)

instance (ToJSON a, TypeLits.KnownSymbol key) => ToJSON (AtKey key a) where
  toJSON (AtKey a) = object [String.fromString (TypeLits.symbolVal (Proxy @key)) .= a]

instance (FromJSON a, TypeLits.KnownSymbol key) => FromJSON (AtKey key a) where
  parseJSON = withObject "AtKey" $ \obj -> do
    (innerVal :: a) <- (obj .: String.fromString (TypeLits.symbolVal (Proxy @key)))
    pure $ AtKey innerVal

pagedOn :: (a -> cursor) -> [a] -> Paged cursor a
pagedOn f items =
  let prevCursor = do
        first <- headMay items
        pure $ Cursor (f first) Previous
      nextCursor = do
        last <- lastMay items
        pure $ Cursor (f last) Next
   in Paged items prevCursor nextCursor
