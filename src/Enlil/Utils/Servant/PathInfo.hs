{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides a mechanism to get information about a given request given the full API.
--    It can be used to normalize routes for reporting in metrics.
module Enlil.Utils.Servant.PathInfo (normalizePath, PathSegment, CaptureSegment, PathInfo, HasPathInfo (..)) where

import Control.Applicative
import Data.Text
import Data.Text qualified as Text
import GHC.TypeLits
import Servant

symbolText :: forall s. KnownSymbol s => Text
symbolText = Text.pack $ symbolVal (Proxy @s)

type PathSegment = Text

data CaptureSegment = CaptureSegment {captureName :: Text, capturedValue :: Text}
  deriving (Show)

type PathInfo = [Either CaptureSegment PathSegment]

-- | Normalizes path segments from an api so that they can be used in metrics.
--
-- >>> parsePathInfo (Proxy @(("books" :> ()) :<|> ("book" :> Capture "something" Text))) ["books"]
-- Just ([Right "books"],[])
--
-- >>> parsePathInfo (Proxy @(("books" :> ()) :<|> ("book" :> Capture "something" Text))) ["book", "bookID"]
-- Just ([Right "book",Left (CaptureSegment {captureName = "something", capturedValue = "bookID"})],[])
class HasPathInfo (api :: k) where
  -- Tries to match segments against the current API type, returns any matched path info and
  -- any remaining segments.
  parsePathInfo :: Proxy api -> [Text] -> Maybe (PathInfo, [PathSegment])

instance {-# OVERLAPPING #-} KnownSymbol piece => HasPathInfo (piece :: Symbol) where
  parsePathInfo _api (seg : rest)
    | seg == symbolText @piece = Just ([Right seg], rest)
  parsePathInfo _api _ = Nothing

instance {-# OVERLAPPING #-} KnownSymbol name => HasPathInfo (Capture name x) where
  parsePathInfo _api (seg : rest) = Just ([Left (CaptureSegment {captureName = symbolText @name, capturedValue = seg})], rest)
  parsePathInfo _api _ = Nothing

instance {-# OVERLAPPING #-} (HasPathInfo a, HasPathInfo b) => HasPathInfo (a :<|> b) where
  parsePathInfo _ segs = parsePathInfo (Proxy @a) segs <|> parsePathInfo (Proxy @b) segs

instance {-# OVERLAPPING #-} (HasPathInfo a, HasPathInfo b) => HasPathInfo (a :> b) where
  parsePathInfo _ segs = do
    (parsedA, restA) <- (parsePathInfo (Proxy @a) segs)
    (parsedB, restB) <- (parsePathInfo (Proxy @b) restA)
    pure (parsedA <> parsedB, restB)

-- | Any other type just gets a catch-all which doesn't fail, but doesn't consume any path segments.
instance {-# OVERLAPPABLE #-} HasPathInfo unknown where
  parsePathInfo _api segs = Just ([], segs)

-- | Normalizes path segments from an api so that they can be used in metrics.
--
-- >>> normalizePath (Proxy @(("books" :> ()) :<|> ("book" :> Capture "something" Text))) ["books"]
-- Just ["books"]
--
-- >>> normalizePath (Proxy @(("books" :> ()) :<|> ("book" :> Capture "book_id" Text))) ["book", "bookID"]
-- Just ["book","<book_id>"]
normalizePath :: HasPathInfo api => Proxy api -> [PathSegment] -> Maybe [PathSegment]
normalizePath api segs =
  case parsePathInfo api segs of
    Just (pathInfo, []) -> Just $ fmap (either (\c -> "<" <> captureName c <> ">") id) pathInfo
    -- If the parse failed, or if there are trailing segments we didn't expect, then fail.
    _ -> Nothing
