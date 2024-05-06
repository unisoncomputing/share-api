{-# LANGUAGE DeriveAnyClass #-}

module Share.Utils.URI
  ( URIParam (..),
    addQueryParam,
    setPathAndQueryParams,
    uriToString,
    uriToText,
  )
where

import Control.Exception
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Binary (Binary (..))
import Data.ByteString.Char8 qualified as BSC
import Data.Function ((&))
import Data.Functor.Contravariant (contramap)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Share.Utils.Show (tShow)
import Hasql.Decoders qualified as Decoders
import Hasql.Interpolate qualified as Hasql
import Network.HTTP.Types (parseQuery, renderQuery)
import Network.URI qualified as URI
import Servant

-- | Helper type to provide additional instances for URIs.
newtype URIParam = URIParam {unpackURI :: URI}
  deriving newtype (Show, Eq, Ord)

newtype BadURI = BadURI String
  deriving stock (Show)
  deriving anyclass (Exception)

instance Hasql.DecodeValue URIParam where
  decodeValue =
    do
      Hasql.decodeValue
      & Decoders.refine \txt ->
        case (URI.parseURI (Text.unpack txt)) of
          Nothing -> Left $ "Bad URI: " <> txt
          Just uri -> pure $ URIParam uri

instance Hasql.EncodeValue URIParam where
  encodeValue =
    Hasql.encodeValue
      & contramap tShow

instance ToJSON URIParam where
  toJSON (URIParam uri) = Aeson.String . Text.pack $ show uri

instance FromJSON URIParam where
  parseJSON =
    Aeson.withText "URIParam" $ \txt ->
      maybe (fail "Invalid URI") (pure . URIParam) $ URI.parseURI (Text.unpack txt)

instance Binary URIParam where
  get = do
    uriStr <- get @String
    maybe (fail "Invalid URI") (pure . URIParam) $ URI.parseURI uriStr
  put (URIParam uri) = put (show uri)

instance ToHttpApiData URIParam where
  toQueryParam (URIParam uri) =
    -- Servant handles uri escaping for us.
    Text.pack (show uri)

instance FromHttpApiData URIParam where
  parseQueryParam uriText =
    case URI.parseURI (Text.unpack uriText) of
      Nothing -> Left $ "Invalid URI: " <> uriText
      Just uri -> Right (URIParam uri)

-- | Helper to add a query param to a URI
addQueryParam :: (ToHttpApiData v) => Text -> v -> URI -> URI
addQueryParam key val uri =
  let existingQuery = parseQuery $ BSC.pack (URI.uriQuery uri)
      newParam = (Text.encodeUtf8 key, Just . Text.encodeUtf8 . toQueryParam $ val)
   in uri {URI.uriQuery = BSC.unpack $ renderQuery True (existingQuery <> [newParam])}

setPathAndQueryParams :: [Text] -> Map Text Text -> URI -> URI
setPathAndQueryParams pathSegments queryParams uri =
  uri
    { uriPath = Text.unpack path
    }
    & addAllQueryParams
  where
    addAllQueryParams baseURI = List.foldl' (\uri (k, v) -> addQueryParam k v uri) baseURI (Map.toList queryParams)
    path = case pathSegments of
      [] -> ""
      xs -> "/" <> Text.intercalate "/" xs

-- | It's sketchy to use 'show' on URIs in the app because if the type of that thing changes the 'show'
-- will still happily succeed. Better to use something explicitly typed.
uriToString :: URI -> String
uriToString = show

-- | It's sketchy to use 'show' on URIs in the app because if the type of that thing changes the 'show'
-- will still happily succeed. Better to use something explicitly typed.
uriToText :: URI -> Text
uriToText = Text.pack . uriToString
