module Enlil.OAuth.Scopes
  ( Scopes (..),
    UnisonScope (..),
    parseScopes,
    containsScope,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.Functor.Contravariant (contramap)
import Data.Set
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hasql.Interpolate qualified as Hasql
import Servant
import Witherable (Filterable (mapMaybe))

-- | https://datatracker.ietf.org/doc/html/rfc6749#section-3.3
newtype Scopes = Scopes (Set UnisonScope)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Show Scopes where
  show (Scopes scopes) = Text.unpack . Text.unwords . fmap renderScope . Set.toList $ scopes

parseScopes :: Text -> Scopes
parseScopes txt =
  txt
    & Text.words
    & mapMaybe parseScope -- Drop unknown scopes
    & Set.fromList
    & Scopes

instance FromHttpApiData Scopes where
  parseQueryParam txt = pure (parseScopes txt)

instance ToHttpApiData Scopes where
  toQueryParam (Scopes scopes) =
    Text.unwords . fmap renderScope . Set.toList $ scopes

instance ToJSON Scopes where
  toJSON (Scopes scopes) =
    scopes
      & Set.toList
      & fmap renderScope
      & Text.unwords
      & Aeson.String

instance FromJSON Scopes where
  parseJSON =
    Aeson.withText "Scopes" $ pure . parseScopes

instance Hasql.EncodeValue Scopes where
  encodeValue =
    Hasql.encodeValue @Text
      & contramap (toQueryParam @Scopes)

instance Hasql.DecodeValue Scopes where
  decodeValue =
    Hasql.decodeValue @Text
      & fmap parseScopes

-- | Type representing scopes that we support
data UnisonScope
  = OpenId
  | Cloud -- Allows interacting with unison cloud to run code.
  | Share -- Allows browsing unison share
  | Sync -- Allows pushing/pulling code
  deriving (Eq, Ord)

containsScope :: UnisonScope -> Scopes -> Bool
containsScope s (Scopes scopes) = Set.member s scopes

parseScope :: Text -> Maybe UnisonScope
parseScope = \case
  "openid" -> Just OpenId
  "cloud" -> Just Cloud
  "share" -> Just Share
  "sync" -> Just Sync
  _ -> Nothing

renderScope :: UnisonScope -> Text
renderScope = \case
  OpenId -> "openid"
  Cloud -> "cloud"
  Share -> "share"
  Sync -> "sync"

instance Show UnisonScope where
  show = Text.unpack . renderScope
