{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Vault
  ( deleteSecret,
    patchSecret,
    storeSecret,
    fetchSecret,
    SecretMount (..),
    VaultToken (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media.MediaType qualified as MT
import Servant
import Servant.Client qualified as SC
import Share.Utils.Show (Censored (..))

type RequiredHeader = Header' '[Required, Strict]

-- API definition
type VaultAPI =
  (RequiredHeader "X-Vault-Token" VaultToken :> Capture "mount" SecretMount :> "data" :> Capture "path" SecretPath :> DeleteNoContent)
    :<|> (RequiredHeader "X-Vault-Token" VaultToken :> Capture "mount" SecretMount :> "data" :> Capture "path" SecretPath :> ReqBody '[JsonMergePatch] SecretPatch :> PatchNoContent)
    :<|> (RequiredHeader "X-Vault-Token" VaultToken :> Capture "mount" SecretMount :> "data" :> Capture "path" SecretPath :> ReqBody '[JSON] SecretRequest :> PostNoContent)
    :<|> (RequiredHeader "X-Vault-Token" VaultToken :> Capture "mount" SecretMount :> "data" :> Capture "path" SecretPath :> Get '[JSON] SecretResponse)

vaultApi :: Proxy VaultAPI
vaultApi = Proxy

data JsonMergePatch

instance Accept JsonMergePatch where
  contentType _ = "application" MT.// "merge-patch+json"

newtype SecretMount = SecretMount Text
  deriving newtype (Show, Eq, Ord, FromHttpApiData, ToHttpApiData)

newtype SecretPath = SecretPath Text
  deriving newtype (Show, Eq, Ord, FromHttpApiData, ToHttpApiData)

newtype VaultToken = VaultToken Text
  deriving newtype (Eq, Ord, FromHttpApiData, ToHttpApiData)
  deriving (Show) via Censored VaultToken

deleteSecret :: VaultToken -> SecretMount -> SecretPath -> SC.ClientM NoContent
patchSecret :: VaultToken -> SecretMount -> SecretPath -> SecretPatch -> SC.ClientM NoContent
storeSecret :: VaultToken -> SecretMount -> SecretPath -> SecretRequest -> SC.ClientM NoContent
fetchSecret :: VaultToken -> SecretMount -> SecretPath -> SC.ClientM SecretResponse
deleteSecret :<|> patchSecret :<|> storeSecret :<|> fetchSecret = SC.client vaultApi

-- Data types

data SecretRequest = SecretRequest
  { options :: Maybe Options,
    data_ :: Map String String
  }
  deriving (Generic)

instance Aeson.ToJSON SecretRequest where
  toJSON (SecretRequest options data_) = Aeson.object ["options" Aeson..= options, "data" Aeson..= data_]

newtype Options = Options
  { cas :: Int
  }
  deriving (Generic)

instance Aeson.ToJSON Options

newtype SecretResponse = SecretResponse SecretData

newtype SecretData = SecretData (Map String String)

data SecretPatch = SecretPatch !(Maybe Options) !(Map String (Maybe String))

instance MimeRender JsonMergePatch SecretPatch where
  mimeRender _ (SecretPatch options data_) = Aeson.encode $ Aeson.object ["options" Aeson..= options, "data" Aeson..= data_]

instance Aeson.FromJSON SecretResponse where
  parseJSON (Aeson.Object v) = SecretResponse <$> v Aeson..: "data"
  parseJSON _ = undefined

instance Aeson.FromJSON SecretData where
  parseJSON (Aeson.Object v) = SecretData <$> v Aeson..: "data"
  parseJSON _ = undefined

instance Aeson.ToJSON SecretData where
  toJSON (SecretData data_) = Aeson.object ["data" Aeson..= data_]
