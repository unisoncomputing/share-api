{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Share.Utils.Servant
  ( LocationHeader,
    StatusFound302,
    GetRedirect,
    RequiredQueryParam,
    RequiredHeader,
    InvalidParam (..),
    redirectTo,
    withTimeoutSeconds,
    parseParam,
    OptionalCapture,
  )
where

import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import Share.Prelude
import Share.Utils.Logging
import Share.Web.Errors (ErrorID (..), InvalidParam (..), ToServerError (..), respondError)
import GHC.TypeLits (KnownSymbol, Nat, Symbol)
import Servant
import UnliftIO qualified

type LocationHeader = Headers '[Header "Location" String] NoContent

type GetRedirect (code :: Nat) = Verb 'GET code '[JSON] LocationHeader

type StatusFound302 = 302

type RequiredQueryParam = QueryParam' '[Required, Strict]
type RequiredHeader = Header' '[Required, Strict]

redirectTo :: URI -> LocationHeader
redirectTo uri = addHeader (show uri) NoContent

data Timeout = Timeout NominalDiffTime

instance Loggable Timeout where
  toLog (Timeout n) =
    withSeverity Error . textLog $ "Request Timeout, took longer than " <> Text.pack (show n)

instance ToServerError Timeout where
  toServerError Timeout {} =
    (ErrorID "timeout", err504 {errReasonPhrase = "Request Timeout.", errBody = "Request Timeout."})

withTimeoutSeconds :: NominalDiffTime -> WebApp a -> WebApp a
withTimeoutSeconds diffTime m = do
  let (seconds, _fractional) = properFraction diffTime
  UnliftIO.timeout (seconds * 1000_000) m >>= \case
    Nothing -> respondError (Timeout diffTime)
    Just a -> pure a

-- | Parse an api parameter, throwing a 400 if it fails.
--
-- Ideally we'd just have servant do all of these in the API layer,
-- but for the UCM APIs the APIs are defined in a separate package, using different domain
-- objects, so we need to 're-parse' from text within the handler.
parseParam :: forall a. (FromHttpApiData a) => Text -> Text -> WebApp a
parseParam paramName param = do
  case parseQueryParam param of
    Left parseError -> respondError (InvalidParam {paramName, param, parseError})
    Right a -> pure a

-- | Like 'Capture', but the capture is optional.
-- 'OptionalCapture sym a' produces a value of type 'Maybe a' instead of just 'a' like
-- 'Capture' does.
--
-- Note that this combinator is only sensible as the final path segment of a route.
--
-- E.g. "path" :> OptionalCapture "my-capture" Int :> Get '[JSON] ()
--
-- Would match both "/path/" (with 'Nothing' as the capture) and "/path/123" with 'Just 123'
-- as the capture.
--
-- The type of the handler would be 'Maybe Int -> WebApp ()'
data OptionalCapture (sym :: Symbol) a

instance (HasServer api ctx, KnownSymbol sym, FromHttpApiData a, HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters, Typeable a) => HasServer (OptionalCapture sym a :> api) ctx where
  type ServerT (OptionalCapture sym a :> api) m = Maybe a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ ctx d = route (Proxy :: Proxy ((Capture sym a :> api) :<|> api)) ctx $ fmap f d
    where
      f may = may . Just :<|> may Nothing
