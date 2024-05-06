{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Share.Utils.Servant.Cookies
  ( Cookie,
    cookieVal,
    CookieVal,
    Cookies,
    CookieMap (..),
    CookieSettings (..),
    defaultCookieSettings,
    Servant.IsSecure (..),
    Cookie.SetCookie,
    newSetCookie,
    clearCookie,
  )
where

import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (DiffTime)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
import Web.Cookie qualified as Cookie

-- | Allows deserializing a single cookie in a servant route.
--
-- E.g.
-- @@
--  "my-route"
--  :> Cookie "color-scheme" ColorScheme
--  :> ...
-- @@
--
-- The handler will receive a value of type @Maybe (CookieVal "color-scheme" ColorScheme)@
-- The redundant Maybe wrapper is unfortunate, but can be mostly ignored by using
-- 'cookieVal' as a view pattern when binding.
type Cookie (s :: Symbol) a = Header "Cookie" (CookieVal s a)

-- | The type used to deserialize individual cookie values.
newtype CookieVal (s :: Symbol) a = CookieVal {getCookieVal :: Maybe a}

-- | Elegantly collapses the duplicate Maybe wrapping on a cookie val, use in a view
-- pattern when binding:
--
-- @@
-- useCookieVal :: CookieVal "color-scheme" ColorScheme
-- useCookieVal (cookieVal -> Just ColorScheme) = ...
-- useCookieVal _ = ...
-- @@
cookieVal :: Maybe (CookieVal s a) -> Maybe a
cookieVal mayCV = mayCV >>= getCookieVal

instance (KnownSymbol s, FromHttpApiData a) => FromHttpApiData (CookieVal s a) where
  parseQueryParam _ = error "CookieVal used outside of Header field"
  parseHeader bs =
    parseHeader bs
      & ( \case
            Left txt -> Left txt
            Right (CookieMap m) ->
              case Map.lookup (Text.pack $ symbolVal (Proxy @s)) m of
                Nothing -> Right (CookieVal Nothing)
                Just valTxt -> CookieVal . Just <$> parseQueryParam valTxt
        )

type Cookies = Header "Cookie" CookieMap

-- | This type is used by 'Cookies' and 'CookieVal' as a way to deserialize the Cookie header
-- into a map to be used by a route.
newtype CookieMap = CookieMap {cookieMap :: Map Text Text}

instance FromHttpApiData CookieMap where
  parseQueryParam _ = error "CookieMap used outside of Header field"
  parseHeader bs = Right . CookieMap . Map.fromList $ Cookie.parseCookiesText bs

-- | The @SameSite@ attribute of cookies determines whether cookies will be sent
-- on cross-origin requests.
--
-- See <https://tools.ietf.org/html/draft-west-first-party-cookies-07 this document>
-- for more information.
data SameSite = AnySite | SameSiteStrict | SameSiteLax
  deriving (Eq, Show, Ord)

-- | Settings for configuring cookies for your web app.
data CookieSettings = CookieSettings
  { -- | 'Secure' means browsers will only send cookies over HTTPS. Default:
    -- @Secure@.
    cookieIsSecure :: !IsSecure,
    -- | 'True' means browsers will only send cookies over HTTP(S) and the cookies will
    -- NOT be accessible to JavaScript.
    cookieHTTPOnly :: !Bool,
    -- | How long from now until the cookie expires. Default: @Nothing@.
    cookieMaxAge :: !(Maybe DiffTime),
    -- | The URL path and sub-paths for which this cookie is used. Default: @Just "/"@.
    cookiePath :: !(Maybe BS.ByteString),
    -- | Domain name, if set, cookie will be shared with subdomains.
    -- Recommended to leave as @Nothing@ unless you need this behavior.
    -- Default: @Nothing@.
    cookieDomain :: !(Maybe BS.ByteString),
    -- | 'SameSite' settings. Default: @SameSiteLax@.
    cookieSameSite :: !SameSite
  }
  deriving (Eq, Show)

defaultCookieSettings ::
  -- | Whether we're currently running on localhost in local development
  Bool ->
  -- | Default cookie lifetime. If 'Nothing', cookies will expire at the end of the session.
  Maybe DiffTime ->
  CookieSettings
defaultCookieSettings onLocal cookieTTL =
  CookieSettings
    { cookieIsSecure = if onLocal then NotSecure else Secure,
      cookieHTTPOnly = True,
      cookieMaxAge = cookieTTL,
      cookieSameSite = SameSiteLax,
      cookiePath = Just "/",
      cookieDomain = Nothing
    }

-- | Create a 'SetCookie' from a 'CookieSettings' and a name/value pair.
-- You can then override the default settings with the 'setCookie*' functions.
newSetCookie :: CookieSettings -> Text -> Text -> Cookie.SetCookie
newSetCookie CookieSettings {cookieIsSecure, cookieHTTPOnly, cookieMaxAge, cookiePath, cookieDomain, cookieSameSite} name val =
  Cookie.defaultSetCookie
    { Cookie.setCookieSecure = cookieIsSecure == Secure,
      Cookie.setCookieMaxAge = cookieMaxAge,
      Cookie.setCookiePath = cookiePath,
      Cookie.setCookieDomain = cookieDomain,
      Cookie.setCookieHttpOnly = cookieHTTPOnly,
      Cookie.setCookieSameSite = case cookieSameSite of
        AnySite -> Nothing
        SameSiteStrict -> Just Cookie.sameSiteStrict
        SameSiteLax -> Just Cookie.sameSiteLax,
      Cookie.setCookieName = Text.encodeUtf8 name,
      Cookie.setCookieValue = Text.encodeUtf8 val
    }

clearCookie :: CookieSettings -> Text -> Cookie.SetCookie
clearCookie cookieSettings name =
  (newSetCookie cookieSettings name "")
    { -- expire the cookie immediately. Apparently '0' isn't valid for all browsers.
      Cookie.setCookieMaxAge = Just 1
    }
