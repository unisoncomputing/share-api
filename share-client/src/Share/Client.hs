module Share.Client () where

import Servant.Client
import Share.Web.API (API, api)

_x :: Client ClientM OrgAPI
_x = client api
