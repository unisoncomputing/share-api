{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Share.Utils.ServantUtils
  ( AddHeader',
    module Cookies,
  )
where

import Servant
import Share.Utils.Servant.Cookies qualified as Cookies

type AddHeader' = AddHeader '[Optional, Strict]
