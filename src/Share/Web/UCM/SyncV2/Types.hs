module Share.Web.UCM.SyncV2.Types
  ( IsLibRoot (..),
    IsCausalSpine (..),
  )
where

data IsLibRoot
  = IsLibRoot
  | NotLibRoot

data IsCausalSpine
  = IsCausalSpine
  | NotCausalSpine
