module Share.Web.Share.Orgs.Types (Org (..)) where

import Share.IDs
import Share.Postgres (DecodeRow (..), decodeField)

newtype Org = Org {orgId :: OrgId}
  deriving (Show, Eq)

instance DecodeRow Org where
  decodeRow = Org <$> decodeField
