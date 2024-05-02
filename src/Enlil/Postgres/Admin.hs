-- | Queries used for admin tasks. Don't use these in normal endpoints.
module Enlil.Postgres.Admin
  ( hardDeleteUser,
  )
where

import Enlil.IDs (UserId)
import Enlil.Postgres qualified as PG

-- | Delete a user COMPLETELY. This is unreversable.
hardDeleteUser :: UserId -> PG.Transaction e ()
hardDeleteUser userId =
  PG.execute_
    [PG.sql|
    DELETE FROM users WHERE id = #{userId}
  |]
