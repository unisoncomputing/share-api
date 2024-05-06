module Share.Postgres.Sync.Types (TypedTempEntity (..)) where

import Share.Postgres (decodeField)
import Share.Postgres qualified as PG
import Share.Postgres.Serialization qualified as S
import Hasql.Interpolate qualified as Hasql
import U.Codebase.Sqlite.TempEntity (TempEntity)

-- | Helper for deserializing typed temp entities.
-- See the attached DecodeRow instance.
newtype TypedTempEntity = TypedTempEntity {unTypedTempEntity :: TempEntity}

-- | Decodes (entityType, entityBytes) into a temp entity.
instance Hasql.DecodeRow TypedTempEntity where
  decodeRow = do
    entityType <- decodeField
    PG.RawBytes entityBytes <- decodeField
    case S.decodeTypedTempEntity entityType entityBytes of
      Left err -> fail (show err)
      Right tempEntity -> pure (TypedTempEntity tempEntity)
