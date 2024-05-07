{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Share.Release where

import Control.Exception
import Control.Lens
import Data.Time (UTCTime)
import Share.IDs
import Share.Postgres qualified as PG
import Share.Prelude

data ReleaseStatus userId
  = PublishedRelease UTCTime (Maybe userId)
  | DeprecatedRelease UTCTime (Maybe userId)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance PG.DecodeValue userId => PG.DecodeRow (ReleaseStatus userId) where
  decodeRow = do
    publishedAt <- PG.decodeField
    publishedBy <- PG.decodeField
    mdeprecatedAt <- PG.decodeField
    deprecatedBy <- PG.decodeField
    case mdeprecatedAt of
      Nothing -> pure (PublishedRelease publishedAt publishedBy)
      Just deprecatedAt -> pure (DeprecatedRelease deprecatedAt deprecatedBy)

data InvalidReleaseStatus = InvalidReleaseStatus Text
  deriving stock (Show, Eq, Ord)
  deriving anyclass (Exception)

data Release causal userId = Release
  { releaseId :: ReleaseId,
    projectId :: ProjectId,
    version :: ReleaseVersion,
    status :: ReleaseStatus userId,
    unsquashedCausal :: causal,
    squashedCausal :: causal,
    createdAt :: UTCTime,
    createdBy :: userId,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Ord)

releaseCausals_ :: Traversal (Release causal userId) (Release causal' userId) causal causal'
releaseCausals_ f Release {..} = do
  unsquashedCausal <- f unsquashedCausal
  squashedCausal <- f squashedCausal
  pure Release {unsquashedCausal, squashedCausal, ..}

releaseUsers_ :: Traversal (Release causal userId) (Release causal userId') userId userId'
releaseUsers_ f Release {..} = do
  createdBy' <- f createdBy
  status' <- traverse f status
  pure Release {createdBy = createdBy', status = status', ..}

instance (PG.DecodeValue causal, PG.DecodeValue userId) => PG.DecodeRow (Release causal userId) where
  decodeRow = do
    releaseId <- PG.decodeField
    projectId <- PG.decodeField
    unsquashedCausal <- PG.decodeField
    squashedCausal <- PG.decodeField
    createdAt <- PG.decodeField
    updatedAt <- PG.decodeField
    status <- PG.decodeRow
    createdBy <- PG.decodeField
    version <- PG.decodeRow
    pure Release {..}
