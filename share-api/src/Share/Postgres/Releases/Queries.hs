{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Releases.Queries
  ( expectReleaseVersionsOf,
    latestReleaseByProjectId,
    releaseById,
    UpdateReleaseResult (..),
    updateRelease,
  )
where

import Control.Lens
import Share.IDs
import Share.Postgres
import Share.Postgres.IDs
import Share.Prelude
import Share.Release
import Share.Web.Share.Releases.Types (StatusUpdate (..))

expectReleaseVersionsOf :: Traversal s t ReleaseId ReleaseVersion -> s -> Transaction e t
expectReleaseVersionsOf trav s = do
  s
    & asListOf trav %%~ \releaseIds -> do
      let numberedReleaseIds = zip [1 :: Int32 ..] releaseIds
      results :: [ReleaseVersion] <-
        queryListRows @ReleaseVersion
          [sql|
      WITH release_ids(ord, id) AS (
        SELECT * FROM ^{toTable numberedReleaseIds}
      )
      SELECT r.major_version, r.minor_version, r.patch_version
        FROM release_ids JOIN project_releases r ON release_ids.id = r.id
        ORDER BY release_ids.ord ASC
      |]
      if length results /= length releaseIds
        then error "expectReleaseVersionsOf: Missing expected release version"
        else pure results

latestReleaseByProjectId :: ProjectId -> Transaction e (Maybe (Release CausalId UserId))
latestReleaseByProjectId projectId = do
  query1Row @(Release CausalId UserId)
    [sql|
        SELECT
          release.id,
          release.project_id,
          release.unsquashed_causal_id,
          release.squashed_causal_id,
          release.created_at,
          release.updated_at,
          release.created_at,
          release.created_by,
          release.deprecated_at,
          release.deprecated_by,
          release.created_by,
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
        WHERE release.project_id = #{projectId}
              AND release.deleted_at IS NULL
        ORDER BY release.major_version DESC, release.minor_version DESC, release.patch_version DESC
        LIMIT 1
      |]

releaseById :: (QueryA m) => ReleaseId -> m (Release CausalId UserId)
releaseById releaseId = do
  queryExpect1Row
    [sql|
        SELECT
          release.id,
          release.project_id,
          release.unsquashed_causal_id,
          release.squashed_causal_id,
          release.created_at,
          release.updated_at,
          release.created_at,
          release.created_by,
          release.deprecated_at,
          release.deprecated_by,
          release.created_by,
          release.major_version,
          release.minor_version,
          release.patch_version
        FROM project_releases AS release
        WHERE release.id = #{releaseId}
      |]

data UpdateReleaseResult
  = UpdateRelease'Success
  | UpdateRelease'NotFound
  | UpdateRelease'CantPublishDeprecated

updateRelease :: (QueryM m) => UserId -> ReleaseId -> Maybe StatusUpdate -> m UpdateReleaseResult
updateRelease caller releaseId newStatus = do
  fromMaybe UpdateRelease'NotFound <$> runMaybeT do
    Release {..} <- lift $ releaseById releaseId
    -- Can go from draft -> published -> deprecated
    -- or straight from draft -> deprecated
    -- but can't go from published -> draft or deprecated -> draft.
    case (status, newStatus) of
      (_, Nothing) ->
        -- No-op
        pure UpdateRelease'Success
      (DeprecatedRelease {}, Just MakePublished) -> do
        pure UpdateRelease'CantPublishDeprecated
      (PublishedRelease {}, Just MakePublished) ->
        -- No-op
        pure UpdateRelease'Success
      (PublishedRelease {}, Just MakeDeprecated) -> do
        lift makeDeprecated
        pure UpdateRelease'Success
      (DeprecatedRelease {}, Just MakeDeprecated) -> do
        -- No-op
        pure UpdateRelease'Success
  where
    makeDeprecated = do
      execute_
        [sql|
          UPDATE project_releases
          SET
            deprecated_at = NOW(),
            deprecated_by = #{caller}
          WHERE
            id = #{releaseId}
        |]
