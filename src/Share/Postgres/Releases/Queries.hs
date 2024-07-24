{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Releases.Queries
  ( expectReleaseVersionsOf,
  )
where

import Control.Lens
import Share.IDs
import Share.Postgres
import Share.Prelude

expectReleaseVersionsOf :: Traversal s t ReleaseId ReleaseVersion -> s -> Transaction e t
expectReleaseVersionsOf trav s = do
  s
    & unsafePartsOf trav %%~ \releaseIds -> do
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
