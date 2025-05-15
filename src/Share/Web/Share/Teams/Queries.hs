module Share.Web.Share.Teams.Queries (teamDisplayInfoOf) where

import Control.Lens
import Share.IDs
import Share.Postgres
import Share.Prelude
import Share.Utils.URI (URIParam (..))
import Share.Web.Share.DisplayInfo.Types (TeamDisplayInfo (..))

-- | Efficiently resolve Team Display Info for TeamIds within a structure.
teamDisplayInfoOf :: (QueryA m) => Traversal s t TeamId TeamDisplayInfo -> s -> m t
teamDisplayInfoOf trav s = do
  s
    & unsafePartsOf trav %%~ \teamIds ->
      do
        let teamTable = zip [0 :: Int32 ..] teamIds
        queryListRows
          [sql|
      WITH values(ord, team_id) AS (
        SELECT * FROM ^{toTable teamTable} AS t(ord, team_id)
      ) SELECT t.id, t.name, t.avatar_url
          FROM values
          JOIN teams t ON t.id = values.team_id
        ORDER BY ord
        |]
          <&> fmap \(teamId, name, avatarUrl) ->
            TeamDisplayInfo
              { teamId,
                name,
                avatarUrl = unpackURI <$> avatarUrl
              }
