module Share.Web.UCM.SyncV3.Queries
  ( fetchSerialisedEntities,
  )
where

import Data.Vector (Vector)
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Postgres
import Share.Prelude
import Unison.SyncV3.Types
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Hash32 (Hash32)
import Unison.SyncV2.Types (CBORBytes)

fetchSerialisedEntities :: (QueryM m) => CodebaseEnv -> Set (EntityKind, Hash32) -> m (Vector (Entity Hash32 Text))
fetchSerialisedEntities (CodebaseEnv {codebaseOwner}) requestedEntities =
  do
    queryVectorRows @(EntityKind, CBORBytes TempEntity, Hash32, EntityDepth)
      [sql|
          WITH requested(kind, hash) AS (
            SELECT kind, hash FROM ^{toTable $ toList requestedEntities} AS t(kind, hash)
          )
           (SELECT req.kind, bytes.bytes, ch.base32, cd.depth
             FROM requested req
               JOIN component_hashes ch ON req.hash = ch.base32
               JOIN serialized_components sc ON sc.user_id = #{codebaseOwner} AND ch.id = sc.component_hash_id
               JOIN bytes ON sc.bytes_id = bytes.id
               JOIN component_depth cd ON ch.id = cd.component_hash_id
               WHERE req.kind = 'component'
           )
           UNION ALL
           (SELECT req.kind, bytes.bytes, req.hash, pd.depth
             FROM requested req
               JOIN patches p ON req.hash = p.hash
               JOIN serialized_patches sp ON p.id = sp.patch_id
               JOIN bytes ON sp.bytes_id = bytes.id
               JOIN patch_depth pd ON p.id = pd.patch_id
               WHERE req.kind = 'patch'
           )
           UNION ALL
           (SELECT req.kind, bytes.bytes, req.hash, nd.depth
             FROM requested req
               JOIN branch_hashes bh ON req.hash = bh.base32
               JOIN serialized_namespaces sn ON bh.id = sn.namespace_hash_id
               JOIN bytes ON sn.bytes_id = bytes.id
               JOIN namespace_depth nd ON bh.id = nd.namespace_hash_id
               WHERE req.kind = 'namespace'
           )
           UNION ALL
           -- TODO: Should probably join in a batch of causal spines here too
           -- to improve parallelism and avoid long-spine bottlenecks.
           (SELECT req.kind, bytes.bytes, req.hash, cd.depth
             FROM requested req
               JOIN causals c ON req.hash = c.hash
               JOIN serialized_causals sc ON c.id = sc.causal_id
               JOIN bytes ON sc.bytes_id = bytes.id
               JOIN causal_depth cd ON c.id = cd.causal_id
           )
  |]
      <&> fmap (\(entityKind, entityData, entityHash, entityDepth) -> Entity {entityKind, entityData, entityHash, entityDepth})
